/**
 *  @file
 *  @copyright defined in fibos/LICENSE.txt
 */

#include <eosio.token/eosio.token.hpp>

namespace eosio {

static constexpr time uniswap_unlock_delay = 3 * 24 * 3600;

enum order_state {
    WITHDRAW = 1,
    COMPLETE = 2,
    INVERSE = 3,
};

void token::addreserves(account_name owner, extended_asset x, extended_asset y)
{
    require_auth(owner);

    eosio_assert(x.is_valid() && y.is_valid(), "invalid currency");
    eosio_assert(x.amount > 0 && y.amount > 0, "must addreserves positive amount");
    eosio_assert(x.get_extended_symbol() != y.get_extended_symbol(), "can't create market with same token");

    if (x.get_extended_symbol() < y.get_extended_symbol()) {
        auto swap = x;
        x = y;
        y = swap;
    }
    auto sym_x = x.get_extended_symbol();
    auto sym_y = y.get_extended_symbol();

    eosio_assert(!check_bancor_pair(sym_x, sym_y), "Must not be bancor pair");

    sub_balance(owner, x);
    sub_balance(owner, y);

    swap_market market(_self, _self);
    auto m_index = market.get_index<N(bysymbol)>();
    auto m_iter = m_index.find(uniswap_market::key(sym_x, sym_y));

    double new_weights = static_weights;
    uint64_t primary;
    if (m_iter == m_index.end()) {
        primary = market.available_primary_key();
        market.emplace(owner, [&](auto& r) {
            r.primary = primary;
            r.tokenx = x;
            r.tokeny = y;
            r.total_weights = new_weights;
        });
    } else {
        primary = m_iter->primary;

        extended_asset new_x = m_iter->tokenx + x;
        extended_asset new_y = m_iter->tokeny + y;
        double real_old_x = get_real_asset(m_iter->tokenx);
        double real_old_y = get_real_asset(m_iter->tokeny);
        double real_new_x = get_real_asset(new_x);
        double real_new_y = get_real_asset(new_y);
        uint64_t price = real_old_x / real_old_y * std::pow(2, 32);
        uint64_t new_price = real_new_x / real_new_y * std::pow(2, 32);
        double div = (double)new_price / (double)price;
        eosio_assert(div <= 1.01 && div >= 0.99, "Excessive price volatility");

        SEND_INLINE_ACTION(*this, pricerec, { _self, N(active) }, { price, new_price });
        double total = std::sqrt(real_old_x) * std::sqrt(real_old_y);
        double new_total = std::sqrt(real_new_x) * std::sqrt(real_new_y);

        new_weights = (new_total - total) / total * m_iter->total_weights;
        eosio_assert(new_weights > 0, "Invalid new weights");
        eosio_assert(new_weights / new_weights > 0, "Invalid new weights");
        auto total_weights = m_iter->total_weights + new_weights;
        eosio_assert(new_weights / total_weights > 0.0001, "Add reserves too lower");
        m_index.modify(m_iter, 0, [&](auto& s) {
            s.tokenx = new_x;
            s.tokeny = new_y;
            s.total_weights = total_weights;
        });

        auto order_primary = primary;
        auto order_price = real_new_y / real_new_x * std::pow(2, 32);
        if (div > 1) {
            get_order_prefix(order_primary);
            order_price = new_price;
        }
        deal_inverse_order(order_price, order_primary, owner);
    }

    swap_pool pool(_self, primary);
    auto pool_iter = pool.find(owner);
    if (pool_iter != pool.end()) {
        pool.modify(pool_iter, 0, [&](auto& s) {
            s.weights += new_weights;
        });
    } else {
        pool.emplace(owner, [&](auto& s) {
            s.owner = owner;
            s.weights = new_weights;
        });
    }
}

void token::outreserves(account_name owner, extended_symbol x, extended_symbol y, double rate)
{
    require_auth(owner);

    if (x < y) {
        auto swap = x;
        x = y;
        y = swap;
    }

    eosio_assert(x.is_valid() && y.is_valid(), "invalid symbol name");
    eosio_assert(rate > 0 && rate <= 1, "invaild rate");

    swap_market market(_self, _self);
    auto m_index = market.get_index<N(bysymbol)>();
    auto m_iter = m_index.find(uniswap_market::key(x, y));
    eosio_assert(m_iter != m_index.end(), "no matched market record");

    auto primary = m_iter->primary;
    swap_lock lock_tbl(_self, primary);
    auto req = lock_tbl.find(owner);
    if (req != lock_tbl.end()) {
        eosio_assert(req->request_time != 0 && req->request_time + uniswap_unlock_delay <= now(), "unlock is not available yet");
        lock_tbl.erase(req);
    }

    swap_pool pool(_self, m_iter->primary);
    auto pool_iter = pool.find(owner);
    eosio_assert(pool_iter != pool.end(), "no matched pool record");

    auto owner_weights = pool_iter->weights * rate;
    auto total_weights = m_iter->total_weights;

    double out_rate = owner_weights / total_weights;

    extended_asset x_quantity = extended_asset(std::floor(m_iter->tokenx.amount * out_rate * (1 - out_fee)), x);
    extended_asset y_quantity = extended_asset(std::floor(m_iter->tokeny.amount * out_rate * (1 - out_fee)), y);

    eosio_assert(x_quantity.amount > 0 && y_quantity.amount > 0, "dust attack detected");
    bool last_one = false;
    if (rate == 1) {
        owner_weights = pool_iter->weights;
        pool.erase(pool_iter);
        auto pool_begin = pool.begin();
        if (pool_begin == pool.end()) {
            x_quantity = m_iter->tokenx;
            y_quantity = m_iter->tokeny;

            auto primary = m_iter->primary;
            swap_order order(_self, primary);
            auto o_iter = order.begin();
            while (o_iter != order.end()) {
                auto withdrawal = o_iter->filled;
                add_balance(o_iter->owner, withdrawal, o_iter->owner);
                SEND_INLINE_ACTION(*this, orderchange, { _self, N(active) }, { o_iter->bid_id, WITHDRAW });
                o_iter = order.erase(o_iter);
            }
            get_order_prefix(primary);
            swap_order order_sell(_self, primary);
            auto o_sell_iter = order_sell.begin();
            while (o_sell_iter != order_sell.end()) {
                auto withdrawal = o_sell_iter->filled;
                add_balance(o_sell_iter->owner, withdrawal, o_sell_iter->owner);
                SEND_INLINE_ACTION(*this, orderchange, { _self, N(active) }, { o_sell_iter->bid_id, WITHDRAW });
                o_sell_iter = order_sell.erase(o_sell_iter);
            }
        } else if (++pool_begin == pool.end()) {
            last_one = true;
            pool_begin--;
            owner_weights = pool_begin->weights;
        }
    } else {
        pool.modify(pool_iter, 0, [&](auto& s) {
            s.weights -= owner_weights;
        });
        eosio_assert(pool_iter->weights > 0, "negative pool weights amount");
    }
    m_index.modify(m_iter, 0, [&](auto& s) {
        s.tokenx -= x_quantity;
        s.tokeny -= y_quantity;
        s.total_weights -= owner_weights;
        if (last_one)
            s.total_weights = owner_weights;
    });
    eosio_assert(m_iter->tokenx.amount >= 0, "negative tokenx amount");
    eosio_assert(m_iter->tokeny.amount >= 0, "negative tokeny amount");
    eosio_assert(m_iter->total_weights >= 0, "negative total weights amount");
    if (m_iter->total_weights == 0)
        m_index.erase(m_iter);

    if (rate != 1)
        eosio_assert(pool_iter->weights / m_iter->total_weights > 0.0001, "The remaining weight is too low");

    SEND_INLINE_ACTION(*this, outreceipt, { _self, N(active) }, { owner, x_quantity, y_quantity });
    add_balance(owner, x_quantity, owner);
    add_balance(owner, y_quantity, owner);
}

void token::uniswaporder(account_name owner, extended_asset quantity, extended_asset to, double price, account_name id, account_name rampay)
{
    auto from_sym = quantity.get_extended_symbol();
    auto to_sym = to.get_extended_symbol();
    swap_market market(_self, _self);
    auto m_index = market.get_index<N(bysymbol)>();
    auto m_iter = m_index.find(uniswap_market::key(from_sym, to_sym));
    eosio_assert(m_iter != m_index.end(), "this uniswap pair dose not exist");

    auto marketx = m_iter->tokenx;
    auto markety = m_iter->tokeny;
    auto marketx_sym = marketx.get_extended_symbol();
    auto markety_sym = markety.get_extended_symbol();
    uint64_t primary = m_iter->primary;

    if (from_sym == marketx_sym && to_sym == markety_sym) {
        uniswapdeal(owner, marketx, markety, quantity, to, primary, price, rampay);
    } else if (from_sym == markety_sym && to_sym == marketx_sym) {
        get_order_prefix(primary);
        uniswapdeal(owner, markety, marketx, quantity, to, primary, price, rampay);
    } else {
        eosio_assert(false, "symbol precision mismatch");
    }

    m_index.modify(m_iter, 0, [&](auto& s) {
        s.tokenx = marketx;
        s.tokeny = markety;
    });
}

void token::uniswapdeal(account_name owner, extended_asset& market_from, extended_asset& market_to, extended_asset from, extended_asset to, uint64_t primary, double price, account_name rampay)
{
    auto from_sym = from.get_extended_symbol();
    auto to_sym = to.get_extended_symbol();
    double real_market_from = get_real_asset(market_from);
    double real_market_to = get_real_asset(market_to);
    double real_market_total = real_market_from * real_market_to;
    uint64_t min_price = real_market_from / real_market_to * std::pow(2, 32);
    uint64_t price_t = price * std::pow(2, 32);
    bool buy = (from.amount == 0) ? true : false;
    bool limit = (price_t == 0) ? false : true;
    bool first = true;
    swap_order order(_self, primary);
    auto o_index = order.get_index<N(bylowerprice)>();
    auto o_iter = o_index.lower_bound(0);

    extended_asset sub_asset = extended_asset(0, from_sym);
    extended_asset add_asset = extended_asset(0, to_sym);
    extended_asset from_scrap = extended_asset(0, from_sym);
    extended_asset to_scrap = extended_asset(0, to_sym);
    while (true) {
        /**
         * Process:
         * 1. trade with order
         * 2. search the min_price which is must uniswap to
         * 3. uniswap
         * 4. update price
         * loop above until the end
         * 
         * The following two conditions mean end:
         * 1. marketorder and from is zero
         * 2. limitorder 
         *  1) to is zero
         *  2) the amount of to is positive and min_price is equal to price which is limit price and no record in order_table
         *  3) limit order's price less than market's price, order it directly
         */
        while (o_iter != o_index.end()
            && o_iter->price <= min_price
            && !is_deplete(buy, from, to)) {
            if (limit && first && o_iter->price > price_t)
                break;
            first = false;

            double real_order_price = (double)o_iter->price / std::pow(2, 32);
            extended_asset trade_asset = buy ? get_asset_by_amount<double, std::ceil>(get_real_asset(to) * real_order_price, from_sym) : from;
            extended_asset order_asset = o_iter->quantity;

            extended_asset taker_pay;
            extended_asset taker_ex_gain;
            extended_asset maker_pay;

            account_name order_owner = o_iter->owner;
            uint64_t order_bid_id = o_iter->bid_id;
            if (trade_asset >= order_asset) {
                taker_pay = order_asset;
                taker_ex_gain = o_iter->filled;
                maker_pay = taker_ex_gain;
                o_iter = o_index.erase(o_iter);
                SEND_INLINE_ACTION(*this, orderchange, { _self, N(active) }, { order_bid_id, COMPLETE });
            } else {
                taker_pay = trade_asset;
                taker_ex_gain = buy ? to : get_asset_by_amount<double, std::floor>(get_real_asset(trade_asset) / real_order_price, to_sym);
                maker_pay = taker_ex_gain;

                o_index.modify(o_iter, 0, [&](auto& s) {
                    s.quantity -= taker_pay;
                    s.filled -= maker_pay;
                });
                eosio_assert(o_iter->quantity.amount > 0, "negative bid quantity amount"); //never hanppend
                eosio_assert(o_iter->filled.amount > 0, "negative bid filled amount"); //never happend
                if (o_iter->quantity.amount <= 2) {
                    from_scrap += o_iter->quantity;
                    o_iter = o_index.erase(o_iter);
                    SEND_INLINE_ACTION(*this, orderchange, { _self, N(active) }, { order_bid_id, COMPLETE });
                }
            }
            extended_asset maker_ex_gain = taker_pay;
            extended_asset taker_fee = extended_asset(std::ceil(taker_ex_gain.amount * order_taker_fee), taker_ex_gain.get_extended_symbol());
            extended_asset maker_fee = extended_asset(std::ceil(maker_ex_gain.amount * order_maker_fee), maker_ex_gain.get_extended_symbol());

            extended_asset taker_real_gain = taker_ex_gain - taker_fee;
            extended_asset maker_real_gain = maker_ex_gain - maker_fee;
            eosio_assert(taker_pay.amount > 0 && maker_real_gain.amount > 0, "dust attack detected in bid order");
            from_scrap += taker_pay - maker_real_gain;
            to_scrap += maker_pay - taker_real_gain;

            if (taker_ex_gain.amount != 0)
                eosio_assert(taker_fee.amount > 0 && maker_fee.amount > 0, "fee cannot zero");
            else {
                taker_fee = to_scrap;
                maker_fee = from_scrap;
            }
            from -= taker_pay;
            to -= taker_ex_gain;
            add_asset += taker_real_gain;
            sub_asset += taker_pay;
            add_balance(order_owner, maker_real_gain, rampay);
            SEND_INLINE_ACTION(*this, traderecord, { _self, N(active) }, { owner, order_owner, taker_pay, taker_real_gain, taker_fee, order_bid_id });
            SEND_INLINE_ACTION(*this, traderecord, { _self, N(active) }, { order_owner, owner, maker_pay, maker_real_gain, maker_fee, order_bid_id });
        }
        if (is_deplete(buy, from, to) || (limit && price_t <= min_price))
            break;

        uint64_t new_price_t = uint64_max;
        double new_price;
        if (buy) {
            extended_asset new_to = market_to - to;
            if (new_to.amount > 0) {
                double real_new_to = get_real_asset(new_to);
                new_price = real_market_total / (real_new_to * real_new_to);
                new_price_t = new_price * std::pow(2, 32);
            }
        } else {
            extended_asset new_from = market_from + from;
            double real_new_from = get_real_asset(new_from);
            new_price = (real_new_from * real_new_from) / real_market_total;
            new_price_t = new_price * std::pow(2, 32);
        }

        auto old_price = min_price;
        price_t = limit ? price_t : new_price_t;
        min_price = price_t;
        if (o_iter != o_index.end())
            min_price = std::min(min_price, o_iter->price);
        min_price = std::min(min_price, new_price_t);

        double real_new_from;
        extended_asset spread_from;
        double cal_price = std::sqrt(min_price / std::pow(2, 32));
        if (min_price == new_price_t && min_price != uint64_max)
            cal_price = std::sqrt(new_price);
        real_new_from = cal_price * std::sqrt(real_market_total);
        extended_asset new_market_from = get_asset_by_amount<double, std::round>(real_new_from, from_sym);
        spread_from = new_market_from - market_from;
        market_from = new_market_from;

        auto real_new_to = real_market_total / real_new_from;
        auto new_market_to = get_asset_by_amount<double, std::round>(real_new_to, to_sym);
        auto spread_ex_to = market_to - new_market_to;
        market_to = new_market_to;
        auto to_fee = extended_asset(std::ceil(spread_ex_to.amount * uniswap_fee), spread_ex_to.get_extended_symbol());
        extended_asset spread_to = spread_ex_to - to_fee;
        to_scrap += to_fee;
        eosio_assert(spread_from.amount > 0 && spread_to.amount > 0, "dust attack detected in uniswap");

        from -= spread_from;
        to -= spread_ex_to;
        eosio_assert(from.amount >= 0 || to.amount >= 0, "can't sub/add negative asset"); //never happened

        sub_asset += spread_from;
        add_asset += spread_to;

        SEND_INLINE_ACTION(*this, pricerec, { _self, N(active) }, { old_price, min_price });
        SEND_INLINE_ACTION(*this, traderecord, { _self, N(active) },
            { owner, eos_account, spread_from, spread_to, to_fee, 0 });
    }
    if (to.amount > 0 || from.amount > 0) {
        extended_asset bid_asset;
        extended_asset filled;
        if (to.amount > 0) {
            bid_asset = to;
            filled = get_asset_by_amount<double, std::ceil>(get_real_asset(to) * price, from_sym);
            sub_asset += filled;
        } else {
            filled = from;
            bid_asset = get_asset_by_amount<double, std::ceil>(get_real_asset(from) / price, to_sym);
            sub_asset += from;
        }
        bid_order(owner, primary, price, bid_asset, filled, rampay);
    }
    add_balance(owner, add_asset, rampay);
    sub_balance(owner, sub_asset);

    market_from += from_scrap;
    market_to += to_scrap;

    SEND_INLINE_ACTION(*this, uniswapsnap, { _self, N(active) },
        { owner, add_asset });
}

void token::withdraw(account_name owner, extended_symbol x, extended_symbol y, uint64_t bid_id)
{
    require_auth(owner);
    swap_market market(_self, _self);
    auto m_index = market.get_index<N(bysymbol)>();
    auto m_iter = m_index.find(uniswap_market::key(x, y));
    eosio_assert(m_iter != m_index.end(), "no matched uniswap pair");

    uint64_t primary = m_iter->primary;
    extended_asset order_asset;
    swap_order o_order(_self, primary);
    auto o_iter = o_order.find(bid_id);
    if (o_iter == o_order.end()) {
        get_order_prefix(primary);
        swap_order order(_self, primary);
        auto o_it = order.find(bid_id);
        eosio_assert(o_it != order.end() && o_it->owner == owner, "No such bid order");
        order_asset = o_it->filled;
        order.erase(o_it);
    } else {
        eosio_assert(o_iter->owner == owner, "No such bid order");
        order_asset = o_iter->filled;
        o_order.erase(o_iter);
    }
    add_balance(owner, order_asset, owner);

    SEND_INLINE_ACTION(*this, orderchange, { _self, N(active) }, { bid_id, WITHDRAW });
}

void token::lockreserve(account_name owner, extended_symbol x, extended_symbol y)
{
    require_auth(owner);
    swap_market market(_self, _self);
    auto m_index = market.get_index<N(bysymbol)>();
    auto m_iter = m_index.find(uniswap_market::key(x, y));
    eosio_assert(m_iter != m_index.end(), "no matched uniswap pair");

    uint64_t primary = m_iter->primary;
    swap_pool pool(_self, primary);
    auto pool_iter = pool.find(owner);
    eosio_assert(pool_iter != pool.end(), "no matched pool record");

    swap_lock lock_tbl(_self, primary);
    auto req = lock_tbl.find(owner);

    if (req == lock_tbl.end()) {
        lock_tbl.emplace(owner, [&](auto& r) {
            r.owner = owner;
            r.request_time = 0;
        });
    } else if (req->request_time != 0) { // relock
        lock_tbl.modify(req, 0, [&](auto& s) {
            s.request_time = 0;
        });
    }
}

void token::unlckreserve(account_name owner, extended_symbol x, extended_symbol y)
{
    require_auth(owner);
    swap_market market(_self, _self);
    auto m_index = market.get_index<N(bysymbol)>();

    auto m_iter = m_index.find(uniswap_market::key(x, y));
    eosio_assert(m_iter != m_index.end(), "no matched uniswap pair");
    uint64_t primary = m_iter->primary;

    swap_lock lock_tbl(_self, primary);
    auto req = lock_tbl.find(owner);
    eosio_assert(req != lock_tbl.end(), "No locked record.");
    eosio_assert(req->request_time == 0, "Repeated unlocking is not allowed.");
    lock_tbl.modify(req, 0, [&](auto& s) {
        s.request_time = now();
    });
}

/**
 * If it is a sell order, need to order buy and vice versa.
 * The price must also be reversed
 */
void token::bid_order(account_name owner, uint64_t primary, double price, extended_asset quantity, extended_asset sub_asset, account_name rampay)
{
    get_order_prefix(primary);
    swap_order o_order(_self, primary);
    uint64_t price_t = std::pow(2, 32) / price;
    auto hash = sha256<bid_id_args>({ owner, quantity, price_t, primary, now() });
    uint64_t bid_id = uint64_t(*reinterpret_cast<const uint64_t*>(&hash));
    o_order.emplace(rampay, [&](auto& r) {
        r.bid_id = bid_id;
        r.owner = owner;
        r.price = price_t;
        r.quantity = quantity;
        r.filled = sub_asset;
    });

    SEND_INLINE_ACTION(*this, bidrec, { _self, N(active) }, { price_t, quantity, sub_asset, bid_id });
}

void token::deal_inverse_order(uint64_t market_price, uint64_t primary, account_name rampay)
{
    swap_order order(_self, primary);
    auto o_index = order.get_index<N(bylowerprice)>();
    auto o_iter = o_index.lower_bound(0);
    auto price = std::pow(2, 64) / o_iter->price;
    while (o_iter != o_index.end() && price > market_price) {
        account_name owner = o_iter->owner;
        extended_asset to = o_iter->quantity;
        extended_asset from = o_iter->filled;
        auto bid_id = o_iter->bid_id;
        add_balance(owner, from, rampay);
        o_iter = o_index.erase(o_iter);
        SEND_INLINE_ACTION(*this, orderchange, { _self, N(active) }, { bid_id, INVERSE });
        uniswaporder(owner, from, extended_asset(0, to.get_extended_symbol()), price / std::pow(2, 32), owner, rampay);
    }
}

bool token::check_bancor_pair(const extended_symbol& x, const extended_symbol& y)
{
    stats statstable(_self, x.contract);
    const auto& st = statstable.get(x.name(), "token with symbol does not exist");
    extended_symbol cw_sym = extended_symbol(st.connector_balance.symbol, st.connector_balance_issuer);
    if (st.connector_weight > 0 && y == cw_sym)
        return true;

    stats paritable(_self, y.contract);
    const auto& pair_st = paritable.get(y.name(), "token with symbol does not exist");
    extended_symbol pair_cw_sym = extended_symbol(pair_st.connector_balance.symbol, pair_st.connector_balance_issuer);
    if (pair_st.connector_weight > 0 && (x == pair_cw_sym || (st.connector_weight > 0 && cw_sym == pair_cw_sym)))
        return true;

    return false;
}

double token::get_real_asset(extended_asset quantity)
{
    return (double)quantity.amount / std::pow(10, quantity.get_extended_symbol().precision());
}

void token::get_order_prefix(uint64_t& primary)
{
    primary ^= uniswap_order_prefix;
}

template <typename T, T (*wipe_function)(T)>
extended_asset token::get_asset_by_amount(T amount, extended_symbol symbol)
{
    uint64_t asset_amount = wipe_function(amount * std::pow(10, symbol.precision()));
    extended_asset asset = extended_asset(asset_amount, symbol);
    eosio_assert(asset.is_valid(), "Invalid asset, please change symbol!");
    return asset;
}

bool token::is_deplete(bool limit, extended_asset from, extended_asset to)
{
    return (!limit && from.amount <= 0) || (limit && to.amount <= 0);
}
} /// namespace eosio