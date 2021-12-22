/**
 *  @file
 *  @copyright defined in fibos/LICENSE.txt
 */

#include <eosio.token/eosio.token.hpp>

namespace eosio {

void token::currency_stats::reset_connector_weight(asset newsupply)
{
    eosio_assert(connector_weight > 0, "only smart token can be adjusted");
    eosio_assert(supply.symbol == newsupply.symbol, "currency symbol should be same to connector balance symbol");
    eosio_assert(newsupply.amount > 0, "negative currency amount");

    max_exchange.amount += newsupply.amount - get_supply().amount;
    eosio_assert(max_exchange.amount <= max_supply.amount, "exceed maximum_exchange amount");

    connector_weight = (get_supply().amount * connector_weight) / newsupply.amount;
    eosio_assert(connector_weight <= 1, "leading to invalid connector_weight");

    double s = newsupply.amount;
    double r = get_balance().amount;
    double e = connector_balance.amount;
    double v = s * (1.0 - std::pow(1.0 - e / r, connector_weight));

    supply.amount = std::round(v);
    reserve_supply.amount = newsupply.amount - supply.amount;
}

void token::currency_stats::reset_connector_weight_by_connector_balance(asset newconnector_balance)
{
    eosio_assert(connector_weight > 0, "only smart token can be adjusted");
    eosio_assert(connector_balance.symbol == newconnector_balance.symbol, "new connector_balance symbol should be same to origin connector balance symbol");
    eosio_assert(newconnector_balance.amount > 0, "negative currency amount");

    connector_weight = (newconnector_balance.amount * connector_weight) / get_balance().amount;
    eosio_assert(connector_weight <= 1, "leading to invalid connector_weight");

    double s = get_supply().amount;
    double r = newconnector_balance.amount;
    double e = (newconnector_balance - reserve_connector_balance).amount;
    double v = s * (1.0 - std::pow(1.0 - e / r, connector_weight));

    supply.amount = std::round(v);
    reserve_supply.amount = s - supply.amount;
    connector_balance.amount = newconnector_balance.amount - reserve_connector_balance.amount;
}

extended_asset token::buy(account_name rampay, extended_asset quantity, extended_symbol to)
{
    stats to_statstable(_self, to.contract);
    const auto& to_st = to_statstable.get(to.name());

    eosio_assert(to_st.connector_weight > 0, "only smart token can be exchanged");
    eosio_assert(extended_symbol(to_st.supply.symbol, to.contract) == to, "symbol precision mismatch");
    extended_symbol to_cb_sym = extended_symbol(to_st.connector_balance.symbol, to_st.connector_balance_issuer);
    eosio_assert(to_cb_sym == quantity.get_extended_symbol(), "buy symbol mismatch");

    double r = to_st.get_balance().amount;
    double s = to_st.get_supply().amount;
    double e = quantity.amount;
    double t;

    if (to_st.connector_weight == 1) {
        double price = get_stable_price(to, s, r, rampay);
        t = e * price;
    } else
        t = s * (std::pow((1.0 + e / r), to_st.connector_weight) - 1.0);

    extended_asset value(std::floor(t), to);

    eosio_assert(value.amount != 0, "dust attack detected");
    eosio_assert(to_st.max_exchange.amount >= to_st.get_supply().amount + value.amount, "currency being exchanged exceeds available supply");

    eosio_assert(to_st.position, "can not exchange");

    to_statstable.modify(to_st, 0, [&](auto& s) {
        s.connector_balance.amount += quantity.amount;
        s.supply.amount += value.amount;
    });

    extended_asset fee(value.amount * to_st.buy_fee, to);
    add_balance(get_foundation(value.contract), fee, rampay);
    value -= fee;

    SEND_INLINE_ACTION(*this, snapshot, { _self, N(active) }, { to_st.issuer, to_st.max_supply, to_st.connector_weight, to_st.max_exchange, to_st.supply, to_st.reserve_supply, to_st.connector_balance, to_st.reserve_connector_balance, to_st.connector_balance_issuer });
    SEND_INLINE_ACTION(*this, receipt, { _self, N(active) }, { quantity, value, fee });

    return value;
}

extended_asset token::sell(account_name rampay, extended_asset quantity)
{
    stats statstable(_self, quantity.contract);
    const auto& st = statstable.get(quantity.symbol.name());

    eosio_assert(st.connector_weight > 0, "only smart token can be exchanged");
    eosio_assert(st.position, "can not exchange");

    extended_asset fee(quantity.amount * st.sell_fee, quantity.get_extended_symbol());
    add_balance(get_foundation(quantity.contract), fee, rampay);
    quantity -= fee;

    double r = st.get_balance().amount;
    double s = st.get_supply().amount;
    double t = quantity.amount;
    double e;

    if (st.connector_weight == 1) {
        double price = get_stable_price(quantity.get_extended_symbol(), s, r, rampay);
        e = t / price;
    } else
        e = r * (1.0 - std::pow(1.0 - t / s, 1.0 / st.connector_weight));

    extended_symbol sym = extended_symbol(st.connector_balance.symbol, st.connector_balance_issuer);

    extended_asset value(std::floor(e), sym);

    eosio_assert(value.amount != 0, "dust attack detected");

    int64_t minimum_connector_balance = std::pow(10, st.get_balance().symbol.precision());
    eosio_assert(st.get_balance().amount - value.amount >= minimum_connector_balance, "connector balance is insufficient");

    statstable.modify(st, 0, [&](auto& s) {
        s.connector_balance.amount -= value.amount;
        s.supply.amount -= quantity.amount;
    });

    SEND_INLINE_ACTION(*this, snapshot, { _self, N(active) }, { st.issuer, st.max_supply, st.connector_weight, st.max_exchange, st.supply, st.reserve_supply, st.connector_balance, st.reserve_connector_balance, st.connector_balance_issuer });
    SEND_INLINE_ACTION(*this, receipt, { _self, N(active) }, { quantity, value, fee });

    return value;
}

extended_asset token::unlock_with_connector(extended_asset quantity, bool isExec)
{
    stats statstable(_self, quantity.contract);
    const auto& st = statstable.get(quantity.symbol.name());

    double e;
    if (st.reserve_connector_balance.amount) {
        double r = st.reserve_connector_balance.amount;
        double s = st.reserve_supply.amount;
        double t = quantity.amount;

        if (st.connector_weight == 1)
            e = r * t / s;
        else
            e = r * (1.0 - std::pow(1.0 - t / s, 1.0 / st.connector_weight));

        e = std::ceil(e);
        eosio_assert(e != 0, "dust attack detected when unlock");

        int64_t minimum_connector_balance = std::pow(10, st.get_balance().symbol.precision());
        eosio_assert(st.get_balance().amount - e >= minimum_connector_balance, "connector balance is insufficient");
    }

    stats connect_symbol_statstable(_self, st.connector_balance_issuer);
    connect_symbol_statstable.get(st.connector_balance.symbol.name(), "token with symbol does not exist");

    extended_asset value(e, extended_symbol(st.connector_balance.symbol, st.connector_balance_issuer));

    if (isExec) {
        statstable.modify(st, 0, [&](auto& s) {
            s.reserve_supply.amount -= quantity.amount;
            s.supply.amount += quantity.amount;
            s.connector_balance.amount += value.amount;
            s.reserve_connector_balance.amount -= value.amount;
        });
        SEND_INLINE_ACTION(*this, snapshot, { _self, N(active) }, { st.issuer, st.max_supply, st.connector_weight, st.max_exchange, st.supply, st.reserve_supply, st.connector_balance, st.reserve_connector_balance, st.connector_balance_issuer });
        SEND_INLINE_ACTION(*this, receipt, { _self, N(active) }, { quantity, value, extended_asset(0, quantity.get_extended_symbol()) });
    }

    return value;
}

extended_asset token::unlock_with_connector(extended_asset connector, extended_asset token)
{
    stats statstable(_self, token.contract);
    const auto& st = statstable.get(token.symbol.name());

    double t;
    if (st.reserve_connector_balance.amount) {
        double r = st.reserve_connector_balance.amount;
        double s = st.reserve_supply.amount;
        double e = connector.amount;

        if (st.connector_weight == 1)
            t = e * s / r;
        else
            t = s * (1.0 - std::pow(1.0 - e / r, st.connector_weight));
        t = std::floor(t);

        int64_t minimum_connector_balance = std::pow(10, st.get_balance().symbol.precision());
        eosio_assert(st.get_balance().amount - e >= minimum_connector_balance, "connector balance is insufficient");
    }

    stats connect_symbol_statstable(_self, st.connector_balance_issuer);
    connect_symbol_statstable.get(st.connector_balance.symbol.name(), "token with symbol does not exist");

    extended_asset value(t, token.get_extended_symbol());

    statstable.modify(st, 0, [&](auto& s) {
        s.reserve_supply.amount -= value.amount;
        s.supply.amount += value.amount;
        s.connector_balance.amount += connector.amount;
        s.reserve_connector_balance.amount -= connector.amount;
    });
    SEND_INLINE_ACTION(*this, snapshot, { _self, N(active) }, { st.issuer, st.max_supply, st.connector_weight, st.max_exchange, st.supply, st.reserve_supply, st.connector_balance, st.reserve_connector_balance, st.connector_balance_issuer });
    SEND_INLINE_ACTION(*this, receipt, { _self, N(active) }, { connector, value, extended_asset(0, connector.get_extended_symbol()) });

    return value;
}

extended_asset token::unlock_with_locked_tokens(extended_asset quantity)
{
    stats statstable(_self, quantity.contract);
    const auto& st = statstable.get(quantity.symbol.name());

    int64_t s = st.supply.amount;
    int64_t s_lock = st.reserve_supply.amount;
    int64_t r = st.connector_balance.amount;
    int64_t r_lock = st.reserve_connector_balance.amount;
    int64_t v1;
    int64_t v2;
    int64_t left = 0;
    int64_t right = s_lock;
    int64_t mid = (left + right) / 2;
    int64_t t = quantity.amount;

    double cw = st.connector_weight;

    while (left < right && mid != left && mid != right) {
        v1 = bancor_sell_by_tokens(s, r, s_lock, r_lock, cw, mid);
        v2 = bancor_sell_by_tokens(0, 0, s_lock, r_lock, cw, mid + t);

        if (v1 > v2)
            right = mid;
        else
            left = mid;
        mid = (left + right) / 2;
    }
    extended_asset value(mid, quantity.get_extended_symbol());

    int64_t minimum_connector_balance = std::pow(10, st.get_balance().symbol.precision());
    int64_t min = r + r_lock + minimum_connector_balance - bancor_sell_by_tokens(s, r, s_lock, r_lock, cw, (s_lock - t));
    eosio_assert(r >= min, "cannot unlock with insufficient connector balance");

    statstable.modify(st, 0, [&](auto& s) {
        s.supply += quantity;
        s.reserve_supply -= (quantity + value);
        s.reserve_connector_balance.amount -= v1;
    });

    return value;
}

int64_t token::bancor_sell_by_tokens(int64_t s, int64_t r, int64_t s_lock, int64_t r_lock, double cw, int64_t t)
{
    return (r + r_lock) * (1.0 - std::pow((1.0 - (double)t / (double)(s + s_lock)), 1.0 / cw));
}

double token::get_stable_price(extended_symbol symbol, double balance, double supply, account_name rampay)
{
    stable_prices price_tbl(_self, _self);
    auto from_iter = price_tbl.get_index<N(byextendedsymbol)>();
    auto from = from_iter.find(stable_price::key(symbol));

    double price;
    if (from != from_iter.end()) {
        price = from->price;
    } else {
        price = balance / supply;
        price_tbl.emplace(rampay, [&](auto& r) {
            r.primary = price_tbl.available_primary_key();
            r.price = price;
            r.symbol = symbol;
        });
    }
    return price;
}
}