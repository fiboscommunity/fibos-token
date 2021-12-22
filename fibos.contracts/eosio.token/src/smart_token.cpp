/**
 *  @file
 *  @copyright defined in fibos/LICENSE.txt
 */

#include <eosio.token/eosio.token.hpp>

namespace eosio {
void token::excreate(account_name issuer, asset maximum_supply, double connector_weight,
    asset maximum_exchange, asset reserve_supply, asset reserve_connector_balance, time_point_sec expiration,
    double buy_fee, double sell_fee, account_name connector_balance_issuer)
{
    require_auth(issuer);

    eosio_assert(maximum_supply.is_valid(), "invalid supply");
    eosio_assert(maximum_supply.amount > 0, "maximum_supply must be positive");
    eosio_assert(reserve_supply.is_valid(), "invalid reserve_supply");
    eosio_assert(reserve_supply.symbol == maximum_supply.symbol, "reserve_supply symbol precision mismatch");
    eosio_assert(reserve_supply.symbol.precision() >= minimum_token_precision, "token precision too small");

    if (issuer != eos_account)
        eosio_assert(expiration >= time_point_sec(now()), "expiration must longer than now");
    else
        expiration = time_point_sec();

    stats statstable(_self, issuer);
    auto existing = statstable.find(maximum_supply.symbol.name());
    eosio_assert(existing == statstable.end(), "token with symbol already exists");

    if (connector_weight == 0) {
        eosio_assert(reserve_supply.amount <= maximum_supply.amount, "invalid reserve_supply amount");
        eosio_assert(reserve_supply.amount >= 0, "reserve_supply must be positive");
        lock_add_balance(get_foundation(issuer), extended_asset(reserve_supply, issuer), expiration, issuer);

        statstable.emplace(issuer, [&](auto& s) {
            s.issuer = issuer;
            s.max_supply = maximum_supply;
            s.supply.symbol = maximum_supply.symbol;
            s.position = false;
            s.connector_balance_issuer = eos_account;
            s.reserve_supply = reserve_supply;
        });
    } else {
        eosio_assert((connector_weight > 0) && (connector_weight <= 1), "invalid connector_weight");

        eosio_assert(maximum_exchange.is_valid(), "invalid maximum_exchange");
        eosio_assert(maximum_exchange.amount > 0, "maximum_exchange must be positive");
        eosio_assert(maximum_exchange.amount <= maximum_supply.amount, "invalid maximum_exchange amount");
        eosio_assert(maximum_exchange.symbol == maximum_supply.symbol, "maximum_exchange symbol precision mismatch");

        eosio_assert(reserve_supply.amount > 0, "reserve_supply must be positive");
        eosio_assert(reserve_supply.amount <= maximum_exchange.amount, "invalid reserve_supply amount");
        eosio_assert(reserve_connector_balance.is_valid(), "invalid reserve_connector_balance");

        int64_t minimum_connector_balance = std::pow(10, reserve_connector_balance.symbol.precision());
        eosio_assert(reserve_connector_balance.amount >= minimum_connector_balance, "connector balance is insufficient");

        eosio_assert(buy_fee >= 0 && buy_fee < 1, "invalid buy fee rate");
        eosio_assert(sell_fee >= 0 && sell_fee < 1, "invalid sell fee rate");

        stats connect_symbol_statstable(_self, connector_balance_issuer);
        const auto& cb_st = connect_symbol_statstable.get(reserve_connector_balance.symbol.name(), "token with symbol does not exist");

        eosio_assert(reserve_connector_balance.symbol == cb_st.supply.symbol, "reserve_connector_balance symbol precision mismatch");

        lock_add_balance(get_foundation(issuer), extended_asset(reserve_supply, issuer), expiration, issuer);

        statstable.emplace(issuer, [&](auto& s) {
            s.issuer = issuer;
            s.max_supply = maximum_supply;
            s.connector_weight = connector_weight;

            s.max_exchange = maximum_exchange;
            s.supply.symbol = maximum_supply.symbol;
            s.reserve_supply = reserve_supply;

            s.connector_balance.symbol = reserve_connector_balance.symbol;
            s.reserve_connector_balance = reserve_connector_balance;

            s.buy_fee = buy_fee;
            s.sell_fee = sell_fee;
            s.position = false;
            s.connector_balance_issuer = connector_balance_issuer;
        });
    }
}

void token::exissue(account_name to, extended_asset quantity, string memo)
{
    eosio_assert(is_account(to), "to account does not exist");

    auto issuer = quantity.contract;

    auto foundation = issuer;
    if (!has_auth(issuer)) {
        foundation = get_foundation(issuer);
        eosio_assert(issuer != foundation && has_auth(foundation), "only issuer and foundation can exissue");
    }

    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");

    stats statstable(_self, issuer);
    const auto& st = statstable.get(quantity.symbol.name(), "token with symbol does not exist, create token before issue");

    eosio_assert(st.connector_weight == 0, "only classic token can be issued");
    eosio_assert(quantity.is_valid(), "issue invalid currency");
    eosio_assert(quantity.amount > 0, "must issue positive amount");
    eosio_assert(quantity.symbol == st.supply.symbol, "symbol precision mismatch");
    eosio_assert(st.position, "can not exissue");

    eosio_assert(quantity.amount <= st.max_supply.amount - st.supply.amount - st.reserve_supply.amount, "amount exceeds available supply when issue");
    statstable.modify(st, 0, [&](auto& s) {
        s.supply += quantity;
    });

    add_balance(foundation, quantity, foundation);

    if (to != foundation) {
        SEND_INLINE_ACTION(*this, extransfer, { foundation, N(active) }, { foundation, to, quantity, memo });
    }
    SEND_INLINE_ACTION(*this, snapshot, { _self, N(active) }, { st.issuer, st.max_supply, st.connector_weight, st.max_exchange, st.supply, st.reserve_supply, st.connector_balance, st.reserve_connector_balance, st.connector_balance_issuer });
}

void token::extransfer(account_name from, account_name to, extended_asset quantity, string memo)
{
    eosio_assert(from != to, "cannot transfer to self");
    require_auth(from);

    if (to == N(eosio.ramfee) || to == N(eosio.saving)) {
        INLINE_ACTION_SENDER(eosio::token, exretire)
        (N(eosio.token), { { from, N(active) } }, { from, quantity, std::string("issue tokens for producer pay and savings") });
        return;
    }
    eosio_assert(is_account(to), "to account does not exist");

    require_recipient(from);
    require_recipient(to);

    eosio_assert(quantity.is_valid(), "invalid currency");
    eosio_assert(quantity.amount > 0, "must transfer positive amount");

    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");

    //producer pay
    auto payer = has_auth(to) ? to : from;

    sub_balance(from, quantity);
    add_balance(to, quantity, payer);
}

void token::exretire(account_name from, extended_asset quantity, string memo)
{
    require_auth(from);

    eosio_assert(quantity.is_valid(), "invalid quantity");
    eosio_assert(quantity.amount > 0, "must retire positive quantity");

    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");

    auto issuer = quantity.contract;

    stats statstable(_self, issuer);
    const auto& st = statstable.get(quantity.symbol.name(), "token with symbol does not exist");

    if (st.connector_weight == 0) {
        sub_balance(from, quantity);
        statstable.modify(st, 0, [&](auto& s) {
            s.supply -= quantity;
        });
    } else {
        uint64_t addamount = 0;
        statstable.modify(st, 0, [&](auto& s) {
            auto reserve_supply = s.reserve_supply;
            s.reset_connector_weight(s.get_supply() - quantity);
            addamount = reserve_supply.amount - s.reserve_supply.amount;
        });

        auto foundation_refund = extended_asset(addamount, extended_symbol(st.supply.symbol, issuer));

        auto foundation = get_foundation(issuer);
        if (issuer == foundation) {
            sub_balance(issuer, quantity - foundation_refund);
        } else {
            sub_balance(from, quantity);
            add_balance(foundation, foundation_refund, from);
        }
        eosio_assert(foundation_refund.amount >= 0, "foundation_refund is insufficient");
        lock_sub_balance(foundation, foundation_refund);
    }

    SEND_INLINE_ACTION(*this, snapshot, { _self, N(active) }, { st.issuer, st.max_supply, st.connector_weight, st.max_exchange, st.supply, st.reserve_supply, st.connector_balance, st.reserve_connector_balance, st.connector_balance_issuer });
}

void token::exclose(account_name owner, extended_symbol symbol)
{
    accounts acnts(_self, owner);
    auto it_iter = acnts.get_index<N(byextendedasset)>();
    auto it = it_iter.find(account::key(symbol));
    eosio_assert(it != it_iter.end(), "Balance entry does not exist or already deleted. Action will not have any effects.");
    eosio_assert(it->balance.amount == 0, "balance entry closed should be zero");
    it_iter.erase(it);
}

void token::sub_balance(account_name owner, extended_asset value)
{
    accounts from_acnts(_self, owner);

    auto from_iter = from_acnts.get_index<N(byextendedasset)>();
    auto from = from_iter.find(account::key(value.get_extended_symbol()));

    eosio_assert(from != from_iter.end(), "no balance object found.");
    eosio_assert(from->balance.amount >= value.amount, "overdrawn balance when sub balance");
    eosio_assert(from->balance.symbol == value.symbol, "symbol precision mismatch");

    from_iter.modify(from, owner, [&](auto& a) {
        a.balance -= value;
    });
}

void token::add_balance(account_name owner, extended_asset value, account_name ram_payer)
{
    accounts to_acnts(_self, owner);
    auto to_iter = to_acnts.get_index<N(byextendedasset)>();
    auto to = to_iter.find(account::key(value.get_extended_symbol()));

    if (to == to_iter.end()) {
        to_acnts.emplace(ram_payer, [&](auto& a) {
            a.primary = to_acnts.available_primary_key();
            a.balance = value;
        });
    } else if (to->balance.amount == 0) {
        to_iter.modify(to, 0, [&](auto& a) {
            a.balance = value;
        });
    } else {
        to_iter.modify(to, 0, [&](auto& a) {
            a.balance += value;
        });
    }
}
}