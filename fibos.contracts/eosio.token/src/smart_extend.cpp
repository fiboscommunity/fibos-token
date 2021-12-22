/**
 *  @file
 *  @copyright defined in fibos/LICENSE.txt
 */

#include <eosio.token/eosio.token.hpp>

namespace eosio {

void token::exdestroy(extended_symbol sym)
{
    eosio_assert(sym.is_valid(), "invalid symbol name");

    auto sym_name = sym.name();

    require_auth(sym.contract);

    stats statstable(_self, sym.contract);
    const auto& st = statstable.get(sym_name, "token with symbol does not exist");

    if (st.supply.amount > 0) {
        sub_balance(st.issuer, extended_asset(st.supply, st.issuer));
    }

    if (st.reserve_supply.amount > 0) {
        lock_accounts from_acnts(_self, sym.contract);

        uint64_t balances = 0;
        for (auto it = from_acnts.begin(); it != from_acnts.end();) {
            if (it->balance.get_extended_symbol() == sym) {
                balances += it->balance.amount;
                it = from_acnts.erase(it);
            } else {
                it++;
            }
        }

        eosio_assert(st.reserve_supply.amount == balances, "reserve_supply must all in issuer");
    }
    if (st.connector_balance.amount > 0) {
        stats connect_symbol_statstable(_self, st.connector_balance_issuer);
        connect_symbol_statstable.get(st.connector_balance.symbol.name(), "token with symbol does not exist");

        add_balance(st.issuer, extended_asset(st.connector_balance, st.connector_balance_issuer), st.issuer);
    }

    statstable.erase(st);
}

void token::exchange(account_name owner, extended_asset quantity, extended_asset to, double price, account_name id, string memo)
{
    require_auth(owner);
    eosio_assert(quantity.is_valid() && to.is_valid(), "invalid exchange currency");
    eosio_assert(price >= 0 && price < ~uint32_t(0), "invaild price");
    eosio_assert((price == 0 && quantity.amount > 0 && to.amount == 0)
            || (price > 0 && ((quantity.amount > 0 && to.amount == 0) || (quantity.amount == 0 && to.amount > 0))),
        "invaild value");
    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");

    auto from_sym = quantity.get_extended_symbol();
    auto to_sym = to.get_extended_symbol();
    if (check_bancor_pair(from_sym, to_sym))
        bancororder(owner, quantity, to_sym);
    else
        uniswaporder(owner, quantity, to, price, id, owner);
}

void token::bancororder(account_name owner, extended_asset quantity, extended_symbol tosym)
{

    eosio_assert(quantity.amount > 0, "must exchange positive amount");
    eosio_assert(tosym != quantity.get_extended_symbol(), "cannot exchange to the same currency");

    sub_balance(owner, quantity);

    stats statstable(_self, quantity.contract);
    const auto& st = statstable.get(quantity.get_extended_symbol().name(), "token with symbol does not exist");
    extended_symbol st_cw_sym = extended_symbol(st.connector_balance.symbol, st.connector_balance_issuer);

    stats to_statstable(_self, tosym.contract);
    const auto& to_st = to_statstable.get(tosym.name(), "tosym token with symbol does not exist");
    extended_symbol to_cw_sym = extended_symbol(to_st.connector_balance.symbol, to_st.connector_balance_issuer);

    if (tosym == st_cw_sym) {
        quantity = sell(owner, quantity);
    } else if (to_cw_sym == quantity.get_extended_symbol()) {
        quantity = buy(owner, quantity, tosym);
    } else {
        eosio_assert(false, "invaild exchange");
    }

    add_balance(owner, quantity, owner);
}

void token::exshare(extended_asset quantity, extended_symbol tosym, string memo)
{
    auto issuer = tosym.contract;
    require_auth(issuer);

    eosio_assert(quantity.is_valid(), "invalid currency");
    eosio_assert(quantity.amount > 0, "must exshare positive amount");
    eosio_assert(tosym.is_valid(), "invalid currency symbol name");
    eosio_assert(tosym != quantity.get_extended_symbol(), "cannot exshare to the same currency");
    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");
    eosio_assert(issuer == tosym.contract, "only issuer can exshare");

    stats statstable(_self, issuer);
    const auto& st = statstable.get(tosym.name(), "token with symbol does not exist");

    uint64_t addamount = 0;
    statstable.modify(st, 0, [&](auto& s) {
        auto reserve_supply = s.reserve_supply;
        s.reset_connector_weight_by_connector_balance(s.get_balance() + quantity);
        addamount = reserve_supply.amount - s.reserve_supply.amount;
    });

    auto foundation_refund = extended_asset(addamount, extended_symbol(st.supply.symbol, issuer));

    auto foundation = get_foundation(issuer);
    sub_balance(issuer, quantity);
    add_balance(foundation, foundation_refund, issuer);

    eosio_assert(foundation_refund.amount >= 0, "foundation_refund is insufficient");
    lock_sub_balance(foundation, foundation_refund);

    SEND_INLINE_ACTION(*this, snapshot, { _self, N(active) }, { st.issuer, st.max_supply, st.connector_weight, st.max_exchange, st.supply, st.reserve_supply, st.connector_balance, st.reserve_connector_balance, st.connector_balance_issuer });
}

void token::setposition(extended_symbol sym, bool position, string memo)
{
    auto issuer = sym.contract;
    require_auth(get_foundation(issuer));

    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");

    stats statstable(_self, issuer);
    const auto& st = statstable.get(sym.name(), "token with symbol does not exists");
    statstable.modify(st, 0, [&](auto& s) {
        s.position = position;
    });
}

} /// namespace eosio
