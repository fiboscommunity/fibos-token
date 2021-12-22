/**
 *  @file
 *  @copyright defined in fibos/LICENSE.txt
 */

#include <eosio.token/eosio.token.hpp>

namespace eosio {

void token::ctxrecharge(account_name owner, extended_asset quantity, string memo)
{
    require_auth(owner);

    eosio_assert(quantity.is_valid(), "invalid currency");
    eosio_assert(quantity.amount > 0, "must recharge positive amount");

    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");

    require_recipient(quantity.contract);

    sub_balance(owner, quantity);
    contract_add_balance(owner, quantity, owner);
}

void token::ctxextract(account_name owner, extended_asset quantity, string memo)
{
    require_auth(owner);

    eosio_assert(quantity.is_valid(), "invalid currency");
    eosio_assert(quantity.amount > 0, "must extract positive amount");

    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");

    require_recipient(quantity.contract);

    contract_sub_balance(owner, quantity);
    add_balance(owner, quantity, owner);
}

void token::ctxtransfer(account_name from, account_name to, extended_asset quantity, string memo)
{
    eosio_assert(from != to, "cannot transfer to self");
    eosio_assert(is_account(from), "contract cannot transfer from nonexist account");

    eosio_assert(is_account(to), "contract cannot transfer to nonexist account");

    eosio_assert(quantity.is_valid(), "invalid quantity");
    eosio_assert(quantity.amount > 0, "must transfer positive amount within contract");

    account_name contract = quantity.contract;
    require_auth(contract);

    eosio_assert(memo.size() <= 256, "memo has more than 256 bytes");

    require_recipient(from);
    require_recipient(to);

    contract_sub_balance(from, quantity);
    contract_add_balance(to, quantity, contract);
}

void token::contract_sub_balance(account_name owner, extended_asset value)
{
    contract_accounts from_acnts(_self, owner);

    auto from_iter = from_acnts.get_index<N(byextendedasset)>();
    auto from = from_iter.find(account::key(value.get_extended_symbol()));

    eosio_assert(from != from_iter.end(), "no balance object found");
    eosio_assert(from->balance.amount >= value.amount, "overdrawn balance when sub contract balance");
    eosio_assert(from->balance.symbol == value.symbol, "symbol precision mismatch");

    if (from->balance.amount == value.amount) {
        from_iter.erase(from);
    } else {
        from_iter.modify(from, 0, [&](auto& a) {
            a.balance -= value;
        });
    }
}

void token::contract_add_balance(account_name owner, extended_asset value, account_name ram_payer)
{
    contract_accounts to_acnts(_self, owner);

    auto to_iter = to_acnts.get_index<N(byextendedasset)>();
    auto to = to_iter.find(account::key(value.get_extended_symbol()));

    if (to == to_iter.end()) {
        to_acnts.emplace(ram_payer, [&](auto& a) {
            a.primary = to_acnts.available_primary_key();
            a.balance = value;
        });
    } else {
        to_iter.modify(to, 0, [&](auto& a) {
            a.balance += value;
        });
    }
}
}