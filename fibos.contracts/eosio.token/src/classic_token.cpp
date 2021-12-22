/**
 *  @file
 *  @copyright defined in fibos/LICENSE.txt
 */

#include <eosio.token/eosio.token.hpp>

namespace eosio {

void token::create(account_name issuer,
    asset max_supply)
{
    asset a(0, max_supply.symbol);
    excreate(eos_account, max_supply, 0, a, a, a, time_point_sec(), 0, 0, issuer);
}

void token::issue(account_name to, asset quantity, string memo)
{
    exissue(to, extended_asset(quantity, eos_account), memo);
}

void token::retire(asset quantity, string memo)
{
    exretire(eos_account, extended_asset(quantity, eos_account), memo);
}

void token::transfer(account_name from, account_name to, asset quantity, string memo)
{
    extransfer(from, to, extended_asset(quantity, eos_account), memo);
}

void token::close(account_name owner, symbol_type symbol)
{
    exclose(owner, extended_symbol(symbol, eos_account));
}
}