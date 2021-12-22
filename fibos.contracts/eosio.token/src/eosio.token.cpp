/**
 *  @file
 *  @copyright defined in fibos/LICENSE.txt
 */

#include <eosio.token/eosio.token.hpp>

#include "./bancor.cpp"
#include "./classic_token.cpp"
#include "./smart_token.cpp"
#include "./smart_extend.cpp"
#include "./contract_account.cpp"
#include "./lock_account.cpp"
#include "./record.cpp"
#include "./uniswap.cpp"

namespace eosio {

} /// namespace eosio

EOSIO_ABI(eosio::token,
    //classic tokens
    (create)(issue)(transfer)(close)(retire)
    //smart tokens
    (excreate)(exissue)(extransfer)(exclose)(exretire)(exdestroy)
    //
    (exchange)(exshare)
    //
    (ctxrecharge)(ctxextract)(ctxtransfer)
    //
    (exunlock)(exlock)(exlocktrans)(setposition)
    //
    (receipt)(snapshot)(outreceipt)(traderecord)(orderchange)(bidrec)(uniswapsnap)
    //
    (addreserves)(outreserves)(withdraw)
    //
    (unlckreserve)(lockreserve))