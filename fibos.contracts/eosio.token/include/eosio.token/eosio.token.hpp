/**
 *  @file
 *  @copyright defined in fibos/LICENSE.txt
 */
#pragma once

#include <eosiolib/asset.hpp>
#include <eosiolib/eosio.hpp>
#include <eosiolib/time.hpp>
#include <eosiolib/crypto.h>

#include <string>
#include <cmath>

namespace eosio {

using std::string;

static const account_name eos_account = N(eosio);
static const extended_symbol eos_sym = extended_symbol(S(4, EOS), eos_account);
static const extended_symbol fo_sym = extended_symbol(CORE_SYMBOL, eos_account);
constexpr double static_weights = 10000.0;
constexpr double out_fee = 0.001;
constexpr double uniswap_fee = 0.003;
constexpr double order_maker_fee = 0.001;
constexpr double order_taker_fee = 0.002;
constexpr uint64_t uint64_max = ~uint64_t(0);
constexpr uint64_t uniswap_order_prefix = 0xFF00000000000000;
constexpr uint64_t minimum_token_precision = 4;

class token : public contract {
    struct uniswap_state;

public:
    token(account_name self)
        : contract(self)
    {
    }

public:
    /*! @brief ClassicToken 创建函数
     @param issuer 通证发行账号
     @param max_supply 通证最大发行量
     */
    void create(account_name issuer, asset max_supply);

    /*! @brief ClassicToken 增发函数
     @param to 接收新通证的账号
     @param quantity 新增通证数量
     @param memo 备注
     */
    void issue(account_name to, asset quantity, string memo);

    /*! @brief ClassicToken 回收函数
     @param quantity 回收通证数量
     @param memo 备注
     */
    void retire(asset quantity, string memo);

    /*! @brief ClassicToken 转账函数
     @param from 发送账号
     @param to 接收账号
     @param quantity 转账通证数量
     @param memo 备注
     */
    void transfer(account_name from, account_name to, asset quantity, string memo);

    /*! @brief ClassicToken 资源回收函数
     * 当通证数量为0时，可以回收RAM

     @param owner 通证发行账号
     @param symbol 回收通证类型
     */
    void close(account_name owner, symbol_type symbol);

public:
    /*! @brief SmartToken 创建函数
     @param issuer 通证发行账号
     @param max_supply 最大可发行通证数量
     @param connector_weight 连接器权重
     @param max_exchange 最大可兑换(流通)的通证数量
     @param reserve_supply 未流通通证数量
     @param reserve_connector_balance 未流通通证保证金数量
     @param expiration 项目方预设的项目锁仓期
     @param buy_fee 兑入手续费率
     @param sell_fee 兑出手续费率
     @param connector_balance_issuer 准备金发行方
     */
    void excreate(account_name issuer, asset maximum_supply, double connector_weight,
        asset maximum_exchange, asset reserve_supply, asset reserve_connector_balance, time_point_sec expiration,
        double buy_fee, double sell_fee, account_name connector_balance_issuer);

    /*! @brief SmartToken 增发函数
     @param to 接收新通证的账号
     @param quantity 新增通证数量
     @param memo 备注
     */
    void exissue(account_name to, extended_asset quantity, string memo);

    /*! @brief SmartToken 销毁函数
    @param from 销毁通证的账号
     @param quantity 销毁通证数量
     @param memo 备注
     */
    void exretire(account_name from, extended_asset value, string memo);

    /*! @brief SmartToken 转账函数
     @param from 发送账号
     @param to 接收账号
     @param quantity 转账通证数量
     @param memo 备注
     */
    void extransfer(account_name from, account_name to, extended_asset quantity, string memo);

    /*! @brief SmartToken 资源回收函数
     * 当通证数量为0时，可以回收RAM

     @param owner 通证发行账号
     @param symbol 回收通证类型
     */
    void exclose(account_name owner, extended_symbol symbol);

public:
    /*! @brief SmartToken 兑换函数
     @param owner 兑换账号
     @param quantity 兑换通证数量
     @param tosymbol 兑换通证目标类型
     @param memo 备注
     */
    void exchange(account_name owner, extended_asset quantity, extended_asset to, double price, account_name id, string memo);

    /*! @brief SmartToken 分红函数
     @param quantity 填入保证金数量
     @param tosymbol 目标通证类型
     @param memo 备注
     */
    void exshare(extended_asset quantity, extended_symbol tosym, string memo);

    /*! @brief SmartToken 使用指定的通证，兑换出确定数量的目标通证
     @param owner 兑换账号
     @param fromsym 兑换通证类型
     @param quantity 兑换目标通证的数量
     @param memo 备注
     */
    void exchangeto(account_name owner, extended_symbol fromsym, extended_asset quantity, string memo);

    /*! @brief SmartToken 删除函数
     * 删除通证满足满足如下条件并回收RAM。
     * 1. 通证发行者
     * 2. 通证发行者拥有所有通证流通数量
     * 3. 没有设置锁仓期的通证

     @param sym 所需删除通证类型
     @param memo 备注
     */
    void exdestroy(extended_symbol sym);

public:
    /*! @brief LockToken 转账函数
     @param from 发送账号
     @param to 接收账号
     @param quantity 转账通证数量
     @param expiration 待转出锁仓时间
     @param expiration_to 待转入锁仓时间
     @param memo 备注
     */
    void exlocktrans(account_name from, account_name to, extended_asset quantity, time_point_sec expiration, time_point_sec expiration_to, string memo);

    /*! @brief LockToken 解锁函数
     @param owner 解锁账号
     @param quantity 解锁通证数量
     @param expiration 锁仓时间
     @param memo 备注
     */
    void exunlock(account_name owner, extended_asset quantity, time_point_sec expiration, string memo);

    /*! @brief LockToken 加锁函数
     @param owner 加锁账号
     @param quantity 加锁通证数量
     @param expiration 加锁时间
     @param memo 备注
     */
    void exlock(account_name from, extended_asset quantity, time_point_sec expiration, string memo);

public:
    /*! @brief ContractToken 充值函数
     @param owner 充值账号
     @param contract 合约名
     @param quantity 充值通证数量
     @param memo 备注
     */
    void ctxrecharge(account_name owner, extended_asset quantity, string memo);

    /*! @brief ContractToken 提取函数
     @param owner 提现账号
     @param contract 合约名
     @param quantity 提现通证数量
     @param memo 备注
     */
    void ctxextract(account_name owner, extended_asset quantity, string memo);

    /*! @brief ContractToken 转账函数
     @param contract 合约名
     @param from 发送账号
     @param to 接收账号
     @param quantity 转账通证数量
     @param memo 备注
     */
    void ctxtransfer(account_name from, account_name to, extended_asset quantity, string memo);

public:
    /*! @brief 设置开关仓状态
     @param sym 需要操作的通证
     @param position 开关仓状态
     @param memo 备注
    */
    void setposition(extended_symbol sym, bool position, string memo);

public:
    /*! @brief uniswap中充值
     @param owner 加仓账户
     @param token_x  加仓的 x 通证数
     @param token_y  加仓的 y 通证数
    */
    void addreserves(account_name owner, extended_asset token_x, extended_asset token_y);

    /*! @brief uniswap中提取
     @param owner 提取账户
     @param x  提取 x 的通证类型
     @param y  提取 y 的通证类型
     @param rate 提取比例
    */
    void outreserves(account_name owner, extended_symbol x, extended_symbol y, double rate);

    /*! @brief 撤单
     @param owner 撤单账户
     @param x  提取 x 的通证类型
     @param y  提取 y 的通证类型
     @param bid_id 挂单 ID
    */
    void withdraw(account_name owner, extended_symbol x, extended_symbol y, uint64_t bid_id);

    /*! @brief 底仓加锁
     @param owner 加锁用户
     @param x  加锁的 x 的通证类型
     @param y  加锁的 y 的通证类型
    */
    void lockreserve(account_name owner, extended_symbol x, extended_symbol y);

    /*! @brief 底仓解锁
     @param owner 解锁用户
     @param x  解锁的 x 的通证类型
     @param y  解锁的 y 的通证类型
    */
    void unlckreserve(account_name owner, extended_symbol x, extended_symbol y);

private:
    bool check_bancor_pair(const extended_symbol& x, const extended_symbol& y);
    void bancororder(account_name owner, extended_asset quantity, extended_symbol tosym);
    void uniswaporder(account_name owner, extended_asset quantity, extended_asset to, double price, account_name id, account_name rampay);
    double get_real_asset(extended_asset quantity);

    template <typename T, T (*wipe_function)(T)>
    extended_asset get_asset_by_amount(T amount, extended_symbol symbol);

    void uniswapdeal(account_name owner, extended_asset& market_from, extended_asset& market_to, extended_asset from, extended_asset to_sym, uint64_t primary, double price, account_name rampay);
    void get_order_prefix(uint64_t& primary);
    void bid_order(account_name owner, uint64_t primary, double price, extended_asset quantity, extended_asset sub_asset, account_name rampay);
    void deal_inverse_order(uint64_t market_price, uint64_t primary, account_name rampay);
    bool is_deplete(bool limit, extended_asset from, extended_asset to);

public:
    void receipt(extended_asset in, extended_asset out, extended_asset fee);
    void snapshot(account_name contract, asset max_supply, double cw, asset max_exchange, asset supply,
        asset reserve_supply, asset connector_balance, asset reserve_connector_balance,
        account_name connector_balance_issuer);
    void outreceipt(account_name owner, extended_asset x, extended_asset y);
    void traderecord(account_name owner, account_name oppo, extended_asset from, extended_asset to, extended_asset fee, uint64_t bid_id);
    void orderchange(uint64_t bid_id, uint8_t state);
    void bidrec(uint64_t price, extended_asset quantity, extended_asset filled, uint64_t bid_id);
    void pricerec(uint64_t old_price, uint64_t new_price);
    void uniswapsnap(account_name owner, extended_asset quantity);

public:
    inline asset get_supply(symbol_type sym) const;

private:
    struct account {
        uint64_t primary;
        extended_asset balance;

        uint64_t primary_key() const { return primary; }
        static uint128_t key(extended_symbol symbol)
        {
            return ((uint128_t(symbol.name()) << 64) + symbol.contract);
        }
        uint128_t get_key() const { return key(balance.get_extended_symbol()); }

        EOSLIB_SERIALIZE(account, (primary)(balance))
    };

    typedef eosio::multi_index<N(accounts), account,
        indexed_by<N(byextendedasset), const_mem_fun<account, uint128_t, &account::get_key>>>
        accounts;

    typedef eosio::multi_index<N(ctxaccounts), account,
        indexed_by<N(byextendedasset), const_mem_fun<account, uint128_t, &account::get_key>>>
        contract_accounts;

    struct lock_account : public account {
        time_point_sec lock_timestamp;

        static key256 key(extended_symbol symbol, time_point_sec lock_timestamp)
        {
            return key256::make_from_word_sequence<uint64_t>(symbol.name(), symbol.contract, uint64_t(lock_timestamp.sec_since_epoch()));
        }
        key256 get_key() const { return key(balance.get_extended_symbol(), lock_timestamp); }

        EOSLIB_SERIALIZE(lock_account, (primary)(balance)(lock_timestamp))
    };

    typedef eosio::multi_index<N(lockaccounts), lock_account,
        indexed_by<N(byextendedasset), const_mem_fun<lock_account, key256, &lock_account::get_key>>>
        lock_accounts;

    struct currency_stats {
        account_name issuer; // 发行者
        asset max_supply; // 最大可发行通证数量
        double connector_weight = 0; // 连接器权重

        asset max_exchange; // 最大可兑换(流通)的通证数量
        asset supply; // 当前流通通证数量
        asset reserve_supply; // 未流通通证数量

        asset connector_balance; // 保证金通证数量
        asset reserve_connector_balance; // 未流通通证保证金数量
        account_name connector_balance_issuer; // 准备金发行方

        double buy_fee; // 入场手续费
        double sell_fee; // 出场手续费
        bool position; // 锁仓状态

        uint64_t primary_key() const { return supply.symbol.name(); }
        asset get_balance() const { return connector_balance + reserve_connector_balance; }
        asset get_supply() const { return supply + reserve_supply; }

        void reset_connector_weight(asset newsupply);
        void reset_connector_weight_by_connector_balance(asset newconnectbalance);

        EOSLIB_SERIALIZE(currency_stats, (supply)(max_supply)(issuer)(max_exchange)(connector_weight)(connector_balance)(reserve_supply)(reserve_connector_balance)(connector_balance_issuer)(buy_fee)(sell_fee)(position))
    };

    typedef eosio::multi_index<N(stats), currency_stats> stats;

    struct stable_price {
        uint64_t primary;
        extended_symbol symbol;
        double price;

        uint64_t primary_key() const { return primary; }
        static uint128_t key(extended_symbol symbol)
        {
            return ((uint128_t(symbol.name()) << 64) + symbol.contract);
        }
        uint128_t get_key() const { return key(symbol); }
        EOSLIB_SERIALIZE(stable_price, (primary)(symbol)(price))
    };
    typedef eosio::multi_index<N(stableprice), stable_price,
        indexed_by<N(byextendedsymbol), const_mem_fun<stable_price, uint128_t, &stable_price::get_key>>>
        stable_prices;

    struct uniswap_market {
        uint64_t primary;
        extended_asset tokenx;
        extended_asset tokeny;
        double total_weights;

        uint64_t primary_key() const { return primary; }
        static key256 key(extended_symbol symbolx, extended_symbol symboly)
        {
            if (symbolx < symboly) {
                auto swap = symbolx;
                symbolx = symboly;
                symboly = swap;
            }
            return key256::make_from_word_sequence<uint64_t>(symbolx.name(), symbolx.contract, symboly.name(), symboly.contract);
        }
        key256 get_key() const { return key(tokenx.get_extended_symbol(), tokeny.get_extended_symbol()); }
        EOSLIB_SERIALIZE(uniswap_market, (primary)(tokenx)(tokeny)(total_weights))
    };
    typedef eosio::multi_index<N(swapmarket), uniswap_market,
        indexed_by<N(bysymbol), const_mem_fun<uniswap_market, key256, &uniswap_market::get_key>>>
        swap_market;

    struct market_pool {
        account_name owner;
        double weights;

        uint64_t primary_key() const { return owner; }
        EOSLIB_SERIALIZE(market_pool, (owner)(weights))
    };
    typedef eosio::multi_index<N(swappool), market_pool> swap_pool;

    struct uniswap_order {
        uint64_t bid_id;
        account_name owner;
        uint64_t price;
        extended_asset quantity;
        extended_asset filled;

        uint64_t primary_key() const { return bid_id; }

        uint64_t get_lower() const { return price; }
        static uint128_t key3(account_name owner, uint64_t price)
        {
            return ((uint128_t(owner) << 64) + price);
        }
        uint128_t get_owner() const { return key3(owner, price); }

        EOSLIB_SERIALIZE(uniswap_order, (bid_id)(owner)(price)(quantity)(filled))
    };
    typedef eosio::multi_index<N(swaporder), uniswap_order,
        indexed_by<N(bylowerprice), const_mem_fun<uniswap_order, uint64_t, &uniswap_order::get_lower>>,
        indexed_by<N(byowner), const_mem_fun<uniswap_order, uint128_t, &uniswap_order::get_owner>>>
        swap_order;

    struct unlock_request {
        account_name owner;
        time request_time;

        uint64_t primary_key() const { return owner; }
        EOSLIB_SERIALIZE(unlock_request, (owner)(request_time))
    };
    typedef eosio::multi_index<N(swaplock), unlock_request> swap_lock;

    struct bid_id_args {
        account_name owner;
        extended_asset quantity;
        uint64_t price;
        uint64_t primary;
        time now;
    };

private:
    inline static account_name get_foundation(account_name issuer)
    {
        return issuer == eos_account ? N(fibos) : issuer;
    }

    void sub_balance(account_name owner, extended_asset value);
    void add_balance(account_name owner, extended_asset value, account_name ram_payer);

    void contract_sub_balance(account_name owner, extended_asset value);
    void contract_add_balance(account_name owner, extended_asset value, account_name ram_payer);

    void lock_sub_balance(account_name owner, extended_asset value, time_point_sec lock_timestamp);
    void lock_sub_balance(account_name foundation, extended_asset quantity, bool recur = false);
    void lock_add_balance(account_name owner, extended_asset value, time_point_sec lock_timestamp, account_name ram_payer);

    extended_asset buy(account_name rampay, extended_asset quantity, extended_symbol to);
    extended_asset sell(account_name rampay, extended_asset quantity);
    extended_asset unlock_with_connector(extended_asset quantity, bool isExec);
    extended_asset unlock_with_connector(extended_asset connector, extended_asset token);
    extended_asset unlock_with_locked_tokens(extended_asset quantity);

    extended_asset get_balance(extended_asset quantity, account_name name);

    int64_t bancor_sell_by_tokens(int64_t s, int64_t r, int64_t s_lock, int64_t r_lock, double cw, int64_t t);
    double get_stable_price(extended_symbol symbol, double balance, double supply, account_name rampay);
};

asset token::get_supply(symbol_type sym) const
{
    stats statstable(_self, eos_account);
    const auto& st = statstable.get(symbol_type(CORE_SYMBOL).name());
    return st.get_supply();
}

template <typename T>
checksum256 sha256(const T& value)
{
    auto digest = pack(value);
    checksum256 hash;
    ::sha256(digest.data(), uint32_t(digest.size()), &hash);
    return hash;
}
} // namespace eosio