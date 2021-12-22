let test = require('test');
test.setup();

let test_util = require('../test_util');
test_util.runBIOS();
let fmt = test_util.fmt;
let fmtDate = test_util.fmtDate;
var nametest;
var users = {};

describe("parameter_destroy", () => {

    before(() => {
        fibos = test_util.getFIBOS();
        nametest = test_util.user(fibos);
    })

    ClassicToken('EOS', 'user1');
    ClassicToken('EOS', 'eosio');
})


function ClassicToken(symbol, contract) {
    describe(`Pass ${symbol}@${contract}`, () => {
        let fibos, ctx, name;

        before(() => {
            fibos = test_util.getFIBOS();
            name = contract;

            if (name !== 'eosio' && name !== 'fibos' && !users[name]) {
                users[name] = true;
                test_util.user(fibos, name);

            }
            ctx = fibos.contractSync("eosio.token");

            ctx.excreateSync(name, `100000000000.0000 ${symbol}`, 0, `0.0000 ${symbol}`, `0.0000 ${symbol}`, '0.0000 FO', fmtDate(), 0, 0, 'eosio', {
                authorization: name
            });
            let auth = name === "eosio" ? "fibos" : name;

            ctx.setpositionSync(fmt(1000000000, 4, symbol, name), 1, "setposition", {
                authorization: auth
            });

        });

        it('destroy asset missing contract suffix', () => {
            test_util.checkstat(fibos, name, symbol, contract, {
                "supply": `0.0000 ${symbol}`,
                "max_supply": `100000000000.0000 ${symbol}`,
                "issuer": name,
                "max_exchange": `0.0000 FO`,
                "connector_weight": "0.00000000000000000",
                "connector_balance": "0.0000 FO",
                "reserve_supply": `0.0000 ${symbol}`,
                "reserve_connector_balance": "0.0000 FO",
                "connector_balance_issuer": "eosio",
                "buy_fee": "0.00000000000000000",
                "sell_fee": "0.00000000000000000",
                "position": 1
            });

            assert.throws(() => {
                let r = ctx.exdestroySync(`0.0000 ${symbol}`, {
                    authorization: nametest
                });
            });

            test_util.checkstat(fibos, name, symbol, contract, {
                "supply": `0.0000 ${symbol}`,
                "max_supply": `100000000000.0000 ${symbol}`,
                "issuer": name,
                "max_exchange": `0.0000 FO`,
                "connector_weight": "0.00000000000000000",
                "connector_balance": "0.0000 FO",
                "reserve_supply": `0.0000 ${symbol}`,
                "reserve_connector_balance": "0.0000 FO",
                "connector_balance_issuer": "eosio",
                "buy_fee": "0.00000000000000000",
                "sell_fee": "0.00000000000000000",
                "position": 1
            });
        })
    });
}

require.main === module && test.run(console.DEBUG);