let test = require('test');
test.setup();

let test_util = require('../test_util');

test_util.runBIOS();
let fmt = test_util.fmt;
let checkaccount = test_util.checkaccount;
let fmtDate = test_util.fmtDate;
let checkunswapmarket = test_util.checkunswapmarket;
let checkmarketpool = test_util.checkmarketpool;
let checkmarketorder = test_util.checkmarketorder;
let now;
var users = {};

describe(`order from pass`, () => {
    let symbol = "AAA";
    let contract = "user1"
    let fibos, ctx, name, name1;
    let precision = 4;

    before(() => {
        fibos = test_util.getFIBOS();
        name = contract;
        if (name !== 'eosio' && name !== 'fibos' && !users[name]) {
            users[name] = true;
            test_util.user(fibos, name);
        }
        name1 = test_util.user(fibos);
        ctx = fibos.contractSync("eosio.token");

        ctx.createSync("eosio", "50000000.0000 EOS", {
            authorization: "eosio"
        });
        ctx.setpositionSync(fmt(1000000000, 4, "EOS", "eosio"), 1, "setposition", {
            authorization: "fibos"
        });
        ctx.excreateSync("eosio", `10000000.0000 FO`, 0, `10000000.0000 FO`, `100.0000 FO`, `10000.0000 EOS`, 0, 0, 0, 'eosio', {
            authorization: "eosio"
        });
        ctx.setpositionSync(fmt(1000000000, 4, "FO", "eosio"), 1, "setposition", {
            authorization: "fibos"
        });
        ctx.issueSync("fibos", `10000000.0000 EOS`, `issue 100.0000 EOS`, {
            authorization: "eosio"
        });
        ctx.issueSync("fibos", `1000000.0000 FO`, `issue 100.0000 FO`, {
            authorization: "eosio"
        });
        let transfer_amount = "10000.0000";
        ctx.extransferSync("fibos", name, `${transfer_amount} FO@eosio`, `exchange FO to ${symbol}@${name}`, {
            authorization: "fibos"
        });
        ctx.extransferSync("fibos", name, `${transfer_amount} EOS@eosio`, `exchange FO to ${symbol}@${name}`, {
            authorization: "fibos"
        });
        now = fmtDate();
        ctx.excreateSync(name, fmt(100000000000, precision, symbol), 0, fmt(100000000000, precision, symbol), fmt(300000000, precision, symbol), fmt(1000000, 4, "FO"), now, 0, 0, 'eosio', {
            authorization: name
        });
        ctx.setpositionSync(fmt(1000000000, 4, symbol, name), 1, "setposition", {
            authorization: name
        });

        ctx.exissueSync(name, `1000000.0000 AAA@${name}`, `issue 1000000.0000 AAA`, {
            authorization: name
        });
    });

    it(`create market`, () => {
        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9990000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "990000.0000 FO",
            "contract": "eosio"
        });
        ctx.addreservesSync("fibos", "10000.0000 EOS@eosio", "1000.0000 FO@eosio", {
            authorization: "fibos"
        });
        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10000.0000 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "1000.0000 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })
        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9980000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989000.0000 FO",
            "contract": "eosio"
        });
        checkmarketpool(fibos, "fibos", 0, {
            "owner": "fibos",
            "weights": "10000.00000000000000000"
        });
    });

    it(`??????fibos???????????????????????????11????????????10000???FO ????????????????????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "10000.0000 EOS@eosio", "0.0000 FO@eosio", 11, "test", "memo", {
                authorization: name
            });
        });
    });

    it(`??????fibos???????????????????????????11????????????10000???FO??? to??????0?????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "10000.0000 EOS@eosio", "10.0000 FO@eosio", 11, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos????????????????????????from???0???to???0?????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "0.0000 EOS@eosio", "0.0000 FO@eosio", 11, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos????????????????????????from???0???to???0???price???0?????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "0.0000 EOS@eosio", "0.0000 FO@eosio", 0, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos????????????????????????from,to??????0???price???0?????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "10000.0000 EOS@eosio", "100.0000 FO@eosio", 0, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos???????????????????????????11????????????10000???FO???from?????????????????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "10000.000 EOS@eosio", "0.0000 FO@eosio", 11, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos???????????????????????????11????????????10000???FO???to?????????????????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "10000.0000 EOS@eosio", "0.000 FO@eosio", 11, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos???????????????????????????11????????????10000???FO???price?????????????????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "10000.0000 EOS@eosio", "0.000 FO@eosio", 11.000001, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos???????????????????????????11????????????10000???FO???from??????????????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "-10000.0000 EOS@eosio", "0.0000 FO@eosio", 11, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos???????????????????????????11????????????10000???FO???price??????????????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "10000.0000 EOS@eosio", "0.0000 FO@eosio", -11, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos???????????????????????????11????????????10000???FO???from???price??????????????????????????? `, () => {
        assert.throws(() => {
            ctx.exchangeSync("fibos", "-10000.0000 EOS@eosio", "0.0000 FO@eosio", -11, "test", "memo", {
                authorization: "fibos"
            });
        });
    });

    it(`??????fibos???????????????????????????11????????????1000???EOS ,?????????????????????????????????????????????????????????`, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9980000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989000.0000 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10000.0000 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "1000.0000 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })
        let r = ctx.exchangeSync("fibos", "1000.0000 EOS@eosio", "0.0000 FO@eosio", 11, "test", "memo", {
            authorization: "fibos"
        });

        assert.deepEqual(r.processed.action_traces[0].inline_traces[1].act.data, {
            "owner": "fibos",
            "oppo": "eosio",
            "from": {
                "quantity": "488.0885 EOS",
                "contract": "eosio"
            },
            "to": {
                "quantity": "46.3977 FO",
                "contract": "eosio"
            },
            "fee": {
                "quantity": "0.1397 FO",
                "contract": "eosio"
            },
            "bid_id": 0
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10488.0885 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "953.6023 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9979000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989046.3977 FO",
            "contract": "eosio"
        });

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

    });

    it(`??????name???????????????????????????0.092????????????46.5375???FO ,???????????????????????????????????????????????????????????????????????????????????? `, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9979000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989046.3977 FO",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "10000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "10000.0000 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10488.0885 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "953.6023 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })
        ctx.exchangeSync(name, "46.5375 FO@eosio", "0.0000 EOS@eosio", 0.0990, "test2", "memo2", {
            authorization: name
        });

        let r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10489.1124 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "953.6489 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9979000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989092.8886 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "10510.8876 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9953.4625 FO",
            "contract": "eosio"
        });

    });

    it(`??????fibos???????????????????????????11????????????1000???EOS ,????????????????????????????????????????????????????????????11?????????????????????????????????????????????????????????`, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9979000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989092.8886 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10489.1124 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "953.6489 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })


        let r = ctx.exchangeSync("fibos", "1000.0000 EOS@eosio", "0.0000 FO@eosio", 11, "test3", "memo3", {
            authorization: "fibos"
        });

        assert.deepEqual(r.processed.action_traces[0].inline_traces[1].act.data, {
            "owner": "fibos",
            "oppo": "eosio",
            "from": {
                "quantity": "0.5127 EOS",
                "contract": "eosio"
            },
            "to": {
                "quantity": "0.0464 FO",
                "contract": "eosio"
            },
            "fee": {
                "quantity": "0.0002 FO",
                "contract": "eosio"
            },
            "bid_id": 0
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10489.6251 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "953.6025 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9978000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989092.9350 FO",
            "contract": "eosio"
        });

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

    });

    it(`??????name???????????????????????????0.0959????????????200???FO ,???????????????????????????????????????????????????????????????????????????????????????????????????????????????uniswap `, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9978000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989092.9350 FO",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "10510.8876 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9953.4625 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10489.6251 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "953.6025 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        let r = ctx.exchangeSync(name, "200.0000 FO@eosio", "0.0000 EOS@eosio", 0.0959, "test2", "memo2", {
            authorization: name
        });

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10215.8533 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "979.5199 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9978000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989183.7066 FO",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "11784.1467 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9753.4625 FO",
            "contract": "eosio"
        });

    });

    it(`??????name???????????????????????????0.0959????????????100???FO ,??????????????????????????????????????????????????????????????????0.0959???????????????????????????uniswap??????????????????????????????????????????????????????????????? `, () => {

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "11784.1467 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9753.4625 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10215.8533 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "979.5199 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        let r = ctx.exchangeSync(name, "100.0000 FO@eosio", "0.0000 EOS@eosio", 0.0959, "test3", "memo3", {
            authorization: name
        });

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10214.9154 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "979.6101 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "11785.0846 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9653.4625 FO",
            "contract": "eosio"
        });

    });

    it(`??????name???????????????????????????0.0969????????????100???FO ,??????????????????????????????????????????????????????????????????0.0959????????????????????????uniswap?????????????????????0.0969???????????????????????????????????????????????????????????????????????????????????? `, () => {

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "11785.0846 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9653.4625 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10214.9154 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "979.6101 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        let r = ctx.exchangeSync(name, "100.0000 FO@eosio", "0.0000 EOS@eosio", 0.0969, "test3", "memo3", {
            authorization: name
        });

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10162.2273 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "984.7045 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "11837.7727 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9553.4625 FO",
            "contract": "eosio"
        });

    });

    it(`??????fibos???????????????????????????11????????????1000???EOS ,?????????????????????????????????????????????????????????????????????uniswap?????????????????????????????????????????????????????????????????????????????????`, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9978000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989183.7066 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "11837.7727 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9553.4625 FO",
            "contract": "eosio"
        });

        let r = checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10162.2273 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "984.7045 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        r = ctx.exchangeSync("fibos", "1000.0000 EOS@eosio", "0.0000 FO@eosio", 11, "test4", "memo4", {
            authorization: "fibos"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10183.7888 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "982.9101 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9977000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989280.4066 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "12816.2112 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9553.4625 FO",
            "contract": "eosio"
        });


    });

    it(`??????fibos???????????????????????????0.0969????????????100???FO ,?????????????????????????????????????????????????????????????????????????????????????????????????????????user??????????????????fibos????????? `, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9977000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989280.4066 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "12816.2112 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9553.4625 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10183.7888 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "982.9101 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        let r = ctx.exchangeSync("fibos", "100.0000 FO@eosio", "0.0000 EOS@eosio", 0.0964, "test3", "memo3", {
            authorization: "fibos"
        });

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10183.7888 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "982.9101 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9977000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989180.4066 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "12816.2112 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9553.4625 FO",
            "contract": "eosio"
        });

    });

    it(`??????fibos???????????????????????????11????????????3000???EOS ,????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????uniswap???`, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9977000.0000 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989180.4066 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "12816.2112 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9553.4625 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10183.7888 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "982.9101 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        r = ctx.exchangeSync("fibos", "3000.0000 EOS@eosio", "0.0000 FO@eosio", 11, "test4", "memo4", {
            authorization: "fibos"
        });
        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10238.8522 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "978.4874 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9975036.3070 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989468.0501 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "14724.8408 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9553.4625 FO",
            "contract": "eosio"
        });
    });

    it(`??????name???????????????????????????0.0954????????????100???FO ,??????????????????????????????????????? `, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9975036.3070 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989468.0501 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "14724.8408 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9553.4625 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10238.8522 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "978.4874 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        let r = ctx.exchangeSync(name, "100.0000 FO@eosio", "0.0000 EOS@eosio", 0.0954, "test3", "memo3", {
            authorization: name
        });

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10238.8522 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "978.4874 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9975036.3070 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989468.0501 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "14724.8408 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9453.4625 FO",
            "contract": "eosio"
        });

    });

    it(`??????name???????????????????????????0.0956????????????100???FO ,???????????????????????????????????????????????????uniswap????????????????????? `, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9975036.3070 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989468.0501 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "14724.8408 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9453.4625 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10238.8522 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "978.4874 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        let r = ctx.exchangeSync(name, "100.0000 FO@eosio", "0.0000 EOS@eosio", 0.0956, "test4", "memo4", {
            authorization: name
        });

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10237.0434 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "978.6608 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9975036.3070 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989468.0501 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "14726.6496 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9353.4625 FO",
            "contract": "eosio"
        });

    });

    it(`??????fibos???????????????????????????10.50????????????3000???EOS ,????????????????????????????????????????????????????????????????????????????????????????????????????????????uniswap??????????????????????????????????????????`, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9975036.3070 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989468.0501 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "14726.6496 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9353.4625 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10237.0434 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "978.6608 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        r = ctx.exchangeSync("fibos", "3000.0000 EOS@eosio", "0.0000 FO@eosio", 10.50, "test5", "memo5", {
            authorization: "fibos"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10258.5651 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "977.2122 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9972036.3070 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989669.3253 FO",
            "contract": "eosio"
        });

        checkaccount(fibos, name, "EOS", "eosio", {
            "quantity": "16816.9864 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, name, "FO", "eosio", {
            "quantity": "9353.4625 FO",
            "contract": "eosio"
        });


    });

    it(`??????fibos???????????????`, () => {

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9972036.3070 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989669.3253 FO",
            "contract": "eosio"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10258.5651 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "977.2122 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        let r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        let bid_id = r.rows[0].bid_id;

        r = ctx.withdrawSync("fibos", "0.0000 FO@eosio", "0.0000 EOS@eosio", bid_id, {
            authorization: "fibos"
        });

        checkunswapmarket(fibos, "FO@eosio", "EOS@eosio", {
            "primary": 0,
            "tokenx": {
                "quantity": "10258.5651 EOS",
                "contract": "eosio"
            },
            "tokeny": {
                "quantity": "977.2122 FO",
                "contract": "eosio"
            },
            "total_weights": "10000.00000000000000000"
        })

        r = fibos.getTableRowsSync(true, "eosio.token", "18374686479671623680", "swaporder");

        r = fibos.getTableRowsSync(true, "eosio.token", 0, "swaporder");

        checkaccount(fibos, "fibos", "EOS", "eosio", {
            "quantity": "9972924.4485 EOS",
            "contract": "eosio"
        });
        checkaccount(fibos, "fibos", "FO", "eosio", {
            "quantity": "989669.3253 FO",
            "contract": "eosio"
        });
    });
});

require.main === module && test.run(console.DEBUG);