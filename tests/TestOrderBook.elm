module TestOrderBook exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import OrderBook
import Test exposing (..)


suite : Test
suite =
    describe "OrderBook"
        [ describe "Empty OrderBook"
            [ test "No events" <|
                \() ->
                    OrderBook.empty
                        |> OrderBook.events
                        |> List.length
                        |> Expect.equal 0
            , fuzz2 int int "Market buy creates no event" <|
                \i j ->
                    OrderBook.empty
                        |> OrderBook.buy { trader = i, quantity = j, price = Nothing }
                        |> OrderBook.events
                        |> List.length
                        |> Expect.equal 0
            , fuzz2 int int "Market sell creates no event" <|
                \i j ->
                    OrderBook.empty
                        |> OrderBook.sell { trader = i, quantity = j, price = Nothing }
                        |> OrderBook.events
                        |> List.length
                        |> Expect.equal 0
            , fuzz2 int int "Market buy creates no buy liquidity" <|
                \i j ->
                    OrderBook.empty
                        |> OrderBook.buy { trader = i, quantity = j, price = Nothing }
                        |> OrderBook.buyDepth
                        |> Expect.equal 0
            , fuzz2 int int "Market buy creates no sell liquidity" <|
                \i j ->
                    OrderBook.empty
                        |> OrderBook.buy { trader = i, quantity = j, price = Nothing }
                        |> OrderBook.sellDepth
                        |> Expect.equal 0
            , fuzz2 int int "Market sell creates no sell liquidity" <|
                \i j ->
                    OrderBook.empty
                        |> OrderBook.sell { trader = i, quantity = j, price = Nothing }
                        |> OrderBook.sellDepth
                        |> Expect.equal 0
            , fuzz2 int int "Market sell creates no buy liquidity" <|
                \i j ->
                    OrderBook.empty
                        |> OrderBook.sell { trader = i, quantity = j, price = Nothing }
                        |> OrderBook.buyDepth
                        |> Expect.equal 0
            , fuzz3 int int int "Limit sell creates no event" <|
                \i j k ->
                    OrderBook.empty
                        |> OrderBook.sell { trader = i, quantity = j, price = Just k }
                        |> OrderBook.events
                        |> List.length
                        |> Expect.equal 0
            , fuzz3 int int int "Limit buy creates no event" <|
                \i j k ->
                    OrderBook.empty
                        |> OrderBook.buy { trader = i, quantity = j, price = Just k }
                        |> OrderBook.events
                        |> List.length
                        |> Expect.equal 0
            , fuzz3 int int int "Limit buy creates buy liquidity" <|
                \i j k ->
                    OrderBook.empty
                        |> OrderBook.buy { trader = i, quantity = j, price = Just k }
                        |> OrderBook.buyDepth
                        |> Expect.equal 1
            , fuzz3 int int int "Limit buy creates no sell liquidity" <|
                \i j k ->
                    OrderBook.empty
                        |> OrderBook.buy { trader = i, quantity = j, price = Just k }
                        |> OrderBook.sellDepth
                        |> Expect.equal 0
            , fuzz3 int int int "Limit sell creates sell liquidity" <|
                \i j k ->
                    OrderBook.empty
                        |> OrderBook.sell { trader = i, quantity = j, price = Just k }
                        |> OrderBook.sellDepth
                        |> Expect.equal 1
            , fuzz3 int int int "Limit sell creates no buy liquidity" <|
                \i j k ->
                    OrderBook.empty
                        |> OrderBook.sell { trader = i, quantity = j, price = Just k }
                        |> OrderBook.buyDepth
                        |> Expect.equal 0
            ]
        ]
