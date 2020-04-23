module OrderBook exposing
    ( OrderBook, OrderRequest, Event
    , empty
    , buy, sell
    , events, sellDepth, buyDepth
    )

{-| A simple OrderBook data type supporting Market and Limit orders on both
the Buy and Sell Side.

The data type was developed for modelling and simulation purposes.

The active order is the request supplied with the command to 'buy' or
'sell', and the passive order is the one that maybe sat on the book
awaiting a match.

Limit order requests specify a price, and will be queued as passive
orders if they do not result in an immediate match. This adds liquidity
to the book. Limit Orders may be partially filled and partially queued.

Market order requests do not specify a price and will be matched against
any quantity available on the book. This removes liquidity from the book
and if there is insufficient the order may be short filled.


# Types

@docs OrderBook, OrderRequest, Event


# Create

@docs empty


# Trade

@docs buy, sell


# Queries

@docs events, sellDepth, buyDepth

-}

import Heap exposing (Heap)


{-| A simple OrderBook
-}
type OrderBook
    = OrderBook
        --Completed Order
        { events :
            List Event

        --Bid
        , buySide :
            Heap Order

        -- Ask
        , sellSide :
            Heap Order
        }


{-| An entry in the order book representing a request for trade at a price
-}
type alias Order =
    { trader : Int
    , quantity : Int
    , price : Int
    }


{-| A request to create an order, either at a specified price, or a price to be determined
-}
type alias OrderRequest =
    { trader : Int
    , quantity : Int
    , price : Maybe Int
    }


{-| The recorded value of the trade
-}
type alias Event =
    { buyer : Int
    , seller : Int
    , price : Int
    , quantity : Int
    }



-- FIXME: Do we want biggest quantity on the sell side?


{-| Return an Empty OrderBook with an empty Event list, where the buy
side proritises the highest price and the biggest quantity and the sell
side prioritises the lowest price and the smallest quantiy
-}
empty : OrderBook
empty =
    OrderBook
        { events = []
        , buySide = Heap.empty (Heap.biggest |> Heap.by .price |> Heap.thenBy .quantity)
        , sellSide = Heap.empty (Heap.smallest |> Heap.by .price |> Heap.thenBy .quantity)
        }



-- FAKE TYPECLASS


type alias SideConfig =
    { getpassiveQueue : OrderBook -> Heap Order
    , maybeFillable : OrderRequest -> ( Order, Heap Order ) -> Maybe ( Order, Heap Order )
    , updatePassiveQueue : OrderBook -> Event -> Heap Order -> OrderBook
    , createEvent : OrderRequest -> Order -> Event
    , maybeCreateLimitOrderFrom : OrderRequest -> OrderBook -> OrderBook
    }



--BID SIDE


{-| Buy a quantity via the book

If no price is supplied then the order takes the best price on the
book. This removes liquidity from the book and is resolved immediately.

If a price is supplied the book limits the order to that price and
queues the order if it can't be filled immediately. This potentially
adds liquidity to the book and is resolved in the future.

-}
buy : OrderRequest -> OrderBook -> OrderBook
buy =
    let
        config : SideConfig
        config =
            { getpassiveQueue = buyPassiveQueue
            , maybeFillable = buyMaybeFillable
            , updatePassiveQueue = buyUpdatePassiveQueue
            , createEvent = createBuyEvent
            , maybeCreateLimitOrderFrom = maybeAddLimit addBuyLimit
            }
    in
    processRequest config


{-| Update the event list and the sell side queue with the new details
-}
buyUpdatePassiveQueue : OrderBook -> Event -> Heap Order -> OrderBook
buyUpdatePassiveQueue (OrderBook book) currentEvent newQ =
    OrderBook { book | events = currentEvent :: book.events, sellSide = newQ }


{-| The passive Queue is the sell side when buying
-}
buyPassiveQueue : OrderBook -> Heap Order
buyPassiveQueue (OrderBook book) =
    book.sellSide


{-| If the buyer will take the sellers price return the matching order
-}
buyMaybeFillable : OrderRequest -> ( Order, Heap Order ) -> Maybe ( Order, Heap Order )
buyMaybeFillable activeBuy ( passiveSell, newSell ) =
    if Maybe.withDefault passiveSell.price activeBuy.price >= passiveSell.price then
        Just ( passiveSell, newSell )

    else
        Nothing


{-| Add a limit order to the buy queue
-}
addBuyLimit : OrderBook -> Order -> OrderBook
addBuyLimit (OrderBook book) order =
    OrderBook { book | buySide = Heap.push order book.buySide }


{-| Fix the price of the buy order, and create a trade event
-}
createBuyEvent : OrderRequest -> Order -> Event
createBuyEvent buyRequest sellOrder =
    createEvent (toOrder buyRequest sellOrder.price) sellOrder



-- ASK SIDE


{-| Sell a quantity via the book.

If no price is supplied then the order takes the best price on the
book. This removes liquidity from the book and is resolved immediately.

If a price is supplied the book limits the order to that price and
queues the order if it can't be filled immediately. This potentially
adds liquidity to the book and is resolved in the future.

-}
sell : OrderRequest -> OrderBook -> OrderBook
sell =
    let
        config : SideConfig
        config =
            { getpassiveQueue = sellPassiveQueue
            , maybeFillable = sellMaybeFillable
            , updatePassiveQueue = sellUpdatePassiveQueue
            , createEvent = createSellEvent
            , maybeCreateLimitOrderFrom = maybeAddLimit addSellLimit
            }
    in
    processRequest config


{-| Update the event list and the buy side queue with the new details
-}
sellUpdatePassiveQueue : OrderBook -> Event -> Heap Order -> OrderBook
sellUpdatePassiveQueue (OrderBook book) currentEvent newQ =
    OrderBook { book | events = currentEvent :: book.events, buySide = newQ }


{-| The passive Queue is the buy side when selling
-}
sellPassiveQueue : OrderBook -> Heap Order
sellPassiveQueue (OrderBook book) =
    book.buySide


{-| If the seller will take the buyers price return the matching order
-}
sellMaybeFillable : OrderRequest -> ( Order, Heap Order ) -> Maybe ( Order, Heap Order )
sellMaybeFillable activeSell ( passiveBuy, newBuy ) =
    if passiveBuy.price >= Maybe.withDefault passiveBuy.price activeSell.price then
        Just ( passiveBuy, newBuy )

    else
        Nothing


{-| Add a limit order to the sell queue
-}
addSellLimit : OrderBook -> Order -> OrderBook
addSellLimit (OrderBook book) order =
    OrderBook { book | sellSide = Heap.push order book.sellSide }


{-| Fix the price of the sell order, and create a trade event
-}
createSellEvent : OrderRequest -> Order -> Event
createSellEvent sellRequest buyOrder =
    createEvent buyOrder (toOrder sellRequest buyOrder.price)



-- QUERIES


{-| Obtain the list of trades executed on the book
-}
events : OrderBook -> List Event
events (OrderBook book) =
    book.events


{-| The amount of orders on the buy queue
-}
buyDepth : OrderBook -> Int
buyDepth =
    sellPassiveQueue >> Heap.size


{-| The amount of orders on the sell queue
-}
sellDepth : OrderBook -> Int
sellDepth =
    buyPassiveQueue >> Heap.size



-- HELPERS


{-| Take a config in lieu of typeclasses and process the request for the
side of the orderbook indicated by the config. Use tail recursion to
resolve short fills.
-}
processRequest :
    SideConfig
    -> OrderRequest
    -> OrderBook
    -> OrderBook
processRequest config activeRequest book =
    case
        config.getpassiveQueue book
            |> Heap.pop
            |> Maybe.andThen
                (config.maybeFillable
                    activeRequest
                )
    of
        Nothing ->
            config.maybeCreateLimitOrderFrom activeRequest book

        Just ( passiveOrder, newPassiveQueue ) ->
            let
                currentEvent =
                    config.createEvent activeRequest passiveOrder
            in
            if isExactFill activeRequest passiveOrder then
                newPassiveQueue
                    |> config.updatePassiveQueue book currentEvent

            else if isOverFill activeRequest passiveOrder then
                -- overfilled. Put the remainder back on the queue
                newPassiveQueue
                    |> Heap.push
                        { passiveOrder | quantity = passiveOrder.quantity - activeRequest.quantity }
                    |> config.updatePassiveQueue book currentEvent

            else
                -- short filled. Recursively fill the shortfall
                newPassiveQueue
                    |> config.updatePassiveQueue book currentEvent
                    |> processRequest config
                        { activeRequest | quantity = activeRequest.quantity - passiveOrder.quantity }


{-| Add a limit order to the book if we've been given a limit price in the request
-}
maybeAddLimit : (OrderBook -> Order -> OrderBook) -> OrderRequest -> OrderBook -> OrderBook
maybeAddLimit limitfunc request book =
    Maybe.map (toOrder request >> limitfunc book) request.price |> Maybe.withDefault book


{-| Fix the price of an order request
-}
toOrder : OrderRequest -> Int -> Order
toOrder request newPrice =
    Maybe.withDefault newPrice request.price
        |> Order request.trader request.quantity


{-| Create an event record from a buyOrder and sellOrder
-}
createEvent : Order -> Order -> Event
createEvent buyOrder sellOrder =
    { buyer = buyOrder.trader
    , seller = sellOrder.trader
    , price = min sellOrder.price buyOrder.price
    , quantity = min sellOrder.quantity buyOrder.quantity
    }



{- Queries -}


isOverFill : OrderRequest -> Order -> Bool
isOverFill active passive =
    active.quantity > passive.quantity


isExactFill : OrderRequest -> Order -> Bool
isExactFill active passive =
    active.quantity == passive.quantity
