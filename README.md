# OrderBook

[![Travis-ci](https://travis-ci.org/NeilW/elm-orderbook.svg?branch=master)](https://travis-ci.org/NeilW/elm-orderbook) │ [![Fossa Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2FNeilW%2Felm-orderbook.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2FNeilW%2Felm-orderbook?ref=badge_shield)
A simple OrderBook data type supporting Market and Limit orders on both
the Buy and Sell Side.

The data type was developed for modelling and simulation purposes. 

## Example

    import OrderBook exposing (OrderBook)

    myBook: OrderBook


## Implementation

Underneath the hood it uses a couple of priority queues implemented [using
a Heap](https://package.elm-lang.org/packages/TSFoster/elm-heap/latest/),
one for the buy side and one for the sell side, along with a
[List](https://package.elm-lang.org/packages/elm/core/latest/List)
of the traded events the book has completed.
