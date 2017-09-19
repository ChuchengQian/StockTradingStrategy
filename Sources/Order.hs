module Order where

import Types

-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--
makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders (cash, holds) history =
    case (belowAverage holds history) of
        x:xs -> [Order (fst x) (-(snd x))] ++ makeOrders (cash, xs) history
        []
         |(aboveAverage history 0.05)==[]->[]
         |otherwise -> makeOrders' (cash, holds) history (aboveAverage history 0.05)


makeOrders' :: Portfolio -> [StockHistory] ->[StockHistory]-> [Order]
makeOrders' (cash, holds) history a = case a of
         []->[]
         c:cs ->[Order (fst c) (quantity cash history)] ++ makeOrders' (cash, holds) history cs



average :: Stock -> [StockHistory] -> Price
average s histories = (sum (take 5 (tail (snd getStock)))) / 5
    where
        getStock = head $ filter (\x -> fst x == s) histories


aboveAverage :: [StockHistory] -> Double -> [StockHistory]
aboveAverage stocks rate
    | stocks == [] = []
    | getdays < 5 =[]
    | otherwise = take 5 (filter (\x -> getStockPrice (fst x) stocks > (average (fst x) stocks) * rate) stocks)
    where
        getdays= length(snd (head stocks))

belowAverage :: Holdings -> [StockHistory] -> Holdings
belowAverage holds stocks
    | stocks ==[]= []
    | holds == [] = []
    | getdays < 5 =[]
    | otherwise = filter (\x -> getStockPrice (fst x) stocks < (average (fst x) stocks)) holds
    where
        getdays= length(snd (head stocks))


quantity :: Cash -> [StockHistory] -> Quantity
quantity cash stocks = floor(cash / (sum (getPriceSum (aboveAverage stocks 0.05))))

getPriceSum:: [StockHistory] -> [Price]
getPriceSum history = case (aboveAverage history 0.05) of
            []->[]
            x:xs-> [getStockPrice (fst x) history]++getPriceSum xs



getStockPrice :: Stock -> [StockHistory] -> Price
getStockPrice s histories = head $ snd getStock
    where
        getStock = head $ filter (\x -> fst x == s) histories