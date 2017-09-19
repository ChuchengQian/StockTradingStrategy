module Order where

import Types

-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--
-- | Stock Market
--
-- >>> average "TLS" [("TLS",[6,5,5,5,5,5]),("NNT",[8,10,10,10,10,10]),("CCS",[9,8,8,8,8,8]),("HHA",[5,4,4,4,4,4]),("KKE",[6,7,7,7,7,7]),("GGA",[4,4,4,4,4,4]),("JJY",[9,6,6,6,6,6]),("NNA",[4,7,7,7,7,7])]
-- 5.0
--
-- >>> aboveAverage [("TLS",[6,5,5,5,5,5]),("NNT",[8,10,10,10,10,10]),("CCS",[9,8,8,8,8,8]),("HHA",[5,4,4,4,4,4]),("KKE",[6,7,7,7,7,7]),("GGA",[4,4,4,4,4,4]),("JJY",[9,6,6,6,6,6]),("NNA",[4,7,7,7,7,7])] 0.05






makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders (cash, holds) history =
    case (belowAverage holds history) of
        x:xs -> [Order (fst x) (-(snd x))] ++ makeOrders (cash, xs) history
        []
         |(aboveAverage history 1.005)==[]->[]
         |otherwise -> makeOrders2 (cash, holds) history (aboveAverage history 1.005)


makeOrders2 :: Portfolio -> [StockHistory] ->[StockHistory]-> [Order]
makeOrders2 (cash, holds) history a = case a of
         []->[]
         c:cs ->[Order (fst c) (quantity cash history)] ++ makeOrders2 (cash, holds) history cs



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
--aboveAverage :: [StockHistory] -> Double -> [StockHistory]
--aboveAverage stocks rate
 --   | stocks == [] = []
 --   | getdays < 5 =[]
   -- | otherwise = take 5 (filter (\x -> getStockPrice (fst x) stocks > (average (fst x) stocks) * rate) stocks)
    --where
     --   getdays= length(snd (head stocks))

belowAverage :: Holdings -> [StockHistory] -> Holdings
belowAverage holds stocks
    | stocks ==[]= []
    | holds == [] = []
    | getdays < 5 =[]
    | otherwise = filter (\x -> getStockPrice (fst x) stocks < (average (fst x) stocks)) holds
    where
        getdays= length(snd (head stocks))


quantity :: Cash -> [StockHistory] -> Quantity
quantity cash stocks = floor(cash / (sum (getPriceSum (aboveAverage stocks 1.005))))

getPriceSum:: [StockHistory] -> [Price]
getPriceSum history = case (aboveAverage history 1.005) of
            []->[]
            x:xs-> [getStockPrice (fst x) history]++getPriceSum xs



getStockPrice :: Stock -> [StockHistory] -> Price
getStockPrice s histories = head $ snd getStock
    where
        getStock = head $ filter (\x -> fst x == s) histories