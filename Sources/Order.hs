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
-- >>> aboveAverage [("TLS",[6,5,5,5,5,5]),("NNT",[8,10,10,10,10,10]),("CCS",[9,8,8,8,8,8]),("HHA",[5,4,4,4,4,4]),("KKE",[6,7,7,7,7,7]),("GGA",[4,4,4,4,4,4]),("JJY",[9,6,6,6,6,6]),("NNA",[4,7,7,7,7,7])] 1.005
--[("TLS",[6.0,5.0,5.0,5.0,5.0,5.0]),("CCS",[9.0,8.0,8.0,8.0,8.0,8.0]),("HHA",[5.0,4.0,4.0,4.0,4.0,4.0]),("JJY",[9.0,6.0,6.0,6.0,6.0,6.0])]
--
-- >>> belowAverage [] [("TLS",[6,5,5,5,5,5]),("NNT",[8,10,10,10,10,10]),("CCS",[9,8,8,8,8,8]),("HHA",[5,4,4,4,4,4]),("KKE",[6,7,7,7,7,7]),("GGA",[4,4,4,4,4,4]),("JJY",[9,6,6,6,6,6]),("NNA",[4,7,7,7,7,7])]
-- []
--
-- >>> belowAverage [("TLS",18),("NNT",50),("CCS",88),("KKE",5),("GGA",100),("NNA",50)] [("TLS",[6,5,5,5,5,5]),("NNT",[8,10,10,10,10,10]),("CCS",[9,8,8,8,8,8]),("HHA",[5,4,4,4,4,4]),("KKE",[6,7,7,7,7,7]),("GGA",[4,4,4,4,4,4]),("JJY",[9,6,6,6,6,6]),("NNA",[4,7,7,7,7,7])]
--[("NNT",50),("KKE",5),("NNA",50)]
--
-- >>> makeOrders (100000,[("TLS",18),("NNT",50),("CCS",88),("KKE",5),("GGA",100),("NNA",50)]) [("TLS",[6.5,5,5.5,5,5.6,5.8]),("NNT",[8.3,10,9.4,8.6,8.5,8.8]),("CCS",[9.3,7.9,8.8,8.6,8,8]),("HHA",[5,4,4,4,4,4]),("KKE",[6,7,7,7,7,7]),("GGA",[4,4,4,4,4,4]),("JJY",[9,6,6,6,6,6]),("NNA",[8,9,9,9,9,9]),("GGG",[9,10,10,10,10,10]),("YYY",[5,6.5,7.8,7.7,7.4,4.3]),("KKK",[4,7,7,7,7,7]),("NNG",[9,7.5,7,7,7,7])]
--[Order "NNT" (-50),Order "KKE" (-5),Order "NNA" (-50),Order "TLS" 2849,Order "NNT" 2849,Order "CCS" 2849,Order "HHA" 2849,Order "KKE" 2849]
--
-- >>> makeOrders (5000000,[]) [("TLS",[6.5,5,5.5]),("NNT",[8.3,10,9.4]),("CCS",[9.3,7.9,8.8]),("HHA",[5,4,4]),("KKE",[6,7,7]),("GGA",[4,4,4]),("JJY",[9,6,6]),("NNA",[8,9,9]),("GGG",[9,10,10]),("YYY",[5,6.5,7.8]),("KKK",[4,7,7]),("NNG",[9,7.5,7])]
--[]


--makeOrders obtains a list of buying and selling order
makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders (cash, holds) history =
    case (belowAverage holds history) of
        x:xs -> [Order (fst x) (-(snd x))] ++ makeOrders (cash, xs) history
        []
         |(aboveAverage history 0.05)==[]->[]
         |otherwise -> makeOrders2 (cash, holds) history (aboveAverage history 0.05)

--makeOrders2 obtains a list of buying orders by recursion and it supports the makeOrders function
makeOrders2 :: Portfolio -> [StockHistory] ->[StockHistory]-> [Order]
makeOrders2 (cash, holds) history a = case a of
         []->[]
         c:cs ->[Order (fst c) (quantity cash history)] ++ makeOrders2 (cash, holds) history cs


--average obtains the past 5-day average price of one stock
average :: Stock -> [StockHistory] -> Price
average s histories = (sum (take 5 (tail (snd getStock)))) / 5
    where
        getStock = head $ filter (\x -> fst x == s) histories

--aboveAverage checks if the prices of stocks shows an upward trend
-- and picks out the first 5 stocks which have increasing prices
aboveAverage :: [StockHistory] -> Double -> [StockHistory]
aboveAverage stocks rate
    | stocks == [] = []
    | getdays < 5 =[]
    | otherwise = take 5 (filter (\x -> getStockPrice (fst x) stocks > (average (fst x) stocks) * rate) stocks)
    where
        getdays= length(snd (head stocks))

--belowAverage checks if the prices of stocks in holding shows an downward trend
-- and picks out all of stocks which have decreasing prices
belowAverage :: Holdings -> [StockHistory] -> Holdings
belowAverage holds stocks
    | stocks ==[]= []
    | holds == [] = []
    | getdays < 5 =[]
    | otherwise = filter (\x -> getStockPrice (fst x) stocks < (average (fst x) stocks)) holds
    where
        getdays= length(snd (head stocks))

--quantity calculates the amount of a stock one is going to buy
quantity :: Cash -> [StockHistory] -> Quantity
quantity cash stocks = floor(cash / (sum (getPriceSum (aboveAverage stocks 0.05))))

--getPriceSum obtains the list of the current price of the stocks one want to buy
getPriceSum:: [StockHistory] -> [Price]
getPriceSum history = case (aboveAverage history 0.05) of
            []->[]
            x:xs-> [getStockPrice (fst x) history]++getPriceSum xs

--getStockPrice calculate the stock price
getStockPrice :: Stock -> [StockHistory] -> Price
getStockPrice s histories = head $ snd getStock
    where
        getStock = head $ filter (\x -> fst x == s) histories