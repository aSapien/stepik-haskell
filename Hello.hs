
module Hello where

import Data.Char

-- lenVec3 :: x -> y -> z -> Float
lenVec3 x y z = sqrt (x ** 2 + y ** 2 + z ** 2)

sign num = 
    if num < 0
    then (-1)
    else if num > 0
    then 1
    else 0

(|-|) a b = abs (a - b)

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: (Double -> Double)
standardDiscount = discount 1000 5 

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = 
        if isDigit x && isDigit y
        then 10 * digitToInt x + digitToInt y
        else 100
        

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = 
    sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2
    where
    x1 = fst (p1)
    y1 = snd (p1)
    x2 = fst (p2)
    y2 = snd (p2)

factorialD 0 = 1
factorialD (-1) = 1
factorialD x = x * factorialD (x - 2)

fibonacci :: Integer -> Integer
fibonacci n 
    | n == 0    = 0
    | n == 1    = 1
    | n == (-1) = 1
    | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
    | n < (-1)  = let 
                    positiveIndex = (-n)
                    sign = (-1) ^ (positiveIndex + 1)
                  in
                    sign * fibonacci positiveIndex


fibonacci' :: Integer -> Integer
fibonacci' n 
    | n == 0    = 0
    | n == 1    = 1
    | n == (-1) = 1
    | n > 1     = helper n 1 0 1
    | n < (-1)  = let 
                    positiveIndex = (-n)
                    sign = (-1) ^ (positiveIndex + 1)
                  in
                    sign * helper positiveIndex 1 0 1

--  2 1 0 1
helper desiredIdx prev beforePrev idx
    | idx == (desiredIdx - 1) = prev + beforePrev
    | idx < desiredIdx =
        helper desiredIdx (prev + beforePrev) prev (idx + 1)


seqA :: Integer -> Integer
seqA requestedIdx = seqAggregate 1 2 3 0
    where
        seqAggregate :: Integer -> Integer -> Integer -> Integer -> Integer
        seqAggregate a0 a1 a2 currentIdx 
                | currentIdx < requestedIdx =
                    let 
                        a3next = a1 + a2 - 2 * a0
                    in
                        seqAggregate a1 a2 a3next (currentIdx + 1)

                | otherwise                 = a0



sum'n'count :: Int -> (Int, Int)
sum'n'count x = 
    let 
        -- numAsString = show x
        -- count = length $ numAsString
        asDigitsList :: (Int, Int) -> ([Int], Int)
        asDigitsList (num, cnt)
            | num > 0   = 
                let 
                    (remainingList, cntSoFar) = asDigitsList (rest, cnt)
                in
                    (digit: remainingList, cntSoFar + 1)
            | otherwise = ([], 0)
            where
                digit = num `mod` 10
                rest = floor (fromIntegral (num `div` 10))
                
        (digilist, sum) = asDigitsList (x, 0)
    in
        (foldl (+) 0 $ digilist, sum)


{- 
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b 
    | a > b     = calcArea a b
    | otherwise = calcArea b a
    where
        interval = abs (b - a) / 100000
        calcArea = areaBetweenIntervals f interval


areaBetweenIntervals f ival endPoint startPoint
    | startPoint >= endPoint    = 0
    | heightDiff < 0            = area height2
    | otherwise                 = area height1
    where
        height1 = f startPoint
        height2 = f (startPoint + ival)
        heightDiff = height2 - height1
        -- triangle = heightDiff * ival / 2
        area h = ival * h + areaBetweenIntervals f ival endPoint (startPoint + ival) -- + triangle
        
-}

integration :: (Double -> Double) -> Double -> Double -> Double
integration func a b =
    calcArea a b
    -- | a > b     = calcArea a b
    -- | otherwise = calcArea b a
    where
        calcArea            = integralArea 1000000
        sumAreas stepsCount start end = 
            let
                interval = (end - start) / stepsCount
                areasList   = map (\idx -> func (interval * idx + start)) [1 .. stepsCount]
            in
                foldl (+) 0 areasList

        integralArea steps start end = (end - start) / steps * ((func a + func b) / 2 + (sumAreas steps start end))