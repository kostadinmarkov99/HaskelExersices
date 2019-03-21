main :: IO()
main = do
 print(countElements [1, 2, 3])
 print(sumElements[1, 2, 3])
 print(elementInList 3 [2, 3, 4])
 print(listWithPrimeNumbs 2 10)
 print(removeX 5 [1, 2, 3, 4, 5, 5])
 print ([[(k,x) | k <- [2..4] ]| x <- [1..5], odd x])
 print(take 10 [2..])
 print([x | (x,i) <- zip [1,3, 5, 6, 7, 8] [0..], odd i])
 print(removeAllX 5 [1, 2, 3, 4, 5, 5])
 
 {- Зад. 1. Да се напише функция, която намира броя на елементите на списък
           с целочислени данни . -}
countElements :: [Int] -> Int
countElements[] = 0
countElements(_:xs) = 1 + countElements xs

{- Зад. 2. Да се напише функция, която намира сумата на елементите в списък 
           с целочислени данни. -}

sumElements :: [Int] -> Int
sumElements[] = 0
sumElements(x:xs) = x + sumElements xs

{- Зад. 3. Да се напише функция, която намира дали даден елемент се
           съдържа в списък с целочислени данни -}
           
elementInList :: Int -> [Int] -> Bool
elementInList n [] = False
elementInList n (x:xs) = if(x == n) then True else elementInList n xs

isPrimeSecond :: Int -> Bool
isPrimeSecond n = 2  == length [d | d <- [1..n], mod n d == 0]

{- Is a num prime -}
isPrime :: Int -> Bool
isPrime n = (n >= 2 && isPrimeHelper n 2)

isPrimeHelper :: Int -> Int -> Bool
isPrimeHelper n k
 |k^2 > n   = True
 |mod n k == 0 = False
 |otherwise = isPrimeHelper n (k + 1)


{- Зад. 4. Да се напише функция, която генерира списък с простите числа
           в интервала [a,b]. -}


listWithPrimeNumbs :: Int -> Int -> [Int]
listWithPrimeNumbs a b 
 | a > b   = []
 | isPrime(a) = a:listWithPrimeNumbs (a + 1) b
 | otherwise = listWithPrimeNumbs (a + 1) b
 
 {- Зад. 5. Да се напише функция, която премахва първия елемент равен на
           x от даден списък с целочислени данни. -}
 
removeX :: Int -> [Int] -> [Int]
removeX _ [] = []
removeX x (a:xs) = if(x /= a) then a:(removeX x xs) else xs
  
removeAllX :: Int -> [Int] -> [Int]
removeAllX _ [] = []
removeAllX x(a:as) = if(x == a) then removeAllX x as else a:(removeAllX x as) 