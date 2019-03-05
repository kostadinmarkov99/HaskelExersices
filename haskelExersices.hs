main :: IO()
main = do
 print "example 2"
 print "The count of digits in the num n"
 print(countDigits 55)
 print "The sum of digits of n"
 print(sumDigits 55)
 print "pow 5 2"
 print(pow 5 2)
 print "Sum digits inerative"
 print(sumDigitsIterative 56)
 print"Revercint the number 567"
 print(reverceNumber 567)
 print "Checking is the number prime 4"
 print(isPrime 4)
 print "Are the digits growing order"
 print (isAscending 456)
 print "The digit 5 in number 555 is"
 print (countOccurences 555 5)
 print "Is the number 28 perfect"
 print (isPerfectNumber 8128)
 print "The sum of prime devisors of 8 is"
 print (sumPrimeDivisors 15)
 
{-
  Зад. 1. Да се дефинира функция countDigits, която генерира линейно рекурсивен
  процес и намира броя на цифрите на дадено естествено число.
-}

countDigits :: Integer -> Integer
countDigits n = if(n < 10) then 1 else 1 + countDigits(n `div` 10)
{-
  Зад. 2. Да се дефинира функция sumDigits, която генерира линейно рекурсивен 
  процес и намира сумата от цифрите на дадено естествено число.
-}
sumDigits :: Integer -> Integer
sumDigits n = if(n < 10) then n else n `mod` 10 + sumDigits(n `div` 10)

{-Зад. 3. Да се дефинира функция pow, която генерира линейно рекурсивен процес 
  и намира x на степен n, където x е реално, а n - естествено число.
-}

pow :: Double -> Integer -> Double
pow x n = if(n == 0) then 1 else x * pow x (n - 1)

{-
  Зад. 4. Да се дефинира функция sumDigitsIterative, която генерира линейно 
  итеративен процес и намира сумата от цифрите на дадено естествено число.
-}

sumDigitsIterative :: Int -> Int
sumDigitsIterative n = sumIter n 0

sumIter :: Int -> Int -> Int
sumIter k sum = if(k < 10) then sum + k else sumIter(k `div` 10) (sum + (k `mod` 10))

{-
  Зад. 5. Да се дефинира функция reverseNumber, която генерира линейно итеративен
  процес и по дадено естествено число n намира числото, записано със същите цифри,
  но в обратен ред.
-}

reverceNumber :: Integer -> Integer
reverceNumber n = changeTheDig 0 n

changeTheDig :: Integer -> Integer -> Integer
changeTheDig sum n = if(n < 10) then (sum * 10) + n else changeTheDig((sum * 10) + (n `mod` 10)) (n `div` 10) 

{-
  Зад. 6. Да се дефинира предикат isPrime, който проверява дали дадено естествено
  число е просто.
  Забележка: Числото 1 не е нито просто, нито съставно.
-}

isPrime :: Integer -> Bool
isPrime n = (n >= 2 && isPrimeHelper n 2)

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper n k
 |k^2 > n   = True
 |mod n k == 0 = False
 |otherwise = isPrimeHelper n (k + 1)

{-
  Зад. 7. Да се напише предикат isAscending, който връща истина, ако цифрите на
  дадено естествено число са в нарастващ ред от първата към последната.
-}

isAscending :: Integer -> Bool
isAscending n 
 | (isAscendingHelper n 1 0) + 1 == isAscendingHelperCouter n    = True
 | otherwise = False

isAscendingHelper :: Integer -> Integer -> Integer ->  Integer
isAscendingHelper n i sum
 | mod n 10 > (mod (div n 10) 10) && n `div` 100 == 0   = sum + 1
 | mod n 10 < (mod (div n 10) 10) && n `div` 100 /= 0  = sum
 | mod n 10 > mod (div n (10 * i)) 10 = isAscendingHelper (n `div` 10) (i + 1) (sum + 1) 
 | otherwise = isAscendingHelper(n `div` 10) (i + 1) sum + 1
 
isAscendingHelperCouter :: Integer -> Integer 
isAscendingHelperCouter n = if(n < 10) then 1 else 1 + isAscendingHelperCouter(n `div` 10)
 
 {-
  Зад. 8. Да се напише функция countOccurences, намираща броя на срещанията на дадена
  цифра d в записа на число n.
-}

countOccurences :: Integer -> Integer -> Integer
countOccurences n dig = if(n < 10 && n == dig) then 1 else countOccurencesHelper n dig 0

countOccurencesHelper :: Integer -> Integer -> Integer -> Integer
countOccurencesHelper n dig sum 
 | n < 10 && n /= dig  = sum 
 | n < 10 && n == dig  = sum + 1
 | n `mod` 10 == dig   = countOccurencesHelper(n `div` 10) dig sum + 1   
 | otherwise  =  countOccurencesHelper(n `div` 10) dig sum
 
 {-
  Зад. 9. Да се напише предикат isPerfectNumber, който връща дали едно число е
  съвършено, т.е. равно на сумата от делите си.
-}

isPerfectNumber :: Integer -> Bool
isPerfectNumber n = isPerfectNumberHelper n 0 1 == n
 
isPerfectNumberHelper :: Integer -> Integer -> Integer -> Integer 
isPerfectNumberHelper n sum i
 | n <= i  = sum 
 | n `mod` i == 0  = isPerfectNumberHelper n (sum + i) (i + 1) 
 | otherwise = isPerfectNumberHelper n sum (i + 1)
 
 {-
  Зад. 10. Да се дефинира функция sumPrimeDivisors, която намира сумата на всички
  прости делители на едно число.
-}

 
sumPrimeDivisors :: Integer -> Integer
sumPrimeDivisors n = sumPrimeDivisorsHelper n 0 1

sumPrimeDivisorsHelper :: Integer -> Integer -> Integer -> Integer
sumPrimeDivisorsHelper n sum i
 | i > n = sum
 | n `mod` i == 0 && isPrime i  = sumPrimeDivisorsHelper n (sum + i) (i + 1)
 | otherwise = sumPrimeDivisorsHelper n sum (i + 1)
