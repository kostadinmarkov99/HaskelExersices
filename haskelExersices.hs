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
 print(isPrime 5)
 
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
isPrime n = (n > 2 && isPrimeHelper n 2)

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper n k
 |k^2 > n   = True
 |mod n k == 0 = False
 |otherwise = isPrimeHelper n (k + 1)

{-
  Зад. 7. Да се напише предикат isAscending, който връща истина, ако цифрите на
  дадено естествено число са в нарастващ ред от първата към последната.
-}
