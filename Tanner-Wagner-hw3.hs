-- Homework 3 Due March 20th 11:59 pm
-- Tanner Wagner
-- Professor Haugh
-- CS 357
-- 20 March 2024

{-
Submission rules:

- All text answers must be given in Haskell comment
  underneath the problem header.
- You must submit a single .hs file with the
  following name: firstName-lastName-hw3.hs.
  Failure to do so will result in -10 points.
  
- You will lose 10 points if you put a module statement
  at the top of the file.

- You will lose 10 points for any import statements you have
  in your file and will automatically miss any problems you used
  an imported function on.

- If your file doesn't compile you will lose 10 points and miss any
  problems that were causing the compilation errors.

- This means that any function which is causing compiler errors should
  be commented out. There will be no partial credit.

- You must use the skeleton file provided and must not alter any type
  signature. If you alter a type signature you will automatically miss
  that problem.

- You will lose 10 points if you include a *main* function in your file.
-}

-------------------------------------------------------------------------------------
-- Problem 1 (Exercise 7.5) (2 pts)

-- Without looking at the definitions from the standard prelude, define the higher-order
-- library function curry' that converts a function on pairs into a curried function, and
-- conversely, the function uncurry' that converts a curried function with two arguments into a
-- function on pairs

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' g (x, y) = g x y

-------------------------------------------------------------------------------------
-- Problem 2 (Exercise 7.9) (5 pts)

-- A higher-order function unfold that encapsulates a simple pattern of recursion for 
-- producing a list can be defined as follows:

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

-------------------------------------------------------------------------------------
-- Problem 3 (4 pts)

-- Define a function, concat, which takes a list of lists and transforms it into a 
-- flattened list. Write this function 3 different ways:

-- 1.) Using explicit recursion
-- 2.) Using foldr
-- 3.) Using foldl

-- Name the functions as follows:

-- 1.) concatER
-- 2.) concatFR
-- 3.) concatFL

concatER :: [[a]] -> [a]
concatER [] = []
concatER (xs:xss) = xs ++ concatER xss

concatFR :: [[a]] -> [a]
concatFR = foldr (++) []

concatFL :: [[a]] -> [a]
concatFL = foldl (++) []

-------------------------------------------------------------------------------------
-- Problem 4 (5 pts)

-- Write a function, disjunciton2, which takes two predicates as arguments and returns
-- the predicate which returns True if either of the two predicates returns True.

disjunction2 :: (a -> Bool) -> (a -> Bool) -> a -> Bool
disjunction2 p1 p2 x = p1 x || p2 x

-------------------------------------------------------------------------------------
-- Problem 5 (5 pts)

-- Use foldr to write a function, disjunction, which takes an arbitrary number (> 0)
-- of predicates as arguments

disjunction :: [a -> Bool] -> a -> Bool
disjunction ps x = foldr (\p acc -> p x || acc) False ps

-------------------------------------------------------------------------------------
-- Problem 6 (7 pts)

-- Use foldr to write a function, deleteDupes, which takes a list and returns a list with 
-- all duplicate elements removed. Note: you must use foldr but you are also free to use
-- other helper/higher order functions

deleteDupes :: Eq a => [a] -> [a]
deleteDupes = foldr notPresent []
  where
    notPresent x ys
      | x `elem` ys = ys
      | otherwise   = x : ys

-------------------------------------------------------------------------------------
-- Problem 7 (7 pts)

-- Using foldl, write a function, tally, which returns the number of elements that 
-- pass a predicate. 

tally :: (a -> Bool) -> [a] -> Int
tally p xs = foldl (\acc x -> if p x then acc + 1 else acc) 0 xs

-------------------------------------------------------------------------------------
-- Problem 8 (7 pts)

-- Using foldr and zip, write a funciton, bangBang, which takes a list and returns 
-- the nth element of the list. You can assume the list is non-empty and if n is 
-- larger than the list then return the last element of the list

-- Problem 8
bangBang :: [a] -> Int -> a
bangBang xs n = foldr (\(i, x) acc -> if i == n then x else acc) (last xs) (zip [0..] xs)

-------------------------------------------------------------------------------------
-- Problem 9 (8 pts)

-- Using foldr and zip, write a function, increasing, which takes a list and determines
-- if the list is in increasing order

increasing :: Ord a => [a] -> Bool
increasing xs = foldr (\(x, y) acc -> x <= y && acc) True (zip xs (tail xs))

-------------------------------------------------------------------------------------
-- Problem 10 (10 pts)

-- Using foldl, write a function, decimate, which takes a list and removes every 10th
-- element 

decimate :: [a] -> [a]
decimate xs = snd $ foldl (\(count, acc) x -> if count == 9 then (0, acc) else (count + 1, acc ++ [x])) (0, []) xs

-------------------------------------------------------------------------------------
-- Problem 11 (10 pts)

-- Define a function, encipher, which takes two lists of equal length and a third list to encrypt
-- It uses the first two lists to define a substitution cipher which it uses to encrypt the third
-- list. 

encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher [] _ _ = []
encipher _ [] _ = []
encipher _ _ [] = []
encipher xs ys zs = map (substitute xs ys) zs
  where 
    substitute :: Eq a => [a] -> [b] -> a -> b
    substitute [] _ _ = error "error"
    substitute (x:xs) (y:ys) z
      | x == z = y
      | otherwise = substitute xs ys z


-------------------------------------------------------------------------------------
-- Problem 12 (10 pts)

-- Define a function, prefixSum, which takes a list and returns a list
-- of all sums of prefixes of that list

prefixSum :: Num a => [a] -> [a]
prefixSum xs = reverse (snd (foldl (\(acc, sums) x -> (acc + x, (acc + x) : sums)) (0, []) xs))

-------------------------------------------------------------------------------------
-- Problem 13 (20 pts)

-- Define a function, minesweeper, which takes a non-empty list of String and
-- returns a non-empty list of String with the number of mines adjacent to each cell
-- Each mine is represented with the * character and empty space the . character
-- If the space has no mines near it then leave it empty. You may need the function
-- intToDigit

-- Helper function provided 
intToDigit :: Int -> Char
intToDigit 0 = '0'
intToDigit 1 = '1'
intToDigit 2 = '2'
intToDigit 3 = '3'
intToDigit 4 = '4'
intToDigit 5 = '5'
intToDigit 6 = '6'
intToDigit 7 = '7'
intToDigit 8 = '8'
intToDigit 9 = '9'

minesweeper :: [String] -> [String]
minesweeper grid = [[newCell x y | x <- [0..length (head grid) - 1]] | y <- [0..length grid - 1]]
  where
    newCell x y
      | (grid !! y) !! x == '*' = '*'
      | otherwise = let
                    count = countMines x y
                    in if count == 0 then '.' else intToDigit count

    countMines x y = length (filter (=='*') (concatMap (\dy -> map (\dx -> getCell (x + dx) (y + dy)) [-1,0,1]) [-1,0,1]))

    getCell x y
      | x < 0 || y < 0 || y >= length grid || x >= length (head grid) = '.'
      | otherwise = (grid !! y) !! x
