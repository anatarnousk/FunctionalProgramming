--Questions from 260 lecture slides

-- Exercise: Write a Function called second that returns the second element of a List 
import Data.Char

second :: [a] -> a

second list
	| length list < 2 = error "That list has less than two elements!"
	| otherwise = head(tail list)

-- Exercise: Write a function called capitalize that capitalizes al lower-case letters in parameter

capitalize :: String -> String

capitalize "" = ""
capitalize str = [toUpper chr | chr <- str ] 

-- Exercise: Write a function, capitalize2, that converts all lower-case to uppercase & discards non-alpha chars

capitalize2 :: String -> String

capitalize2 "" = ""
capitalize2 (x:xs) 
	| isAlpha x = toUpper x : capitalize2 xs 
	| otherwise = capitalize2 xs

-- Exercise: firtOdd returns the first number in an argument that is odd

firstOdd :: [Int] -> Int

firstOdd [] = error "Error! None of the elements of your list are odd."
firstOdd (x:xs)
	| mod x 2 /= 0 = x
	| otherwise = firstOdd xs

-- Exercise: write your own head function: newHead

newHead :: [a] -> a

newHead (x:xs) = x
newHead _ = error "Error! head of empty list"

-- Exercise: write your own tail function: newTail

newTail :: [a] -> [a]

newTail [] = error "Error! tail of empty list"
newTail (x:xs) = xs

-- Exercise: write a program that detects singletons

isSingle :: [a] -> Bool

isSingle [_] = True
isSingle _ = False

-- Exercise: Write a function that replaces ++ 

append :: [a] -> [a] -> [a]

append [] list = list
append (x:xs) list = x : (append xs list)

-- Exercise: prefix: checks if param1 is a prefix of param2

--prefix :: [a] -> [a] -> Bool

prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
	| x == y = prefix xs ys
	| otherwise = False

-- Exercise: Write roots, finds roots of quadratic

roots :: Float -> Float -> Float -> (Float, Float)

roots a b c
	| discrim < 0 = error "no roots found!"
	| otherwise = (root1, root2)
	where 
		discrim = b^2 - 4*a*c
		root1 = (-b-sqrt discrim)/2/a
		root2 = (-b+sqrt discrim)/2/a

-- Exercise: Write f: is sum of triple (e1,e2,e3) sum of e1, length e2, e3

triple :: (Int, String, Float) -> Float

triple (a,b,c) = fromIntegral a + fromIntegral (length b) + c 

-- Exercise: related to names (Strings) and marks (Ints)

type CourseData = [(String, Int)]

testCourse :: CourseData 

testCourse = [("Peter", 97), ("Susan", 76), ("Edmund", 50), ("Lucy", 97), ("Peter", 80)]

--Exercise: write function that finds all marks earned by a particular student
courseMarks :: String -> CourseData -> [Int]

courseMarks name cData = [mark | (n, mark) <- cData, n == name]

--Exercise: write sortedPrefix: takes list of integers (Ints), returns longest sorted prefix of list

sortedPrefix :: [Int] -> [Int]

sortedPrefix [] = []
sortedPrefix (x:xs)
	| xs == [] = [x]
	| x < head xs = [x] ++ sortedPrefix xs
	| otherwise = [x]

-- Exercise: write longString: takes list of Strings, returns longest Str in list

longString :: [String] -> String

longString [] = error "Empty list!"
longString (x:xs)
	| xs == [] = x
	| length x < length (head xs) = longString xs
	| otherwise = longString (x : (tail xs))