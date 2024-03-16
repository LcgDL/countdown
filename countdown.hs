-- Countdown Game - Solution

-- Declaring a new data type for the four arithmetic operations
data Op = Add | Sub | Mul | Div

-- Defining how to display an operator using the Show instance
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- Function to check if an operation is valid under certain rules
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

-- Function to apply an operation to two integers
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Data type to represent expressions, either a value or an application of an operation to two expressions
data Expr = Val Int | App Op Expr Expr

-- Defining how to display an expression using the Show instance
instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
        where
            brak (Val n) = show n
            brak e = "(" ++ show e ++ ")"

-- Function to extract all integer values from an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- Function to evaluate an expression, if possible
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- Generate all subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x :) yss
    where yss = subs xs

-- Function to interleave an element into all possible positions within a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y :) (interleave x ys)

-- Generate all permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- Generate all choices from a list, which are all possible ways to select zero or more elements in any order
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- Check if an expression is a solution for a given list of numbers and a target number
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- Function to split a list into all possible pairs of non-empty sublists
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- Generate all possible expressions that can be made from a list of numbers
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = 
    [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

-- Combine two expressions using all operations
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- List of all operations
ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- Find all solutions for a given list of numbers and a target number
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- Type alias for a result, which is a pair of an expression and its value
type Result = (Expr, Int)

-- Generate all possible results (expressions and their values) from a list of numbers
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
    [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

-- Combine two results using all valid operations
combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

-- Find all solutions that match the target number using the refined results approach
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

-- Main function to find and print solutions for a specific case
main :: IO ()
main = print (solutions' [1, 3, 7, 10, 25, 50] 765)
