
mult x = 2 * x

data Op =     Sum Op Op 
            | Product Op Op 
            | Partial Int 
            | Variable Int

infixl 6 |+|
(|+|) :: Op -> Op -> Op
(|+|) a b = Sum a b

infixl 7 |*|
(|*|) :: Op -> Op -> Op
(|*|) a b = Product a b


instance Show Op where
    show (Sum a b) = show a ++ " + " ++ show b
    show (Product a b ) = "(" ++ show a ++ ")*(" ++ show b ++ ")"
    show (Partial n) = "d" ++ show n
    show (Variable n) = "x" ++ show n



generate :: String -> Op
generate "11" = 
    (Partial 1) |+| (Variable 3)|*|(Partial 2) |+| (Variable 4)|*|(Partial 3)
generate s = 
    if last s == '1' then
        yj |+| (Variable (len+2)) |*| (Partial (len+1))
    else 
        (Variable (len+2)) |*| yj |+| (Partial (len+1))
    where yj = generate (init s)
          len = length s


data Exp =    SumExp [Exp]
            | ProdExp Exp Exp
            | FunExp [Int] Bool
            | ConstExp Int
            | VarExp Int Bool

instance Show Exp where
    show (SumExp [x]) = show x
    show (SumExp (x:xs)) = show x ++ " + " ++ show (SumExp xs)
    show (ProdExp a b) = "(" ++ show a ++ ")*(" ++ show b ++ ")"
    show (FunExp xs True) = "-f_{" ++ show xs ++ "}"
    show (FunExp xs False) = "f_{" ++ show xs ++ "}"
    show (ConstExp n) = show n
    show (VarExp n True) = "-x_{" ++ show n ++ "}"
    show (VarExp n False) = "x_{" ++ show n ++ "}"

infixl 6 <+>
(<+>) :: Exp -> Exp -> Exp
(<+>) (ConstExp 0) a = a
(<+>) a (ConstExp 0) = a
(<+>) (SumExp xs) (SumExp ys) = SumExp (xs ++ ys)
(<+>) (SumExp xs) a = SumExp (a:xs)
(<+>) a (SumExp xs) = SumExp (a:xs)
(<+>) a b = SumExp [a, b]

infixl 7 <**>
(<**>) :: Exp -> Exp -> Exp
(<**>) (ConstExp 0) _ = ConstExp 0
(<**>) _ (ConstExp 0) = ConstExp 0
(<**>) (ConstExp 1) a = a
(<**>) a (ConstExp 1) = a
(<**>) (ConstExp (-1)) a = minus a
(<**>) a (ConstExp (-1)) = minus a
(<**>) a b = ProdExp a b


f1 :: Exp
f1 = FunExp [3] True

f3 :: Exp
f3 = (FunExp [1] False) <+> (VarExp 3 False) <**> (FunExp [2] False)

y2 :: Op
y2 = generate "11"


generateExp :: String -> Exp

generateExp "11" = (eval y2 f3) <+> (VarExp 4 True) <**> (eval y2 f1)

generateExp code = 
    if last code == '1' then
        if s == 0 then
            (eval yj fj) <+> (VarExp (len+2) True) <**> (eval y2 f1)
        else
            (eval yj fj) <+> (VarExp (len+2) True) <**> (eval ys fs)
    else
        if s == 0 then
            (VarExp (len+2) False) <**> ((eval y2 f1) <+> minus (eval yj fj))
        else 
            (VarExp (len+2) False) <**> ((eval ys fs) <+> minus (eval yj fj))
    where s = calc_s code
          len = length code
          yj = generate code
          fj = generateExp (init code)
          fs = generateExp (take (s-1) code)
          ys = generate (take s code) 

calc_s :: String -> Int
calc_s "1121" = 3
calc_s "11211" = 3
calc_s _ = 0

minus :: Exp -> Exp 
minus (SumExp xs) = SumExp (map minus xs)
minus (ProdExp a b) = ProdExp (minus a) b
minus (FunExp xs b) = FunExp xs (not b)
minus (ConstExp n) = ConstExp (-n)
minus (VarExp n b) = VarExp n (not b)


eval :: Op -> Exp -> Exp
eval (Sum a b) e = (eval a e) <+> (eval b e)
eval (Product a b) e = eval a (eval b e)
eval (Partial n) e = partial n e
eval (Variable n) e = ProdExp (VarExp n False) e


partial :: Int -> Exp -> Exp
partial n (SumExp xs) = SumExp (map (partial n) xs)
partial n (ProdExp a b) = (a' <**> b) <+> (a <**> b')
    where a' = partial n a
          b' = partial n b
partial n (FunExp xs b) = if n > 3 then ConstExp 0 else FunExp (n:xs) b
partial n (ConstExp _) = ConstExp 0
partial n (VarExp v False) = if n == v then ConstExp 1 else ConstExp 0
partial n (VarExp v True) = if n == v then ConstExp (-1) else ConstExp 0

-- varNum n exp
subst :: Int -> Int -> Exp -> Exp
subst v n (SumExp xs) = 
    if length filtered == 0 then
        ConstExp 0
    else
        SumExp filtered
    where filtered = filter notZero (map (subst v n) xs)
          notZero (ConstExp 0) = False
          notZero _ = True

subst v n (ProdExp a b) = (subst v n a) <**> (subst v n b)
subst v n x@(VarExp varNum True) = if v == varNum then ConstExp (-n) else x
subst v n x@(VarExp varNum False) = if v == varNum then ConstExp n else x
subst _ _ e = e

--x = generateExp "11211"

--x = eval ((Partial 4) |*| generate "1121") (generateExp "112")
--x = eval ((Variable 6) |*| (Partial 5) |*| generate "1121") (generateExp "112")
--x = eval ((Partial 4) |*| (Variable 6) |*| generate "112") (generateExp "11")
x = eval ((Variable 6) |*| (Variable 6) |*| (Partial 5) |*| generate "112") (generateExp "11")



x1 = subst 1 0 x
x2 = subst 2 0 x1
x3 = subst 3 0 x2
x4 = subst 4 0 x3
x5 = subst 5 0 x4
x6 = subst 6 1 x5
x7 = subst 7 0 x6


z = eval ((Partial 4) |*| (Partial 4) |+| (Variable 6) |*| (Partial 4) |*| (Partial 5)) (generateExp "112")

