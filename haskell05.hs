--Nome: Gabriele Soares Quevedo

--1
bmi :: Float -> Float -> String  
bmi peso alt =
         let imc = peso / (alt * alt)    
          in if imc <= 18.5 then "ABAIXO" else if imc >= 30 then "ACIMA" else "NORMAL"

--2
bmi' :: Float -> Float -> String
bmi' peso alt = if imc <= 18.5 then "ABAIXO" else if imc >= 30 then "ACIMA" else "NORMAL"
      where imc = peso / (alt * alt)

--3
cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
          where digits = take 9 cpf
                dv1 = cpfDV digits [10,9..]
                dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
          let expr = (sum $ zipWith (*) digits mults) `mod` 11
          in if expr < 2 then 0 else 11-expr

--4
andTable:: [(Bool,Bool,Bool)]
andTable = [(x,y,x&&y) | x <- [False,True], y <- [False,True]]