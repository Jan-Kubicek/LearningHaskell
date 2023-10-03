-- Škola Komentář
nasobeni a b = a * b
cel x = x/(5/9) + 32  -- z F => C
far x = (5/9)*(x-32)  -- z C => F 
absolute a = if(a < 0) then a * (-1) else a
negation a = if(a) then False else True
myAnd a b = if( ( a  == True )  &&  ( b == True ) ) then True else False  
myOr a b = if(( a == True ) || ( b == True)) then True else False
vetsi2 a b = if(a > b) then a else b
mensi2 a b = if(a<b) then a else b
mocnina a b = abs(a)^abs(b)
dvojnasobek a = 2 * a
soucetCtvercu a b = (a ^ 2) + (b ^ 2)
soucetCtvercU a b = (mocnina a 2) + (mocnina b 2)
jemensi a b = a < b
mensi a b = if(a < b) then a else b
vetsi a b = if(a >b) then a else b
logickySoucet a b = if a then True else if b then True else False
pyr 1 = 1
-- když budu mít štěstí 
pyr n = (n+(n-1))^2 + pyr(n-1) -- funguje správně
-- něco malého se musí stát => n-1
--maxSez [h] =  h
--maxSez a (h:t) = vetsi h (maxSez t)
{-
logickySoucet2 True True = True
logickySoucet2 False True = True
logickySoucet2 True False = True
logickySoucet2 False False = False
-}
logickySoucet2 _ True = True    -- u _ je jedno na hodnotě
logickySoucet2 True _ = True
logickySoucet2 _ _ = False      -- záleží na pořadí řádků => tento musí být poslední

logickaPodminka a = logickySoucet2 (a < 0) (a > 0)

--logicky soucin => oba způsoby doma        viz výše

absolutniHodnota a = if(a < 0) then (-a) else a

absolutniHodnota2 a 
    | a < 0 = (-a)
    | otherwise = a 

sign a -- domaci ukol toto pomocí if-else 
    | a > 0 = 1
    | a < 0 = (-1)
    | a == 0 = 0 -- otherwise = 0
-- tomuto se říká "stráže"

faktorial 0 = 1 -- ukončovací podmínka
faktorial n = n * faktorial (n-1)   -- něco malého co se udělá

faktorial2 n = if(n == 0) then 1 else n * faktorial2(n-1)

faktorial3 n
    | n < 0 = error "Nelze počítat faktoriál z negativního čísla" 
    | n == 0 = 1
    | otherwise = n * faktorial3(n-1)
-- naučit se používat stráže => jsou přehlednější

zbytekPoDeleni a d 
    | a < d = a
    | otherwise = zbytekPoDeleni(a - d) d
        -- rem

nsd a 0 = a
nsd a b  = nsd b x 
    where x = (zbytekPoDeleni a b) 
{-
nsd a 0 = a
nsd a b  = nsd b (zbytekPoDeleni a b) 
-}