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
-- to binary
bin 0 = 1--b = b
bin a --b
   -- | a > 0 = if((mod a 2) == 0 ) then b ++ "0" else b ++ "1" 
    | otherwise = bin (a-2) --b


--bin2::(Int -> Int, String -> String)
{-
bin2 n s 
    | n > 0 = if((mod n 2) == 0) then bin2 round(n/2) (s ++ "0") else bin2 round(n/2) (s ++ "1") 
    | otherwise = s
--pattern1 a b  = pattern1 (a+round(b/10) b/10)
-}
{-
bin2 0 s = 0
bin2 n s = if((mod n 2) == 0) then bin2 round(n/2) s++"0" else bin2 round(n/2) s++"1"
-}

pattern1 0 = 0
pattern1 n = pattern1(n-1) + n

--intenzionální zápis [x| x <- [1..6], even x] -> [2,4,6]
--             Transformace  Generátor  Filtrace
--                  [ f(x) | x <-s, p(x)]
-- [1..10]      [0,2..10]

-- Seznamy
s1 = [1..100]
s2 = [100,99..1]
s3 = [-1,-2..(-10)]
s4 = [10,20..100]
s5 = [100,90..10]
s6 = [ x ^ 2 | x <- [1..10]] -- zde není filtr jen transformace 
s7 = [ [x,y,z] | x <- [1..4], y <- [1..4], z <-[1..4]] -- variace (použití více generátorů najednou)
{-
    Generátor generuje od posledního generátoru ( dokud mu nedojdou možnosti )
    Podobný vnořeným for cyklům
-}
s7'= let s = [1..4] in [[x,y,z] | x <- s , y <- s , z <- s ]
-- výraz let s nahrazuje všechny svoje instance s za seznam od 1 do 4 

s7''= let s = [1..4] in [(x,y,z) | x <- s , y <- s , z <- s ]
-- výraz vytváří n-tice nikoliv seznamy (s n-ticemi je horší manipulace)

s8 = [ x ^ 2 | x <-[1..10] , even x]

s9 = let c = 1200600 in [ x | x <- [1..c], (mod c x) == 0] -- delitele cisla 1200600
s9' c = [ x | x <- [1..c], (mod c x) == 0] -- delitele cisla c

-- manipulace se seznamem
prvniPrvek [] = error"Kde nic neni ani smrt nebere"
prvniPrvek (h:t) = h    --sekani hlav (cutting head) 
prvniPrvek' (h:_) = h -- pokud nás tělo vůbec nezajímá
-- manipulace se seznamem v půlsemestrálce
zbytekVSeznamu (h:t) = t
zbytekVSeznamu' (_:t) = t -- pokud nás hlava vůbec nezajímá
{-
    vždy vzory závorkovat
-}

druhyPrvek' (_:h:_) = h 
druhyPrvek'' (_:t) = prvniPrvek t

posledniPrvek [x] = x
posledniPrvek (_:xt) = posledniPrvek xt 

predPosledniPrvek [x] = error"Nemá smysl"
predPosledniPrvek [x,_] = x -- vzor pro výsledek [x,y] 
predPosledniPrvek (_:t) = predPosledniPrvek t
-- musíme si prvně naimplementovat "když mám štěstí a až poté pokračovat ulehčí nám to práci"

isPrvekHere a [] = False
isPrvekHere a (h:t) = if( h == a) then True else isPrvekHere a t
-- 2. koncové podmínky rekurze
-- elem funkce

nty_prvek 1 (h:_) = h
nty_prvek k (h:t) = nty_prvek (k-1) t
-- počítáme od 1

pocetPrvkuVPoli k [] = k
pocetPrvkuVPoli k (h:t) = pocetPrvkuVPoli (k+1) t

pocetPrvkuVPoli' [] = 0
pocetPrvkuVPoli' (h:t) = 1 + pocetPrvkuVPoli' t 

indexPrvkuVPoli k x [y] = y
indexPrvkuVPoli k x (h:t) = if( x == h) then k else indexPrvkuVPoli (k+1) x t
-- k je index x je vyhledávaný výraz

smazPrvni s [] = []
smazPrvni s (h:t) = if (s == h) then t else h: smazPrvni s t

smazVsechny s [] = []
smazVsechny s (h:t) = if( s == h) then smazVsechny s t else h: smazVsechny s t

otoc [] akumulator = akumulator
otoc (h:t) akumulator = otoc t (h:akumulator) 
-- 2. parametry

-- 1. parametr =>
otoc' xs = otoc xs []
-- funkce s jedním parametrem využívá funkce se 2. parametry

-- funkce vyšších řádů je schopna přijmou nebo vrátit funkci 
-- řezy umožňují vytváření částečných aplikací funkcí

-- řezy         řez => (+2)  "pojmenování výrazu"
zvysO2 = (+2)

umocniNa6 = (^6)

umocni6 = (^) 6
umocni6' = (6^)

aplikujBinarniFunkcni a b f = f a b

a1 = aplikujBinarniFunkcni 8 4 (*) 
a2 = aplikujBinarniFunkcni 5 9 zbytekPoDeleni

abf12 = aplikujBinarniFunkcni 1 2

zvys [] = []
zvys (h:t) = (h+1): zvys t

zvys' [] = []
zvys' (h:t) = aplikujBinarniFunkcni 1 h (+) : zvys' t

aplikujNaSeznam [] f = []
aplikujNaSeznam (h:t) f = f h : aplikujNaSeznam t f
-- map
-- aplikujNaSeznam [1..5] (^3)

aplikuj = aplikujNaSeznam [(+),(-),(/),(*)] abf12