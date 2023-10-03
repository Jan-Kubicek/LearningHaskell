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
pyr n = (n+(n-1))^2 + pyr(n-1) -- funguje správně
--maxSez [h] =  h
--maxSez a (h:t) = vetsi h (maxSez t)