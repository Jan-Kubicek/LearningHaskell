maxFromTwo a b = if(a > b) then a else b
obvodObdelnika a b = (a+b)*2
sign x
     | x < 0 = -1
     | x > 0 = 1
     | x == 0 = 0
obsahKrychle x = x^3

factorial 0 = 1
factorial n = n * factorial (n-1) 

hlava (h:_) = h

ocas (_:t) = t

mesic (rok:mesic:den) = mesic

tretiPrvek (_:_:h:_) = h

seznam = [x | x <- [1..90], (mod (x^2) 9 ) == 0 ]
seznam1 = [ x^2 | x <- [1..90], (mod x 9) == 0]

posledniPrvek [x] = x
posledniPrvek (_:x) = posledniPrvek x

seznam' = [1000,998 .. 1]

seznam'' = [-99.. (-2)]

ntyPrvek 1 (x:_) = x
ntyPrvek k (x:s) = ntyPrvek(k-1) s

delka [] = 0
delka (_:t) = 1 + delka t

jeClen _ [] = False
jeClen x (h:t) = if(x == h) then True else jeClen x t

ntyPrvek' 1 (x:_) = x
ntyPrvek' k (h:t) = ntyPrvek' (k-1) t

spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)
-- funguje


