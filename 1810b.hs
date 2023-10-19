-- Práce se seznamem
spoj [] s2 = s2
spoj (h:t) s2 = h:spoj t s2 

smazVsechnyZapornycisla [] = []
smazVsechnyZapornycisla (h:t) = if ( h >= 0 ) then spoj [h] (smazVsechnyZapornycisla t) else smazVsechnyZapornycisla t

smazVseNaSudychMistech [] p = []
smazVseNaSudychMistech (h:t) p = if((mod p 2) == 0) then smazVseNaSudychMistech t (p+1) else spoj [h] (smazVseNaSudychMistech t (p+1))

smazVseNadMez [] m = []
smazVseNadMez (h:t) m = if ( m <= h ) then smazVseNadMez t m else spoj [h] (smazVseNadMez t m)

skalarniSoucin [] [] = 0
skalarniSoucin (h:t) (h1:t1) = (h*h1) + (skalarniSoucin t t1)

kombinaceSeznamu [] [] = []
kombinaceSeznamu (h:t) (h1:t1) = spoj [h] (spoj [h1] (kombinaceSeznamu  t t1))

indexPrvkuVPoli [] p = 0
indexPrvkuVPoli (h:t) p = if(h == p) then  indexPrvkuVPoli [] p else 1 + (indexPrvkuVPoli t p)

pocetPvrkuVPoli [] = 0
pocetPvrkuVPoli (h:t) = 1 + (pocetPvrkuVPoli t)

n_tyPrvek (h:_) 0 = h
n_tyPrvek (h:t) n = n_tyPrvek t (n-1)

isPrvekHere [] p = False
isPrvekHere (h:t) p = if ( p == h) then True else isPrvekHere t p

posledniPrvek [x] = x
posledniPrvek (h:t) = posledniPrvek t

predposledniPrvek [x,y] = x
predposledniPrvek (h:t) = predposledniPrvek t

-- Částečná aplikace funkce

nasobeni a b = a * b
-- petProcent = nasobeni 0.05
-- osmina = nasobeni 0.125

-- generování
seznam1 = let x = [1..3] in [[a,b,c] | a <- x, b <- x, c <- x ]
seznam2 = [[x,x^2-1] | x<- [1..10]]
seznam3 = [ x | x <- [1..500], ((mod x 2) == 0) && ((mod x 3) == 0) && ((mod x 7) == 0)]