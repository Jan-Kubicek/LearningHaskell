spoj [] s2 = s2
spoj (h:t) s2 = h: spoj t s2

otoc [] = []
otoc (h:t) = spoj (otoc t ) [h]

smazZapornaCisla [] = []
smazZapornaCisla (h:t) = if ( h > 0 ) then spoj [h] (smazZapornaCisla t) else smazZapornaCisla t

smazVseNaSudychMistech [] _ = []
smazVseNaSudychMistech (h:t) p = if((mod p 2) == 0 ) then spoj [h] (smazVseNaSudychMistech t (p+1)) else smazVseNaSudychMistech t (p+1)

smazVseNadMez [] m = []
smazVseNadMez (h:t) m = if( m >= h ) then spoj [h] (smazVseNadMez t m) else smazVseNadMez t m

skalarniSoucin [] [] = 0
skalarniSoucin (h:t) (h1:t1) = (h*h1) + (skalarniSoucin t t1)

kombinace2Seznamu [] [] = []
kombinace2Seznamu (h:t) (h1:t1) = spoj [h] (spoj [h1] (kombinace2Seznamu t t1))

sumFce [] = 0
sumFce (h:t) = h + ( sumFce t )

indexPrvkuVPoli [] i = 0
indexPrvkuVPoli (h:t) i = if( h == i ) then indexPrvkuVPoli [] i  else 1 + (indexPrvkuVPoli t i) 

pocetPrvkuVPoli [] = 0
pocetPrvkuVPoli (h:t) = 1 + ( pocetPrvkuVPoli t )

ntyPrvek (h:_) 0 = h
ntyPrvek (h:t) n = ntyPrvek t (n-1)

isPrvekHere []  p = False
isPrvekHere (h:t) p = if(h == p) then True else isPrvekHere t p

posledniPrvek [x] = x
posledniPrvek (h:t) = posledniPrvek t

predposledniPrvek [x,y] = x
predposledniPrvek (h:t) = predposledniPrvek t

nasobeni a b = a*b

variace1az3 = let c = [1..3] in [[x,y,z] | x<- c, y <-c, z <- c ]

dvojiceCisel = [[x, x^2-1] | x <-[1..10]]

sudaCisla = [ x | x <- [1..500], ((mod x 2) == 0 )&& ((mod x 3 )== 0) && ((mod x 7) == 0)]