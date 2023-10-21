spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2) 

otoc [] = []
otoc (h:t) = spoj (otoc t) [h]

smazZapornaCisla [] = []
smazZapornaCisla (h:t) = if ( h >= 0 ) then spoj [h] ( smazZapornaCisla t ) else smazZapornaCisla t

smazVseNaSudychMistech [] m = []
smazVseNaSudychMistech (h:t) m = if (((mod m 2) == 0 )) then smazVseNaSudychMistech t (m+1) else spoj [h] (smazVseNaSudychMistech t (m+1))

smazVseNadMez [] m = []
smazVseNadMez (h:t) m = if (h > m) then smazVseNadMez t m else spoj [h] (smazVseNadMez t m)

skalarniSoucin [x] [y] = x*y
skalarniSoucin (h:t) (h1:t1) = (h*h1) + skalarniSoucin t t1

kombinace2Seznamu [] [] = []
kombinace2Seznamu (h:t) (h1:t1) = spoj [h] (spoj [h1] (kombinace2Seznamu t t1))

fceSum [x] = x
fceSum (h:t) = h + fceSum t 

indexPrvkuVPoli [] p = 0
indexPrvkuVPoli (h:t) p = if( h == p) then indexPrvkuVPoli [] p else 1+(indexPrvkuVPoli t p)   

pocetPrvkuVPoli [x] = 1
pocetPrvkuVPoli (h:t) = 1+(pocetPrvkuVPoli t)

ntyPrvek (h:_) 0 = h
ntyPrvek (h:t) n = ntyPrvek t (n-1)

isPrvekHere [] p = False
isPrvekHere (h:t) p = if (h == p ) then True else isPrvekHere t p

posledniPrvek [x] = x
posledniPrvek (h:t) = posledniPrvek t

predposledniPrvek [x,y] = x
predposledniPrvek (h:t) = predposledniPrvek t

nasobeni a b = a*b

variace3 = let x = [1..3] in [[a,b,c]| a <-x, b<- x, c <-x ]
variace5 = let x = [1..5] in [[a,b,c,d,e]| a<-x,b<-x,c<-x,d<-x,e<-x]
sudaCisla = [x | x <- [1..500], ((mod x 2) ==0)&& ((mod x 3) ==0) && ((mod x 7) ==0)]
dvojice = [[x,x^2-1]| x<-[1..10]]
