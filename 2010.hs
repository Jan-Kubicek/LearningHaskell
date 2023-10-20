spoj [] s2 = s2
spoj (h:t) s2=  h: spoj t s2 

otoc [] = [] 
otoc (h:t) = spoj ( otoc t ) [h]

smazZapornaCisla [] = []
smazZapornaCisla (h:t) = if (h >= 0) then spoj [h] (smazZapornaCisla t) else smazZapornaCisla t

smazVseNaSudychMistech [] i = []
smazVseNaSudychMistech (h:t) i = if((mod i 2)==0) then smazVseNaSudychMistech t (i+1) else spoj [h] (smazVseNaSudychMistech t (i+1))

smazVseNadMez [] m = []
smazVseNadMez (h:t) m = if( h > m ) then smazVseNadMez t m else spoj [h] (smazVseNadMez t m)

skalarniSoucin [] [] = 0
skalarniSoucin (h:t) (h1:t1) = (h*h1) + (skalarniSoucin t t1)

kombinace2Seznamu [] [] = []
kombinace2Seznamu (h:t) (h1:t1) = spoj [h] (spoj [h1] (kombinace2Seznamu t t1))

funSum [] = 0
funSum (h:t) = h + (funSum t)

indexPrvkuVPoli [] p = 0
indexPrvkuVPoli (h:t) p = if (h == p) then indexPrvkuVPoli [] p else 1 + (indexPrvkuVPoli t p)

pocetPrvkuVPoli [] = 0
pocetPrvkuVPoli (h:t) = 1 + (pocetPrvkuVPoli t)

ntyPrvek (h:_) 0 = h
ntyPrvek (h:t) n = ntyPrvek t (n-1)

isPrvekHere [] p = False
isPrvekHere (h:t) p = if ( h == p) then True else isPrvekHere t p

posledniPrvek [x] = x
posledniPrvek (h:t) = posledniPrvek t

predPosledniPrvek [x,y] = x
predPosledniPrvek (h:t) = predPosledniPrvek t

nasobeni a b = a*b

variace1az5 = let x = [1..5] in [[a1,a2,a3,a4,a5]| a1<- x, a2<-x, a3<-x, a4<-x, a5<-x] 
dvojiceCisel = [[x,x^2-1]| x<- [1..10]]
sudaCisla = [ x | x <- [1..500], ((mod x 2)== 0)&& ((mod x 3) == 0 ) && ((mod x 7) == 0) ]