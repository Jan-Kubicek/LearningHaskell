spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

otoc [] = []
otoc (h:t) = spoj (otoc t) [h]

smazVsechnyZapornyCisla [] = []
smazVsechnyZapornyCisla (h:t) = if(h>= 0) then spoj [h] (smazVsechnyZapornyCisla t) else smazVsechnyZapornyCisla t

smazVseNaSudychMistech [] s = []
smazVseNaSudychMistech (h:t) s = if((mod s 2) == 0) then smazVseNaSudychMistech t (s+1) else spoj [h] (smazVseNaSudychMistech t (s+1))

smazVseNadMez [] m = []
smazVseNadMez (h:t) m = if(h>m) then smazVseNadMez t m else spoj [h] (smazVseNadMez t m)

skalarniSoucin [x] [y] = x*y
skalarniSoucin (h:t) (h1:t1) = (h*h1) + (skalarniSoucin t t1)

kombinace [] [] = []
kombinace (h:t) (h1:t1) = spoj [h] ( spoj [h1] (kombinace t t1))

fceSum [x] = x
fceSum (h:t) = h+(fceSum t)

indexPrvku [] x = 0
indexPrvku (h:t) x = if( h == x) then indexPrvku [] x else 1 + (indexPrvku t x)

pocetPrvkuVPoli [] = 0
pocetPrvkuVPoli (h:t) = 1 + (pocetPrvkuVPoli t)

ntyPrvek (h:_) 0 = h
ntyPrvek (h:t) n = ntyPrvek t (n-1) 

isPrvekHere [] p = False
isPrvekHere (h:t) p = if ( h == p ) then True else isPrvekHere t p

posledniPrek [x] = x
posledniPrek (h:t) = posledniPrek t

predPosledniPrvek [x,y] = x
predPosledniPrvek (h:t) = predPosledniPrvek t

nasobeni a b = a*b

s = let i = [1..5] in [[a,b,c,d,e] | a<-i,b<-i,c<-i,d<-i,e<-i]
s1 = let i = [1..3] in [[a,b,c] | a<-i, b<-i, c<-i]
s2 = [[x,x^2-1] | x<- [1..10]]
s3 = [ x | x<-[1..500], ((mod x 2) == 0)&&((mod x 3) ==0)&&((mod x 7)==0)]