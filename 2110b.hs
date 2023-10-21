spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

otoc [] = []
otoc (h:t) = spoj (otoc t) [h]

smazVsechnyZapornaCisla [] = []
smazVsechnyZapornaCisla (h:t) = if(h>=0) then spoj [h] (smazVsechnyZapornaCisla t) else smazVsechnyZapornaCisla t

smazVseNaSudychMistech [] m = []
smazVseNaSudychMistech (h:t) m = if((mod m 2)==0) then smazVseNaSudychMistech t (m+1) else spoj [h] (smazVseNaSudychMistech t (m+1))

smazVseNadMez [] m = []
smazVseNadMez (h:t) m = if( h > m) then smazVseNadMez t m else spoj [h] (smazVseNadMez t m)

skalarniSoucin [x] [y] = x*y
skalarniSoucin (h:t) (h1:t1) = (h*h1) + (skalarniSoucin t t1)

kombinace2Seznamu [] [] = []
kombinace2Seznamu (h:t) (h1:t1) = spoj [h] (spoj [h1] (kombinace2Seznamu t t1))

fceSum [x] = x
fceSum (h:t) = h + (fceSum t)

indexVPoli [] i = 0
indexVPoli (h:t) i = if(i == h) then indexVPoli [] i else 1 + (indexVPoli t i)

toBinary 1 = ['1']
toBinary n = if (n > 1 ) && ((mod n 2) == 0) then (['0'] ++ (toBinary (round (n/2)))) else (['1'] ++ (toBinary (round(n/2)))) 

toBinary' 1 = ['1']
toBinary' n = if((mod n 2)==0) then spoj (toBinary' (round(n/2))) ['0'] else spoj (toBinary' (round(n/2))) ['1']

sign x 
    | x > 0 = 1
    | x < 0 = -1
    | x == 0 = 0

pocetPrvkuVPoli [] = 0
pocetPrvkuVPoli (h:t) = 1+(pocetPrvkuVPoli t)

ntyPrvek (h:_) 0 = h
ntyPrvek (h:t) n = ntyPrvek t (n-1)

isPrvekHere [] p = False
isPrvekHere (h:t) p = if(h == p) then True else isPrvekHere t p

posledniPrvek [x] = x
posledniPrvek (h:t) = posledniPrvek t

predposledniprvek [x,y] = x
predposledniprvek (h:t) = predposledniprvek t

nasobeni a b = a * b

variace5 = let x = [1..5] in [[a,b,c,d,e] | a<-x,b<-x,c<-x,d<-x,e<-x]
variace3 = [[a,b,c]| a <-[1..3], b <- [1..3], c <- [1..3]]
dojiceCisel = [[x, x^2-1]| x <- [1..10]]
suda = [ x | x<- [1..500], ((mod x 2)== 0)&&((mod x 3)==0)&&((mod x 7)==0) ]