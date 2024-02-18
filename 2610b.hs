spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

ot [] ak = ak
ot (h:t) ak = ot t (h:ak)
otoc s2 = ot s2 []

smazPrvni p [] = []
smazPrvni p (h:t) = if (p == h) then t else h:(smazPrvni p t)

smazVsechny p [] = []
smazVsechny p (h:t) = if (p == h) then smazVsechny p t else h:(smazVsechny p t) 

smazZapornyCisla [] = []
smazZapornyCisla (h:t) = if(h>0) then h:(smazZapornyCisla t) else smazZapornyCisla t

smazNaSudych p [] = []
smazNaSudych p (h:t) = if((mod p 2)==0) then smazNaSudych (p+1) t else h:(smazNaSudych (p+1) t)

smazNadMez m [] = []
smazNadMez m (h:t) =if( m > h) then h:(smazNadMez m t) else smazNadMez m t

skalarniSoucin [x] [y] = x*y
skalarniSoucin (h:t) (h1:t1) = (h*h1) + (skalarniSoucin t t1)

kombinace [] [] = []
kombinace (h:t) (h1:t1) = h:(h1:(kombinace t t1))

fSum [] = 0
fSum (h:t) = h + (fSum t)

indexPrvkuVPoli _ [] = 0
indexPrvkuVPoli i (h:t) = if (i == h) then indexPrvkuVPoli i [] else 1 + (indexPrvkuVPoli i t)

pocetPrvkuVPoli [] = 0
pocetPrvkuVPoli (h:t) = 1 + (pocetPrvkuVPoli t)

ntyPrvek 0 (h:_) = h
ntyPrvek n (h:t) = ntyPrvek (n-1) t

isPrvekHere p [] = False
isPrvekHere p (h:t) = if( p == h ) then True else isPrvekHere p t

posledniPrvek [x] = x
posledniPrvek (h:t) = posledniPrvek t

predPosledniPrvek [x,y] = x
predPosledniPrvek (h:t) = predPosledniPrvek t

petProcent = (*0.05)

osmina = (*0.125)

zvysO2 = (+2)

umocniNa6 = (^6)

umicni6 = (6^)

variace = let x = [1..5] in [[x1,x2,x3,x4,x5] |  x1<-x, x2<-x, x3<-x, x4<-x, x5<-x]

variace2 = let x = [1..3] in [[a,b,c] | a <-x, b <-x, c <-x]

sudyCisla = [[x] | x <- [1..500],((mod x 2) == 0) && ((mod x 3)==0) && ((mod x 7) == 0)]

dvojice = [[x,x^2-1] | x <- [1..10]]

aplikujFunkciNaSeznam f [] = []
aplikujFunkciNaSeznam f (h:t) = f h : (aplikujFunkciNaSeznam f t)