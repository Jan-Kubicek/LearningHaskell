spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

ot [] ak = ak
ot (h:t) ak = ot t (h:ak)
otoc tet = ot tet []

smazPrvni p [] = []
smazPrvni p (h:t) = if (p==h) then t else h:(smazPrvni p t)

smazVse p [] = []
smazVse p (h:t) = if (p==h) then smazVse p t else h:(smazVse p t)

smazZC [] = []
smazZC (h:t) = if (h>0) then h:(smazZC t) else smazZC t

smazNSM p [] = []
smazNSM p (h:t) = if((mod p 2)==0) then smazNSM (p+1) t else h:(smazNSM (p+1) t)

smazNM m [] = []
smazNM m (h:t) = if (h > m) then smazNM m t else h:(smazNM m t)

skalarniS [x] [y] = x*y
skalarniS (h:t) (h1:t1) = (h*h1) + (skalarniS t t1)

kombinace [] [] = []
kombinace (h:t) (h1:t1) = h:(h1:(kombinace t t1))

fSum [] = 0
fSum (h:t) = h + (fSum t)

indexPVP p [] = 0
indexPVP p (h:t) = if(p == h) then indexPVP p [] else 1+(indexPVP p t)

pocetPVP [] = 0
pocetPVP (h:t) = 1+(pocetPVP t)

ntyPrvek 0 (h:_) = h
ntyPrvek n (h:t) = ntyPrvek (n-1) t

isPH p [] = False
isPH p (h:t) = if (p == h) then True else isPH p t

posledniP [x] = x
posledniP (h:t) = posledniP t

pPP [x,y] = x
pPP (h:t) = pPP t

petP = (*0.05)

osmina = (*0.125)

zvysO2 = (+2)

umocniNa6 = (^6)

umocni6 = (6^)

zvysVSO f [] = []
zvysVSO f (h:t) = (f h): zvysVSO f t

variace13 = [[x,y,z]| x<-p, y<-p, z<-p] where p = [1..3]

variace15 = let i = [1..5] in [[x1,x2,x3,x4,x5] | x1<-i, x2<-i, x3<-i, x4<-i, x5<-i]

dvojice = [[x,x^2-1]| x <- [1..10]]
sudC = [[x]| x<-[1..500], ((mod x 2)==0) && ((mod x 3)==0) && ((mod x 7) == 0)]