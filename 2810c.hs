-- Lets go boys
{-

    multiple line Comment

-}

ot [] ak = ak
ot (h:t) ak = ot t (h:ak)
otoc text = ot text []

spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

smazVV p [] = []
smazVV p (h:t) = if( h == p ) then smazVV p t else h:(smazVV p t)

smazPV p [] = []
smazPV p (h:t) = if ( h == p ) then t else h:(smazPV p t)

smazVZP [] = []
smazVZP (h:t) = if ( h > 0 ) then h: ( smazVZP t ) else smazVZP t

smazVNSM m [] = []
smazVNSM m (h:t) = if ( (mod m 2) == 0 ) then smazVNSM (m+1) t else h:(smazVNSM (m+1) t)

smazVNM m [] = []
smazVNM m (h:t) = if ( h > m ) then smazVNM m t else h:(smazVNM m t)

sS [x] [y] = x*y
sS (h:t) (h1:t1) = (h*h1) + (sS t t1)

kombinace [] [] = []
kombinace (h:t) (h1:t1) = h:(h1:(kombinace t t1))

fSum [] = 0
fSum (h:t) = h + ( fSum t )

iPVP p [] = 0
iPVP p (h:t) = if ( p == h ) then iPVP p [] else 1 + ( iPVP p t )

pPVP p [] = 0
pPVP p (h:t) = 1 + ( pPVP p t )

ntyP 0 (h:_) = h
ntyP n (_:t) = ntyP (n-1) t

isPH p [] = False
isPH p (h:t) = if ( p == h ) then True else isPH p t

pP [x] = x
pP (h:t) = pP t

prP [x,y] = x
prP (h:t) = prP t

petPRO = (*0.05)

osmina = (*0.125)

zvysO2 = (+2)

umocniNa6 = (^6)

umocni6 = (6^)

aplNS f [] = []
aplNS f (h:t) = (f h):(aplNS f t)

var15 = let x = [1..5] in [[a1,a2,a3,a4,a5] | a1<-x,a2<-x,a3<-x,a4<-x,a5<-x]

var13 = [[a,b,c] | a<-p, b<-p, c<-p] where p = [1..3]

dvojce = [[x,x^2-1]| x<-[1..10]]

sudaC = [[x] | x<-[1..500], ((mod x 2)==0)&&((mod x 3)==0)&&((mod x 7)==0)]