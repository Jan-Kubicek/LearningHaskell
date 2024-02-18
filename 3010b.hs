{-

    Zkusit funkčnost

-}

ot [] ak = ak
ot (head:tail) ak = ot tail (head:ak)
otoc text = ot text []

spoj [] s2 = s2
spoj (head:tail) s2 = h:(spoj tail s2)

smazPV p [] = []
smazPV p (head:tail) = if(p == head) then tail else head:(smazPV p tail)

smazVV p [] = []
smazVV p (head:tail) = if( p == head) then smazVV p tail else head:(smazVV p tail)

smazVZC [] = []
smazVZC (head:tail) = if (head > 0) then head:(smazVZC tail) else smazVZC tail

smazVNSM m [] = []
smazVNSM m (head:tail) = if ((mod m 2)==0) then smazVNSM (m+1) tail else head:(smazVNSM (m+1) tail)

smazNM m [] = []
smazNM m (head:tail) = if (head > m )then smazNM m tail else head:(smazNM m tail)

sS [x] [y] = x*y
sS (head:tail) (head1:tail1) = (head*head1) + (sS tail tail1)

komb [] [] = []
komb (head:tail) (head1:tail1) = head:(head1:(komb tail tail1))

fSum [] = 0
fSum (head:tail) = head + (fSum tail)

iPVP p [] = 0
iPVP p (head:tail) = if(p == head) then iPVP p [] else 1 + (iPVP p tail)

pPVP [] = 0
pPVP (head:tail) = 1 + (pPVP tail)

ntyP 0 (head:_) = head
ntyP n (_:tail) = ntyP (n-1) tail

isPH p [] = False
isPH p (head:tail) = if (p==head) then True else isPH p tail

pP [x] = x
pP (_:tail) = pP tail

prP [x,y] = x
prP (_:tail) = prP tail

petP = (*0.05)

osmina = (*0.125)

zvysO2 = (+2)

umocniNa6 = (^6)

umocni6 = (6^)

aFNS f [] = []
aFNS f (head:tail) = (f head) : (aFNS f tail)

var15 = let x = [1..5] in [[a,b,c,d,e]| a<-x, b<-x,c<-x,d<-x,e<-x]

var13 = [[a,b,c,] | a<-p, b<-p, c<-p] where p = [1..3]

dvojce = [[x,x^2-1] | x <- [1..10] ]

suda = [[x]| x<-[1..500], ((mod x 2)==0)&&((mod x 3)==0)&&((mod x 7)==0)]

