ot [] ak = ak
ot (h:t) ak = ot t (h:ak)
otoc text = ot text []

spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

smazPV p [] = []
smazPV p (h:t) = if(h==p) then t else h:(smazPV p t)

smazVV p [] = []
smazVV p (h:t) = if(h==p) then smazVV p t else h:(smazVV p t)

smazVZC [] = []
smazVZC (h:t) = if(h>0) then h:(smazVZC t) else smazVZC t

smazVNSM m [] = []
smazVNSM m (h:t) = if((mod m 2)==0) then smazVNSM (m+1) t else h:(smazVNSM (m+1) t)

smazVNM m [] = []
smazVNM m (h:t) = if(h>m) then smazVNM m t else h:(smazVNM m t)

sS [x] [y] = x*y
sS (h:t) (h1:t1) = (h*h1) + (sS t t1)

komb [] [] = []
komb (h:t) (h1:t1) = h:(h1:(komb t t1))

fSum [] = 0
fSum (h:t) = h + (fSum t)

iPVP p [] = 0
iPVP p (h:t) = if(h==p) then iPVP p [] else 1 + (iPVP p t)

pPVP [] = 0
pPVP (h:t) = 1 + (pPVP t)

nty 0 (h:_) = h
nty n (_:t) = nty (n-1) t

isPH p [] = False 
isPH p (h:t) = if (p==h) then True else isPH p t

pP [x] = x
pP (h:t) = pP t

predP [x,y] = x
predP (h:t) = predP t

petP = (*0.05)

osmina = (*0.125)

zvysO2 = (+2)

umocniNa6 = (^6)

umocni6 = (6^)

apFNS f [] = []
apFNS f (h:t) = (f h):(apFNS f t)

var15 = let x = [1..5] in [(a,b,c,d,e) | a<-x, b<-x, c<-x, d<-x, e<-x]

var13 = [(a,b,c) | a<-p, b<-p, c<-p] where p = [1..3]

dvojce = [(x,x^2-1)| x<-[1..10]]

sudaC = [(x)| x<-[2,4..500], ((mod x 3)==0)&&((mod x 7)==0)]

var x = [(x) | x<-[-1,-2..(-x)]]

vetsi2 a b = if(a>b) then a else b
vetsi3 a b c = vetsi2 (vetsi2 a b) c