ot [] ak = ak
ot (h:t) ak = ot t (h:ak)
otoc text = ot text []

spoj [] s2 = s2 
spoj (h:t) s2 = h:(spoj t s2)

smazPV p [] = []
smazPV p (h:t) = if(p == h) then t else h:(smazPV p t)

smazVV p [] = []
smazVV p (h:t) = if (h==p) then smazVV p t else h:(smazVV p t)

smazZC [] = []
smazZC (h:t) = if(h>0) then h:(smazZC t) else smazZC t

smazVNSM p [] = []
smazVNSM p (h:t) = if((mod p 2)==0) then smazVNSM (p+1) t else h:(smazVNSM (p+1) t)

smazVNM m [] = []
smazVNM m (h:t) = if(h>m) then smazVNM m t else h:(smazVNM m t)

sS [x] [y] = x*y
sS (h:t) (h1:t1) = (h*h1) + (sS t t1)

komb [] [] = []
komb (h:t) (h1:t1) = h:(h1:(komb t t1))

fSum [] = 0
fSum (h:t) = h + (fSum t)

inPVP p [] = 0
inPVP p (h:t) = if(h==p) then inPVP p [] else 1+(inPVP p t)

pPVP [] = 0
pPVP (h:t) = 1+(pPVP t)

ntyP 0 (h:_) = h
ntyP n (_:t) = ntyP (n-1) t

isPH p [] = False
isPH p (h:t) = if (p==h) then True else isPH p t

posP [x] = x
posP (h:t) = posP t

predP [x,y] = x
predP (h:t) = predP t

petP = (*0.05)

osmina = (*0.125)

zvysO2 = (+2)

umocniNa6 = (^6)

umocni6 = (6^)

zvysVS f [] = []
zvysVS f (h:t) = (f h):(zvysVS f t)

var15 = let x = [1..5] in [[a1,a2,a3,a4,a5] | a1<-x, a2<-x, a3<-x, a4<-x, a5<-x]

var13 = [[a,b,c] | a<-p, b<-p, c<-p] where p = [1..3]

dvojce = [[x,x^2-1] |  x <- [1..10]]

sudaC = [[x] | x<-[1..500], ((mod x 2)==0)&&((mod x 3)==0)&&((mod x 7)==0)]
