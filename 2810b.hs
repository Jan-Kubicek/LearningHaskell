ot [] ak = ak
ot (h:t) ak = ot t (h:ak)
otoc text = ot text []

spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

smazVV t [] = []
smazVV t (h:ta) = if(h==t) then smazVV t ta else h:(smazVV t ta)

smazPV p [] = []
smazPV p (h:t) = if(h==p) then t else h:(smazPV p t)

smazZC [] = []
smazZC (h:t) = if(h>0) then h:(smazZC t) else smazZC t

smazNSM m [] = []
smazNSM m (h:t) = if((mod m 2)==0) then smazNSM (m+1) t else h:(smazNSM (m+1) t)

smazVNM m [] = []
smazVNM m (h:t) = if(h>m) then smazVNM m t else h:(smazVNM m t)

sS [x] [y] = x*y
sS (h:t) (h1:t1) = (h*h1) + (sS t t1)

komb [] [] = []
komb (h:t) (h1:t1) = h:(h1:(komb t t1))

fSum [] = 0
fSum (h:t) = h+ (fSum t)

iPVP i [] = 0
iPVP i (h:t) = if(h==i) then iPVP i [] else 1+(iPVP i t)

pPVP [] = 0
pPVP (h:t) = 1+(pPVP t)

nty 0 (h:_) = h
nty n (_:t) = nty (n-1) t

isPH p [] = False
isPH p (h:t) = if(h==p) then True else isPH p t

posP [x] = x
posP (h:t) = posP t

predP [x,y] = x
predP (h:t) = predP t

preP = (*0.05)

osmina = (*0.125)

zvysO2 = (+2)

umocniNa6 = (^6)

umocni6 = (6^)

aplFNS f [] = []
aplFNS f (h:t) = (f h):(aplFNS f t)

var15 = let x = [1..5] in [[a1,a2,a3,a4,a5]| a1<-x, a2<-x, a3<-x, a4<-x, a5<-x]

var13 = [[a,b,c]| a<-p, b<-p, c<-p ] where p = [1..3]

dvojice = [[x,x^2-1]| x <- [1..10]]

sudC = [[x]| x <- [1..500], ((mod x 2)==0)&&((mod x 3)==0)&&((mod x 7)==0)]