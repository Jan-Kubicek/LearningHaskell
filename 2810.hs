ot [] ak = ak
ot (h:t) ak = ot t (h:ak)
otoc text = ot text []

spoj [] s = s
spoj (h:t) s = h:(spoj t s)

smazP p [] = []
smazP p (h:t) = if(h==p) then t else h:(smazP p t)

smazV p [] = []
smazV p (h:t) = if(h==p) then smazV p t else h:(smazV p t)

smazZ [] = []
smazZ (h:t) = if(h>0) then h:(smazZ t) else smazZ t

smazNSM m [] = []
smazNSM m (h:t) = if((mod m 2)==0) then smazNSM (m+1) t else h:(smazNSM (m+1) t)

smazNM m [] = [] 
smazNM m (h:t) = if(h>m) then smazNM m t else h:(smazNM m t)

sS [x] [y] = x*y
sS (h:t) (h1:t1) = (h*h1) + (sS t t1)

z2S [] [] = []
z2S (h:t) (h1:t1) = h:(h1:(z2S t t1))

fSum [] = 0
fSum (h:t) = h + (fSum t)

iPVP i [] = 0
iPVP i (h:t) = if(h == i) then iPVP i [] else 1+(iPVP i t)

pPVP [] = 0
pPVP (h:t) = 1+(pPVP t)

ntyP 0 (h:_) = h
ntyP n (h:t) = ntyP (n-1) t

iPH p [] = False
iPH p (h:t) = if(h==p) then True else iPH p t

pP [x] = x
pP (h:t) = pP t

pPP [x,y] = x
pPP (h:t) = pPP t

pPro = (*0.05)

osmina = (*0.125)

umocniNa6 = (^6)

zvysO2 = (+2)

umocni6 = (6^)

zvysVS f [] = []
zvysVS f (h:t) = (f h):(zvysVS f t)

var15 = let a = [1..5] in [[x1,x2,x3,x4,x5]| x1<-a, x2<-a, x3<-a, x4<-a, x5<-a]

var13 = [[a,b,c]| a<-p,b<-p,c<-p] where p = [1..3]

dvojice = [[x,x^2-1]| x<- [1..10]]

sC = [[x]| x<-[1..500], ((mod x 2)==0)&&((mod x 3)==0)&&((mod x 7)==0)]