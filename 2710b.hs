ot [] ak = ak
ot (h:t) ak = ot t (h:ak) 
otoc text = ot text []

spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

smazP p [] = []
smazP p (h:t) = if(h==p) then t else h:(smazP p t)

smazAll p [] = []
smazAll p (h:t) = if (p==h) then smazAll p t else h:(smazAll p t)

smazZC [] = []
smazZC (h:t) =  if (h>0) then h:(smazZC t) else smazZC t

smazNSM m [] = []
smazNSM m (h:t) = if((mod m 2)==0) then smazNSM (m+1) t else h:(smazNSM (m+1) t)

smazNM m [] = []
smazNM m (h:t) = if(h>m) then smazNM m t else h:(smazNM m t)

sS [x] [y] = x*y
sS (h:t) (h1:t1) = (h*h1) + (sS t t1)

kombinuj [] [] = []
kombinuj (h:t) (h1:t1) = h:(h1:(kombinuj t t1))

fSum [] = 0
fSum (h:t) = h+(fSum t)

indexPVP i [] = 0
indexPVP i (h:t) = if( h == i ) then indexPVP i [] else 1 + (indexPVP i t)

pocetPVP [] = 0
pocetPVP (h:t) = 1 + (pocetPVP t)

ntyPrvek 0 (h:_) = h
ntyPrvek n (h:t) = ntyPrvek (n-1) t

isPrvek p [] = False
isPrvek p (h:t) = if (p==h) then True else isPrvek p t

posledniPrvek [x] = x
posledniPrvek (h:t) = posledniPrvek t

pPP [x,y] = x
pPP (h:t) = pPP t

pP = (*0.05)

osmina = (*0.125)

zvys02 = (+2)

umocniNa6 = (^6)

umocni6 = (6^)

zvysVS f [] = []
zvysVS f (h:t) = (f h):(zvysVS f t)

var15 = let x = [1..5] in [ [x1,x2,x3,x4,x5] | x1 <- x, x2 <- x, x3 <- x, x4 <- x, x5 <- x]

var13 = [[a,b,c] | a <- p, b <- p, c <- p ] where p = [1..3]

dvojice = [[x,x^2-1] | x <- [1..10]]

sada = [[x] | x <- [1..500], ((mod x 2)==0) && ((mod x 3)==0) && ((mod x 7)==0)]