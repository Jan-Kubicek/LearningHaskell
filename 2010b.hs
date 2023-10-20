spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

otoc [] = []
otoc (h:t) = spoj (otoc t ) [h]

sVZP [] = []
sVZP (h:t) = if(h >= 0) then spoj [h] (sVZP t) else sVZP t 

sVNSM [] i = []
sVNSM (h:t) i = if( (i > 1) && ((mod i 2)==0) ) then sVNSM t (i+1) else spoj [h] (sVNSM t (i+1))

sVNM [] m = []
sVNM (h:t) m = if(h > m) then sVNM t m else spoj [h] (sVNM t m)

k2S [] [] = []
k2S (h:t) (h1:t1) = spoj [h] (spoj [h1] (k2S t t1))

sS [] [] = 0
sS (h:t) (h1:t1) = (h*h1) + (sS t t1)

fSum [] = 0
fSum (h:t) = h+(fSum t)

iPVP [] p = 0
iPVP (h:t) p = if (h == p ) then iPVP [] p  else 1+( iPVP t p )

pPVP [] = 0
pPVP (h:t) = 1+(pPVP t)

nP (h:_) 0 = h
nP (h:t) p = nP t (p-1)

iPH [] p = False
iPH (h:t) p = if(h == p ) then True else iPH t p

lP [x] = x
lP (h:t) = lP t

pPP [x,y] = x
pPP (h:t) = pPP t

nasobeni a b = a * b

s = let x = [1..3] in [ [a,b,c] | a <- x, b<-x, c<-x]
s1 = [[x,x^2-1]| x <- [1..10]]
s2 = [x | x<-[1..500], ((mod x 2)==0) && ((mod x 3)==0) && ((mod x 7)==0)]