spoj [] s2 = s2
spoj (h:t) s2= h: spoj t s2

otoc s2 = ot s2 [] 
ot [] akumulator = akumulator
ot (h:t) akumulator = ot t (h:akumulator)

smazVsechnyZapornyCisla [] = []
smazVsechnyZapornyCisla (h:t) = if(h >= 0 ) then h : smazVsechnyZapornyCisla t else smazVsechnyZapornyCisla t

smazPrvniVyskyt a [] = []
smazPrvniVyskyt a (h:t) = if( h == a ) then t else h: smazPrvniVyskyt a t

smazVsechnyVyskyty a [] = []
smazVsechnyVyskyty a (h:t) = if( h == a ) then smazVsechnyVyskyty a t else h: (smazVsechnyVyskyty a t)

smazVseNaSudychMistech p [] = []
smazVseNaSudychMistech p (h:t) = if ( (mod p 2 ) == 0 ) then smazVseNaSudychMistech (p+1) t else h: smazVseNaSudychMistech (p+1) t

smazVseNadMez m [] = []
smazVseNadMez m (h:t) = if (m < h) then smazVseNadMez m t else h: smazVseNadMez m t

skalarniSoucin [x] [y] = x*y
skalarniSoucin (h:t) (h1:t1) = (h*h1) + skalarniSoucin t t1

kombinaceSeznamu [] [] = []
kombinaceSeznamu (h:t) (h1:t1) = h:(h1:(kombinaceSeznamu t t1))

sumSeznamu [] = 0
sumSeznamu (h:t) = h + sumSeznamu t

indexPrvkuVPoli p [] = 0
indexPrvkuVPoli p (h:t) = if ( h == p ) then indexPrvkuVPoli p [] else 1 + indexPrvkuVPoli p t

pocetPrvkuVPoli [] = 0
pocetPrvkuVPoli (h:t) = 1 + pocetPrvkuVPoli t

ntyPrvek 0 (h:_) = h 
ntyPrvek n (h:t) = ntyPrvek (n-1) t

isPrvekHere n [] = False
isPrvekHere n (h:t) = if ( n == h ) then True else isPrvekHere n t

posledniPrvek [x] = x
posledniPrvek (h:t) = posledniPrvek t

predposledniPrvek [x,y] = x
predposledniPrvek (h:t) = predposledniPrvek t

petProcent = (*0.05)
osmina = (*0.125)
zvysO2 = (+2)
umocniNa6 = (^6)
umocni6 = (6^)

aplikujFunkciNaSeznam f [] = []
aplikujFunkciNaSeznam f (h:t) = f h : aplikujFunkciNaSeznam f t

variace1 = let p = [1..5] in [[a,b,c,d,e] | a <- p, b <- p, c <- p, d <- p, e <- p]

variace2 = let p = [1..3] in [[a,b,c] | a <- p, b <- p, c <- p]

dvojice = [[x,x^2-1] | x <- [1..10]]

sudaCisla = [ [x] | x <- [1..500], ((mod x 2) == 0) && ((mod x 3) == 0 ) && ((mod x 7) == 0)]