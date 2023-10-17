-- variace 1...5 
variace = [ [a,b,c,d,e] | a <- [1..5], b <- [1..5], c <- [1..5], d <- [1..5], e <- [1..5]]
variace1 = [[a,b,c] | a<-[1..3], b<-[1..3], c<-[1..3]]
variace3 = [[a,b,c,d,e,f] | a <- [1..49], b <- [1..49], c <-[1..49], d<- [1..49], e<-[1..49], f<-[1..49]]

--spoj
spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)

-- smaz zaporny cisla ze seznamu
smazZapornyCisla [] = []
smazZapornyCisla (h:t) = if( h < 0 ) then smazZapornyCisla(t)  else spoj [h] ( smazZapornyCisla t)

-- smaz vše na sudých místech
smazVseNaSudychMistech [] c = []
smazVseNaSudychMistech (h:t) c = if( (mod c 2) == 0 ) then smazVseNaSudychMistech(t) (c+1) else spoj [h] (smazVseNaSudychMistech t (c+1))

smazVseNadMez [] m = []
smazVseNadMez (h:t) m = if( h > m ) then spoj [h] (smazVseNadMez(t) m) else smazVseNadMez(t) m

sumaSeznamu [] = 0
sumaSeznamu (h:t) = h + sumaSeznamu t 

indexPrvkuVPoli [] p = 0
indexPrvkuVPoli (h:t) p = if(h == p) then 0 else 1 + indexPrvkuVPoli t p  

pocetPrvkuVPoli [] = 0
pocetPrvkuVPoli (_:t) = 1 + pocetPrvkuVPoli t

ntyPrvek 1 (h:_) = h
ntyPrvek n (h:t) = ntyPrvek (n-1) t 

isPrvekHere p [] = False
isPrvekHere p (h:t) = if ( p == h) then True else isPrvekHere p t

posledniPrvek [x] = x
posledniPrvek (_:t) = posledniPrvek t

predposledniPrvek [x,y] = x
predposledniPrvek (h:t) = predposledniPrvek t