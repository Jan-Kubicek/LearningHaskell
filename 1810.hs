-- seznam
    -- SPOJ (funkční)
spoj [] s2 = s2
spoj (h:t) s2 = h:(spoj t s2)
    -- SMAZ ZAPORNY CISLA Z SEZNAMU (funkční)
smazZapornyCisla [] = []
smazZapornyCisla (h:t) = if(h >= 0) then spoj [h] (smazZapornyCisla t) else smazZapornyCisla t
    -- SMAZ VSE NA SUDYCH POZICICH (funkční)
smazVseNaSudychMistech [] s = []
smazVseNaSudychMistech (h:t) s = if((mod s 2) == 0 ) then smazVseNaSudychMistech t (s+1) else spoj [h] (smazVseNaSudychMistech t (s+1) )
    -- SMAZ VSE NAD MEZ (funkční)
smazVseNadMez [] m = []
smazVseNadMez (h:t) m = if( h > m ) then spoj [h] (smazVseNadMez t m) else smazVseNadMez t m
    -- SKALARNÍ SOUČIN (funkční)
skalarniSoucin [] [] = 0
skalarniSoucin (h:t) (h2:t2) = (h*h2) + skalarniSoucin t t2
    -- Kombinace 2 seznamů (funkční)
kombinaceSeznamu [] [] = []
kombinaceSeznamu (h:t) (h2:t2) = spoj [h] (spoj [h2] (kombinaceSeznamu t t2))
    -- SUMA seznamu (funkční)
sumaSeznamu [] = 0
sumaSeznamu (h:t) = h + (sumaSeznamu t)
    -- INDEX PRVKU V POLI (funkční)
indexPrvkuVPoli [] p = 0
indexPrvkuVPoli (h:t) p = if( h == p ) then 1 else 1+ (indexPrvkuVPoli t p)
    -- POCET PRVKU V POLI (funkční)
pocetPrvkuVPoli [] = 0
pocetPrvkuVPoli (h:t) = 1 + (pocetPrvkuVPoli t)
    -- JE PRVEK V POLI (funkční)
isPrvekHere [] p = False
isPrvekHere (h:t) p = if(h == p) then True else isPrvekHere t p
    -- POSLEDNÍ PRVEK (funkční)
lastPrvek [x] = x
lastPrvek (h:t) = lastPrvek t
    -- PREDPOSLEDNÍ PRVEK (funkční)
predposledniPrvek [x,y] = x
predposledniPrvek (h:t) = predposledniPrvek t

-- částečná aplikace
    --Funkce petProcent
nasobeni a b = a * b
-- v ghci > petProcent = nasobeni 0.05
-- v ghci > osmina = nasobeni 0.125

-- generování
seznam1 = let rozmezi = [1..5] in [[a,b,c,d,e] | a <- rozmezi, b <- rozmezi, c <- rozmezi, d <- rozmezi, e <- rozmezi]
seznam2 = [[x, x^2-1] | x <- [1..10]]
seznam3 = [ x | x <- [1..500], ((mod x 3) == 0) && ((mod x 7) == 0) ]