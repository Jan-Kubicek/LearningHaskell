nasobeni a b = a * b
cel x = x/(5/9) + 32  -- z F => C
far x = (5/9)*(x-32)  -- z C => F 
absolute a = if(a < 0) then a * (-1) else a
negation a = if(a) then False else True
myAnd a b = if( ( a  == True )  &&  ( b == True ) ) then True else False  
myOr a b = if(( a == True ) || ( b == True)) then True else False