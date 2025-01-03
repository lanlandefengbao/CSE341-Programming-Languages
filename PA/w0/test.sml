
fun null xs =
    let
	val x = hd xs
    in
	fn x => false
    end
	

fun append xs ys =
    if xs = []
    then ys
    else
	(hd xs)::(append (tl xs) ys)

signature TEST =
sig
    type mytype
    val f: int -> int
		      

structure test :> TEST =
struct
datatype mytype = A of int 
		| B of string
fun f x = 0
end
    
    
	      

		  
