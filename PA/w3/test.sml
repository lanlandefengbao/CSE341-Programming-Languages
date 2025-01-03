fun filter(f, xs) =
    case xs of
	[] => []
	   | x :: xs' => if f x then x :: (filter(f, xs')) else filter(f, xs')

val i  = 10
fun test (xs, x) =
    let
	val i = (print "!"; String.size x)
    in
	filter(fn a => String.size a < i, xs)
    end
fun test2 (xs, x) =
    filter(fn a => String.size a < (print "!"; String.size x), xs)

fun test3 i = (Real.fromInt o abs) i
val test4 = Real.fromInt o abs

fun fold1 (f, lst, acc) =
    case lst of
	[] => acc
      | x::x' => fold1(f, x', f(x, acc))

val rec fold2 = fn f => fn lst => fn acc => case lst of [] => acc
						      | x::x' => fold2 f x' (f acc x')
fun test f(x,y) = f x y
								  
									  
fun f y =
    let
	val y = 3
    in
	y+y
    end
	
fun fold f acc xs =
    case xs of
	[] => acc
      | x :: x' => fold f (f(x,acc)) x'

fun howmany xs n =
    fold (fn(x,acc) => if x mod n = 0 then acc+1 else acc) 0 xs

	 
	
