(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals x =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) x

(*
fun filter f x =
    case x of
	[] => []
      | a :: a' => if f a then a :: filter f a' else filter f a'
val test = filter (fn s => s > 1)
*)

(*for the 'fn' argument in foldl(), acc is by default its second argument*)
fun longest_string1 x =
	foldl (fn(s,acc) => if String.size s > String.size acc then s else acc) "" x

(* **f is the function that updating acc**
fun fold f acc lst =
    case lst of
	[] => acc
      | x::x' => fold f (f(x, acc)) x' 

val sumup = fold (fn(a,b) => a+b) 0 [1,2,3]		      

val test(a,b) => a+b;
*)
	
fun longest_string2 x =
	foldl (fn(s,acc) => if String.size s >= String.size acc then s else acc) "" x

fun longest_string_helper f lst =
    case lst of
	[] => ""
     | [a] => a
     | a :: a' :: a'' => if f(String.size a', String.size a)
			 then
			     longest_string_helper f (a'::a'')
			 else
			     longest_string_helper f (a::a'')

val longest_string3 = longest_string_helper (fn(s,acc) => s > acc)
val longest_string4 = longest_string_helper (fn(s,acc) => s >= acc)
    
fun longest_capitalized lst =
    (longest_string1 o only_capitals) lst

fun rev_string st =
    (String.implode o rev o String.explode) st

fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
      | x::x' => case f x of
	     SOME v => v
		   | _ => first_answer f x'
				       
fun all_answers f lst =
    let
	fun helper f acc lst =
	    case lst of
		[] => SOME acc 
	      | x::x' => case f x of
			     NONE => NONE
			  | SOME y => helper f (acc@y) x'
    in
	helper f [] lst
    end


fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

fun count_wildcards pattern =
    let
	fun is_wildcard () = 1
	fun is_variable x = 0				
    in
	g is_wildcard is_variable pattern
    end
	
fun count_wild_and_variable_lengths pattern =
    let
	fun is_wildcard () = 1
	fun is_variable x = String.size x
    in
	g is_wildcard is_variable pattern
    end

fun count_some_var (sp: string*pattern) =
    let
	val s = #1 sp
	val p = #2 sp
	fun is_wildcard () = 0
	fun is_variable x = if x = s then 1 else 0
    in
	g is_wildcard is_variable p
    end
	
fun all_vars p =
    case p of
	Variable x => [x]
      | TupleP x => foldl (fn(s,i) => (all_vars s) @ i) [] x
      | ConstructorP (_,x)  => all_vars x
      | _ => []

fun check_duplicates lst =
    case lst of
	[] => true
      | x::x' => if List.exists (fn y => y = x) x' then false else check_duplicates x' 
		 
fun check_pat pattern =
    let
	val candidates = all_vars pattern
    in
	check_duplicates candidates
    end
	
fun match (v,p) =
    case (v,p) of
	(_,Wildcard) => SOME []
     | (_,Variable s) => SOME [(s,v)]
     | (Unit,UnitP) => SOME []
     | (Const j,ConstP i)  => if i = j then SOME [] else NONE
     | (Tuple y, TupleP x) => if length x = length y
			      then all_answers match (ListPair.zip(y,x))
			      else NONE
     | (Constructor (s2, v), ConstructorP (s1, p)) => if s1 = s2
						      then match(v,p)
						      else NONE
     | _ => NONE
				       
				     
fun first_match v ps =
    SOME(first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE
    
