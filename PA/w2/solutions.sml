(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (x, y) =
    let fun check_existence (target, lst) =
	    case lst of
		[] => false
	      | a :: b => if a = target then true else check_existence(target, b)

	fun build_lst (target, lst, acc) =
	    case lst of
			[] => SOME(acc)
		|a :: b => if target = a
			   then build_lst(target, b, acc)
			   else build_lst(target, b, a :: acc)	    
	    
	val existence = check_existence(x, y)
    in
	if existence
	then build_lst(x, y, [])
	else NONE
    end


fun get_substitutions1 (lst_lst, target) =
	case lst_lst of
	    a :: b => (case all_except_option(target, a) of
			   NONE => get_substitutions1(b, target)
			 |SOME r => r @ get_substitutions1(b, target))
	   |[] => []			   

fun get_substitutions2 (lst_lst, target) =
    let fun recu (lst_lst, target, acc) =
	    case lst_lst of
		[] => acc
	       |a :: b => (case all_except_option(target, a) of
			       NONE => recu(b, target, acc)
			     | SOME r => recu(b, target, r@acc))
    in recu(lst_lst, target, [])
    end
	
fun similar_names (lst_lst, full_names) =
    let
	val {first = x, middle = y, last = z} = full_names
	val subs = get_substitutions2(lst_lst, x)
	fun recu (lst, target, acc) =
	    case lst of
		[] => acc
	      | a :: b => recu(b, target, {first = a, middle = y, last = z} :: acc)
    in
	full_names :: recu(subs, x, [])
    end
	
		
	    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (c) =
    case c of
	(Clubs,_) => Black
     | (Spades,_) => Black
     | (Diamonds,_) => Red
     | (Hearts,_) => Red
		    
fun card_value (suit, rank) =
    case rank of
	Ace => 11
     | Jack => 10
     | Queen => 10
     | King => 10
     | Num x => x  

fun remove_card (cs, c, exp) =
    let fun cards_remain (cs, c, exp, acc) = 
	    case cs of
		[] => raise exp
	      | a :: b => (if c <> a then cards_remain(b, c, exp, acc@[a]) else acc@b)
    in cards_remain(cs, c, exp, [])
    end
							  
fun all_same_color (cs) =
    case cs of
     c :: c' :: c'' => if card_color(c) = card_color(c') then all_same_color(c' :: c'') else false											  | _  => true
									  
fun sum_cards (cs) =
    let	fun accumulator (lst, acc) =
	    case lst of
		[] => acc
	      | a::b => accumulator(b, acc+card_value(a))
    in
	accumulator(cs, 0)
    end
			 
fun score (hand, goal) =
    let
	val sum = sum_cards(hand)
	val preliminary_score = if sum > goal then 3*(sum-goal) else goal - sum
    in
	if all_same_color(hand)
	then preliminary_score div 2
	else
	    preliminary_score
    end

	
fun officiate (c_lst, m_lst, goal) =
    let fun moving (c_left, m_left, hand) =
	    if sum_cards(hand) > goal
	    then hand
	    else
		case m_left of
		    [] => hand
		  | a :: b => (case a of
				   Discard x => moving(c_left, b, remove_card(hand, x, IllegalMove))
				 | Draw => (case c_left of
						[] => hand
					      | c :: d => moving(d, b, c :: hand)))
    in
	score(moving(c_lst, m_lst, []), goal)
    end 

