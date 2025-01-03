fun is_older (x : int*int*int, y : int*int*int) =
    if (#1 x) < (#1 y)
    then true
    else
	if (#1 x) = (#1 y)
	then if (#2 x) < (#2 y)
	     then true
	     else
		 if (#2 x) = (#2 y)
		 then if (#3 x) < (#3 y)
		      then true
		      else false
		 else false
	else false
		  
fun number_in_month (x : (int*int*int) list, y : int) =
    if null x
    then 0
    else
	let
	    val current = number_in_month(tl(x), y)
	in
	    if #2 (hd(x)) = y
	    then 1 + current
	    else current
	end
	(*
         use let..in to simplify the following program

        if #2 hd(x) = y
	then 1 + number_in_month(tl(x), y)
	else number_in_month(tl(x), y)

        *)

fun number_in_months (x : (int*int*int) list, y : int list) =
    if null y
    then 0
    else				    
	number_in_month(x, hd(y)) + number_in_months(x, tl(y))

fun dates_in_month (x : (int*int*int) list, y : int) =
    if null x
    then []
    else
	let
	    val current = dates_in_month(tl(x), y)
	in
	    if #2 (hd(x)) = y
	    then hd(x) :: current
	    else current
	end
	(*			
	if hd(x) = y
	then hd(x) :: dates_in_month(tl(x), y)
	else dates_in_month(tl(x), y)
	  *)

fun dates_in_months (x : (int*int*int) list, y : int list) =
    let
	fun append (x: (int*int*int) list, y: (int*int*int) list) =
	    if null x
	    then y
	    else
		hd(x) :: append(tl(x), y)
    in	  
	if null y
	then []
	else
	    append(dates_in_month(x, hd(y)), dates_in_months(x, tl(y)))
    end
			   
	
fun get_nth (x : string list, y : int) =
    if y = 1
    then hd(x)
    else
	get_nth(tl(x), y - 1)
	

fun date_to_string (x: int*int*int) =
    let
	val month_string = ["January", "February"," March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	val y = Int.toString(#1 x)
	val m = get_nth(month_string, #2 x)
	val d = Int.toString(#3 x)
    in
	m^" "^d^", "^y
    end
	
fun number_before_reaching_sum (x : int, y : int list) =
    if x <= hd(y)
    then 0
    else
	1 + number_before_reaching_sum(x - hd(y), tl(y))

				      
fun what_month (x : int) =
    let
	val cnt_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(x, cnt_days) + 1
    end
	

fun month_range (x: int, y: int) =
    if y < x
    then []
    else
	what_month(x) :: month_range(x+1, y)
	
			    
fun oldest (x: (int*int*int) list) =
    if null x
    then NONE
    else
	let
	    val current = oldest(tl(x))
	in
	    if isSome(current) andalso is_older(valOf(current), hd(x))
	    then current
	    else SOME(hd(x))
	end

fun check_duplicates (x: int, y: int list) =
    if null y
    then false
    else
	if x <> hd(y) andalso check_duplicates(x, tl(y)) = false
	then false
	else
	    true

fun remove_duplicates (y: int list) =
    if null y
    then []
    else
	let
	    val current = remove_duplicates(tl(y))
	in
	    if check_duplicates(hd(y), current)
	    then current
	    else
		hd(y)::current
	end

fun number_in_months_challenge (x : (int*int*int) list, y : int list) =
    let
	val y_new = remove_duplicates(y)
    in
	if null y_new
	then 0
	else				    
	    number_in_month(x, hd(y_new)) + number_in_months(x, tl(y_new))
    end
	
	
fun dates_in_months_challenge (x : (int*int*int) list, y : int list) =
    let
	fun append (x: (int*int*int) list, y: (int*int*int) list) =
	    if null x
	    then y
	    else
		hd(x) :: append(tl(x), y)
	val y_new = remove_duplicates(y)
    in
	if null y_new
	then []
	else
	    append(dates_in_month(x, hd(y_new)), dates_in_months(x, tl(y_new)))
    end	

fun append x y =
    case x of
	[] => y
      | a::b => a::(append b y)

		  
