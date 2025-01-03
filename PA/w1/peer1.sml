
fun is_older (first : (int*int*int), second : (int*int*int)) =
	(#1 first * 365) + (#2 first * 30) + #3 first < (#1 second * 365) + (#2 second * 30) + #3 second


fun number_in_month (dates_list : (int*int*int) list, month : int) =
	if null (dates_list)
	then 0
	else
		if #2 (hd dates_list) = month
		then 1 + number_in_month(tl dates_list, month)
		else number_in_month(tl dates_list, month)


fun number_in_months (dates_list : (int*int*int) list, months : int list) =
	if null (months)
	then 0
	else
		number_in_month(dates_list, hd months) + number_in_months(dates_list, tl months)


fun dates_in_month (dates_list : (int*int*int) list, month : int) =
	if null (dates_list)
	then []
	else
		if #2 (hd dates_list) = month
		then hd dates_list :: dates_in_month(tl dates_list, month)
		else dates_in_month(tl dates_list, month)


fun dates_in_months (dates_list : (int*int*int) list, months : int list) =
	if null (months)
	then []
	else
		dates_in_month(dates_list, hd months) @ dates_in_months(dates_list, tl months)

fun get_nth (string_list : string list, n : int) =
	if n = 1
	then hd string_list
	else get_nth(tl string_list, n - 1)

fun date_to_string (date : (int*int*int)) =
	let val months_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(months_list, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

fun number_before_reaching_sum (sum : int, number_list : int list) =
	let fun search_list(i : int, list_sum : int, vs : int list) = 
	    if list_sum + hd vs >= sum
	    then i
	    else search_list(i + 1, list_sum + hd vs, tl vs)
    in 
	search_list(0, 0, number_list)
    end


fun what_month (day : int) =
	let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
		number_before_reaching_sum(day, days_in_months) + 1
	end

fun month_range (first : int, second : int) =
	if first > second
	then []
	else
		(what_month(first)) :: month_range(first + 1, second)

fun oldest (dates_list : (int*int*int) list) =
	if null (dates_list)
	then NONE
	else let
		fun older_date(dates_list : (int*int*int) list) =
			if null (tl dates_list)
			then hd dates_list
			else let val ans = older_date(tl dates_list)
				in
					if is_older(hd dates_list, ans)
					then hd dates_list
					else ans
				end
	in
		SOME (older_date(dates_list))
	end;

