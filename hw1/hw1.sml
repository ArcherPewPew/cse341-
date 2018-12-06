fun is_older(d1 : int * int * int, d2 : int * int * int) = 
    if (#3 d1) < (#3 d2)
    then true
    else 
        if (#3 d1) = (#3 d2)
        then 
            (#2 d1) < (#2 d2) orelse ((#2 d1) = (#2 d2) andalso (#3 d1) < (#3 d2))
        else false

fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else 
        let 
            val num = number_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then 1 + num 
            else num
        end

fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else 
        let
            val ds = dates_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then hd dates :: ds 
            else ds
        end

fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strs : string list, n : int) =
    if n = 1
    then hd strs
    else
        get_nth(tl strs, n - 1)

fun date_to_string(date : int * int * int) =
    let
        val names = ["January", "February", "March", "April", "May", "June", "July", "August",
        "September", "October", "November", "December"]
    in
        get_nth(names, #2 date)^"-"^Int.toString(#1 date)^"-"^Int.toString(#3 date)
    end

fun number_before_reaching_sum(nums : int list, sum : int) =
    if hd nums >= sum
    then 0
    else 1 + number_before_reaching_sum(tl nums, sum - (hd nums))

fun what_month(day : int) = 
    let
        val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(days, day)
    end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2) 
    
fun oldest(dates : (int * int * int) list) = 
    if null dates
    then NONE
    else
        let
            val d = oldest(tl dates)
        in
            if isSome d andalso is_older(valOf d, hd dates) 
            then d
            else SOME(hd dates) 
        end

fun cumulative_sum(nums : int list) =
    let
        fun helper(nums : int list, prev : int) =
            if null nums
            then []
            else hd nums + prev :: helper(tl nums, hd nums + prev)
    in
        helper(nums, 0)
    end

fun no_duplicate(nums : int list) =
    let 
        fun is_in(num : int, nums : int list) =
            if null nums
            then false
            else hd nums = num orelse is_in(num, tl nums) 
    in
        if null nums
        then []
        else 
            if is_in(hd nums, tl nums)
            then no_duplicate(tl nums)
            else hd nums :: no_duplicate(tl nums)
    end

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
    let
        val months = no_duplicate(months) 
    in
        number_in_months(dates, months) 
    end

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
    let
        val months = no_duplicate(months)
    in
        dates_in_months(dates, months)
    end

fun reasonable_date(date : int * int * int) =
    let
        val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun is_leap(year : int) =
            year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)            
            
        fun days_of_month(month : int, days : int list) = 
            if month = 1
            then hd days
            else days_of_month(month - 1, tl days)
    in
        #3 date > 0 andalso (#2 date >= 1 andalso #2 date <= 12) andalso (
            if #2 date = 2 andalso is_leap(#3 date)
            then #1 date > 0 andalso #1 date <= 29
            else #1 date > 0 andalso #1 date <= days_of_month(#2 date, days)
        )
    end
