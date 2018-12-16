(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare

(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports),
   medium_incident_reports (100 reports), and large_incident_reports
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
(*use "parsed_medium_police.sml";*)

(* uncomment when you are ready to do the problems needing the large report*)
(*use "parsed_large_police.sml";

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")

*)
(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)
(* 问题1-8 完成*)
fun make_silly_json i =
  let
    fun make_json_list i = 
      if i = 0
      then []
      else (Object [("n", Num (int_to_real i)), ("b", True)]) :: make_json_list (i-1)
  in
    Array (make_json_list i)
  end 

fun assoc (k, xs) =
  case xs of 
      [] => NONE
    | (k', v) :: xs' => if k' = k then SOME v else assoc (k, xs')
  
fun dot (j, f) =
  case j of 
     Object xs => assoc (f, xs)
    | _ => NONE

fun one_fields j =
  case j of 
     Object xs => let
      fun helper (xs, acc) =
        case xs of 
          [] => acc
          | (s, _) :: xs' => helper(xs', s::acc)
     in
      helper (xs, [])
     end
    | _ => []

fun no_repeats sl = length(sl) = length(dedup sl)

fun recursive_no_field_repeats j =
  let
    fun helper1 xs =
      case xs of 
        [] => true
        | j :: xs' => recursive_no_field_repeats j andalso helper1 xs'
    fun helper2 xs =
      case xs of
        [] => true
        | (_, j) :: xs' => recursive_no_field_repeats j andalso helper2 xs'
  in
    case j of 
      Array xs => helper1 xs
      | Object xs => no_repeats(one_fields (Object xs)) andalso helper2 xs
      | _ => true
  end

fun count_occurrences (sl, ex) =
  let
    fun helper (sl, flag, cs, cc, acc) = 
      case sl of
        [] => (cs, cc) :: acc
        | s::sl' => if s = cs 
                    then helper(sl', flag, cs, cc+1, acc) 
                    else if flag = EQUAL orelse flag = strcmp(s, cs)
                         then helper(sl', strcmp(s, cs), s, 1, (cs, cc)::acc)
                         else raise ex
  in
    case sl of 
      [] => []
      |s::sl' => helper(sl', EQUAL, s, 1,[])
  end

fun string_values_for_field (s, jl) =
  case jl of 
    [] => []
    |j::jl' => case dot (j, s) of
                SOME (String v) => v :: string_values_for_field(s, jl')
                |_ => string_values_for_field(s, jl')
(*
fun filter_field_value (s1, s2, jl) =
  case jl of
    [] => []
    |j::jl' => case dot (j, s1) of
                SOME (String v) => if v = s2 
                                   then j::filter_field_value(s1, s2, jl')
                                   else filter_field_value(s1, s2, jl')
                |_ => filter_field_value(s1, s2, jl')


(* histogram and historgram_for_field are provided, but they use your
   count_occurrences and string_values_for_field, so uncomment them
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))


(**** PUT PROBLEMS 9-11 HERE ****)
val large_event_clearance_description_histogram = histogram_for_field("event_clearance_description", large_incident_reports_list)
val large_hundred_block_location_histogram = histogram_for_field("hundred_block_location", large_incident_reports_list)
val forty_third_and_the_ave_reports = filter_field_value("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list)
val forty_third_adn_the_ave_event_clearance_description_histogram = histogram_for_field("event_clearance_description", forty_third_and_the_ave_reports)

val nineteenth_and_forty_fifth_reports = filter_field_value("hundred_block_location", "45XX BLOCK OF 19TH AVE NE", large_incident_reports_list)
val nineteenth_and_forty_fifth_event_clearance_description_histogram = histogram_for_field("event_clearance_description", nineteenth_and_forty_fifth_reports)
*)
;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)


;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)

(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

fun concat_with (sep, sl) =
  case sl of
    [] => ""
    |s::[] => s
    |s1::(s2::sl') => s1^sep^concat_with(sep, s2::sl') 

fun quote_string s = concat_with(s, ["\"","\""])

fun real_to_string_for_json r = if real_is_negative r then "-"^real_to_string(real_abs r) else real_to_string r

fun json_to_string js = 
  let
    fun helper arr =
      case arr of
        [] => []
        |js::arr' => json_to_string js::helper arr'
    fun helper1 sjl =
      case sjl of
        [] => []
        |(s, j)::sjl' => ((quote_string s) ^ " : " ^ (json_to_string j)) :: helper1 sjl'
  in
  case js of 
    Num r => real_to_string_for_json r
    |String s => quote_string s
    |False => "false"
    |True => "true"
    |Null => "null"
    |Array jl => "["^concat_with(", ", helper jl)^"]"
    |Object ob => "{"^concat_with(", ", helper1 ob)^"}"
  end