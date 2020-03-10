(* SML comments appear like this *)
(* Dennis Qiu *)

(* #1 - pow - given a, as long as b > 0 then a *= b else 1*)
fun pow (a, b) = if b = 0 then 1 else a * pow(a, b-1);


(* #2 - sumTo - given x, use real to transform int x to decimal for sml to accept 1.0 / x.0 else 1*)
fun sumTo x = if x = 0 then 0.0 else (1.0/real(x)) + sumTo(x-1);
               
               
(* #3 - repeat - ^ appends text so given s, as long as n > 0 s += s else "" *)
fun repeat (s, n) = if n = 0 then "" else s^repeat(s, n-1);


(* #4 - binary - x mod 2 returns the bit representation whether 0 or 1 and x div 2 continues until 0. Basically like doing division by hand. Int.toString is a built-in function to convert an int to string. 0*)
fun binary x = if x = 0 then "0" else binary(x div 2)^Int.toString(x mod 2);

(* #5 - countNegative - checks if element is positive or negative. if positive return 0 else 1 and continues adding regardless of whether it adds 0 or 1, until tl x is empty. *)
fun countNegative x = if null x then 0 else (if hd x > 0 then 0 else 1) + countNegative(tl x);


(* #6 - absList - contains two helper functions called checkPositve and positiveTuple. checkPositive checks if a number in the tuple is negative and if so multiply by -1 to become positive. positiveTuple will just simply execute checkPositive twice for the first and second values. map is a built-in function to map functions to a list. *)
fun absList x = let fun checkPositive(check) = if check < 0 then check * ~1 else check fun positiveTuple (a, b) = (checkPositive(a), checkPositive(b)) in if null x then [] else List.map (positiveTuple) x end;


(* #7 - split - divides element by 2 using divBy2 which always halves the element and is one less than the element minus this result. splitElement replaces each element with a tuple of its halves. ex 5 div 2 returns 2 which is one less than 3. *)
fun split x = let fun divBy2(d) = d div 2 fun splitElement(n) = (divBy2(n), n - divBy2(n)) in if null x then [] else List.map (splitElement) x end;


(* #8 - isSorted - checkSort checks the first and second element and if first <= second then continues to iterate through the list otherwise false. Returns true if list is empty or has one element. In other words returns true if the length of the list is 0 or 1, which we can further simplify to if length x <= 1 then true else checkSort or if length x > 1 then checkSort else true. *)
fun isSorted x = let fun getElement(list) = hd list fun checkSort(list) = getElement(list) <= getElement(tl list) in if length x > 1 then (if checkSort(x) = true then isSorted(tl x) else false) else true end;

 
(* #9 - collapse - structure is very very similar to isSorted. :: allows an element to be appended to a list. getSum is structured like checkSort in which it is adding the elements instead of comparing them. tl (tl x) makes sure that we skip the first two elements so we don't accidentally use the second element. if the list is [] or [n] in which n is some number, then the list is returned. Thus we can keep appending until the last list in sequence has length <= 1. *) 
fun collapse x = let fun getElement(list) = hd list fun getSum(list) = getElement(list) + getElement(tl list) in if length x > 1 then getSum(x) :: collapse(tl (tl x)) else x end;

        
(* #10 - insert - first checks if list is sorted using isSorted. if true then the first parentheses places n inside list -> [n] if the list x is empty. The second parentheses checks if the first element of the list is less then n. if so then appends it to the next list -> tl x. We can simplify this to saying some number of elements <= n == a. b == tl x would always represent the list with the first element > n. Thus a :: n :: b. *)        
fun insert (n, x) = if isSorted x = true then (if null x then [n] else (if hd x <= n then hd x :: insert(n, tl x) else n :: x)) else x;
