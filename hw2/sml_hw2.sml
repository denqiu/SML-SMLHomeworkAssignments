(* SML comments appear like this *)
(*Dennis Qiu*)

(* #1 - quicksort. The partition function returns a tuple containing a list of integers less than the pivot and a list of integers greater than the pivot. The quick function then sorts the list, in which [all less than pivot] @ (pivot::[all greater than pivot]*)
fun quicksort x:int list = 
	let
		fun partition (pivot, nil) = (nil, nil) |
			partition (pivot, first::rest) =
				let 
					val (less, greater) = partition(pivot, rest)
				in 
					if first < pivot then (first::less, greater) else (less, first::greater)
				end
		fun quick nil = nil |
			quick [pivot] = [pivot] |
			quick (pivot::rest) =
				let
					val (less, greater) = partition(pivot, rest)
				in
					(quick less) @ (pivot::(quick greater))
				end
	in
		quick x
	end;

(* #2 - member. Basically does tl s if a match is not found. if tl s ends up nil, then it returns false, otherwise returns true because a match has been found and therefore element e is in list s*)
fun member (e, s) = let fun isMember(m, nil) = false | isMember(m, n::rest) = m = n orelse isMember(m, rest) in isMember(e, s) end;
               
               
(* #3 - returns the union of sets (lists) s1 and s2*)
(* You may assume that s1 and s2 do not have any duplicate elements.*)
(* The insert function inserts the given element e where for all the elements in the list, [all elements less than e]::e::[all elements greater than e]. If the list contains e, then the list is returned so there will be no duplicated elements in the list.*)
(* The setUnion function continues to iterate through the first list and uses the insert function to insert each element into list u. The line where nil is the third parameter would add an element from s1, which makes sure that all future list u's would remain sorted. Afterall, a list with one element inside is already sorted. If s1 is nil, then continue with s2 until s2 = nil. Once s1 and s2 = nil, return u, the union of s1 and s2 in sorted order.*)
fun union (s1, s2) = 
	let 
		fun insert(e, nil) = [e] | insert(e, first::rest) = if first = e then first::rest else (if first > e then e::first::rest else first::insert(e, rest))
		fun setUnion(nil, nil, u) = u |
			setUnion(nil, s2First::s2Rest, u) = setUnion([s2First], s2Rest, u) |
			setUnion(s1First::s1Rest, s2, nil) = setUnion(s1Rest, s2, [s1First]) |
			setUnion(s1First::s1Rest, s2, u) = setUnion(s1Rest, s2, insert(s1First, u))			
	in
		setUnion(s1, s2, nil)
	end;

(* #4 - returns the intersection of sets (lists) s1 and s2 *)
(* You may assume that s1 and s2 do not have any duplicate elements *)
(* The insert function is the same as the insert function in the union function, but without the equals checking. We don't need to check for we have another function to check.*)
(* The contains function returns true if element e is within the list, otherwise false.*)
(* Pretty much the same setup as the union function but with a conditional statement to check if both sets contain the same element.*)
fun intersection (s1, s2) = 
	let 
		fun insert(e, nil) = [e] | insert(e, first::rest) = if first > e then e::first::rest else first::insert(e, rest)
		fun contains(e, nil) = false | contains(e, first::rest) = if first = e then true else contains(e, rest)
		fun setIntersection(nil, s2, i) = i |
			setIntersection(s1First::s1Rest, s2, i) = setIntersection(s1Rest, s2, if contains(s1First, s2) then insert(s1First, i) else i)		
	in
		setIntersection(s1, s2, nil)
	end;

(* #5 - Return list of integers from start (inclusive) to stop (exclusive) by step - two variables called endValue and check are created to check when the last number in the range has been reached. endValue and check reverses their meaning when step is negative.*)
fun range(start, stop, step) = let val endValue = stop - (if step < 0 then ~1 else 1) val check = if step < 0 then start < endValue else start > endValue in if check then nil else (if start = endValue then [endValue] else start::range(start+step, stop, step)) end;

(* #6 - Return a slice of a list between indices start inclusive, and stop exclusive. setSlice adds an element to the new list if the condition index >= start and index < stop is true, otherwise the size of the list does not change. Checks for two cases, in which both return the new list if the list to iterate becomes nil or if the index = stop.*)
fun slice(aList, start, stop) = 
	let
		fun setSlice(nil, index, s) = s | 
			setSlice(first::rest, index, s) = if index = stop then s else setSlice(rest, index+1, if index >= start andalso index < stop then s @ [first] else s)
	in
		setSlice(aList, 0, nil)
	end;