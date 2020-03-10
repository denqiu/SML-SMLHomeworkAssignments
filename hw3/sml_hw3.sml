(* SML comments appear like this *)
(* Dennis Qiu *)

(* #1 - duplist - maps [a,a] to transform each element to a list containing two elements where a = mapped element. Then foldr b@c merges all elements into one list, in which all elements of the mapped list are all lists. Thus, resulting in a list where [a,a,b,b,...].*)
fun duplist x = foldr(fn(b,c)=>b@c)nil(map(fn a=>[a,a])x);

(* #2 - mylength - replaces each element as 1 using map function and then using foldl op+ to add all mapped elements, resulting in the length of the list.*)
fun mylength x = foldl(op+)0(map(fn i=>1)x);

(* #3 - il2absrl - maps each integer to real using real r. if r is negative, multiply -1 to convert sign of integer to positive.*)
fun il2absrl x = map(fn r=>real(if r < 0 then r * ~1 else r))x;

(* #4 - myimplode - first maps each char to string, then foldr op^ to append all strings correctly.*)
fun myimplode x = foldr(op^)""(map(fn c=>str(c))x);

(* #5 - lconcat - literally same method used to foldr list of lists in duplist function.*)
fun lconcat x = foldr(fn(a,b)=>a@b)nil x;

(* #6 - convert - map functions maps each tuple reduced to either the first or second element, in which the the first and second elements of each tuple corresponds to left and right.*)
fun convert x = (map(fn(left,right)=>left)x,map(fn(left,right)=>right)x);

(* #7 - mymap - folds the list as (a,b) and performs the given function on "a" while appending b (::b) to maintain the list.*)
fun mymap f x = foldr(fn(a,b)=>f(a)::b)nil x;

(* #8 - myfoldl - "case someList of nil=>... | first::rest=>..." mimics fun someFunction nil = nil | someFunction(first::rest) = first::someFunction rest. In this case, if aList is nil returns initialValue else if aList contains some number of elements we call myfoldl recursively, in which aList becomes rest and initialValue is some function performed on first(same as hd aList) and ends with initialValue.*)
fun myfoldl f initialValue aList = case aList of nil=>initialValue | first::rest=>myfoldl f (f(first,initialValue)) rest;