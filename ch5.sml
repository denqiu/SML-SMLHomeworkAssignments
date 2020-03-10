(*1.*)
fun cube x = x * x * x;

(*2.*)
fun cuber x:real = x * x * x;

(*3.*)
fun fourth f = if length f = 4 then hd (rev f) else fourth (rev (tl (rev f)));

(*4.*)
fun min3(a, b, c) = let fun min(a, b) = if a <= b then a else b in min(min(a, b), c) end;

(*5.*)
fun red3 (a, b, c) = (a, c);

(*6.*)
fun thirds t = let fun getThird array = if length array = 3 then hd (rev array) else getThird (rev (tl (rev array))) in getThird(explode t) end;

(*7.*)
fun cycle1 c = rev ((hd c) :: rev (tl c));

(*8.*)
fun sort3(a:real, b:real, c:real) = if a > b then sort3(b, a, c) else (if b > c then sort3(a, c, b) else [a, b, c]);

(*9.*)
fun del3 d = let fun isThird(n, d) = if n = 3 then tl d else hd d :: isThird(n+1, tl d) in isThird(1, d) end;

(*10.*)
fun sqsum n = if n < 0 then 0 else n*n + sqsum(n-1);

(*11.*)
fun cycle (c, n) = if n = 0 then c else cycle(cycle1 c, n-1);

(*12.*)
fun pow(a:real, b) = if b > 0 then a * pow(a, b-1) else 1.0;

(*13.*)
fun max x = let fun maxHelper(a, b) = if null a then b else (if hd a > b then maxHelper(tl a, hd a) else maxHelper(tl a, b)) in maxHelper(tl x, hd x) end;

(*14.*)
fun isPrime p = let fun checkPrime(c) = if p mod c = 0 then false else (if c * c >= p then true else checkPrime(c+1)) in if p > 2 then checkPrime(2) else (if p = 2 then true else false) end;

(*15.*)
fun minMax m = 
	let
		fun compute(min, max, m) = if null m then (min, max) else compute((if hd m <= min then hd m else min), (if hd m > max then hd m else max), tl m)
	in 
		compute(hd m, hd m, m)
	end;
fun remove(r, h) = if null h then [] else (if hd h = r then tl h else hd h :: remove(r, tl h));
fun contains(k, c) = if null c then false else (if k = hd c then true else contains(k, tl c));
val r = Random.rand(1,1);
fun shuffleIndexes(i, j, stop) = 
	let
		val m = if null i then stop else Random.randRange(minMax i) r
	in
		if m = stop then j else (if contains(m, j) = true then shuffleIndexes(i, j, stop) else shuffleIndexes(remove(m, i), m :: j, stop))
	end;
fun shuffledIndexes(i, n, stop) = if n = 0 then i else shuffledIndexes(shuffleIndexes(i,[], stop),n-1, stop);
fun shuffledList(a, i, s) = 
	let 
		fun shuffleList(b, j, t) = if null t then [] else (if null b andalso null j then shuffleList(a, i, t) else (if hd j = hd t then hd b :: shuffleList(a, i, tl t) else shuffleList(tl b, tl j, t)))
	in
		shuffleList(a, i, s)
	end;
fun select(s, f) =
	let 
		val filter = (List.filter f) s
		fun listIndexes i = let fun getIndexes g = if g = 1 then [1] else g :: getIndexes(g-1) in rev(getIndexes(length i)) end
		val indexes = listIndexes filter
		val m = minMax indexes
		val getShuffledIndexes = shuffledIndexes(indexes, Random.randRange(m) r, (#2 m)+1)
	in
		shuffledList(filter, indexes, getShuffledIndexes)
	end;