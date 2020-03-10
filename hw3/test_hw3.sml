use "sml_hw3.sml";

fun match(nil,nil) = true
|match(nil, _) = false
|match(_, nil) = false
|match(f1::r1, f2::r2) = Real.==(f1,f2) andalso match(r1,r2)

fun pass_fail x = if x then "Pass" else "\t\tFail";

fun test(function, input, expected_output) = pass_fail ((function(input) = expected_output));

fun test_match(function, input, expected_output) = pass_fail (match(function(input),expected_output));

fun error_print (n, f) = print (Int.toString(n)^"."^f^": Error encountered in test of " ^f^"\n");

print("*********TEST RESULTS******************\n");
print("\n1.duplist: "^test(duplist, [1,3,2], [1,1,3,3,2,2])^"\n") handle _ => error_print (1, "duplist");
print("\n2.duplist: "^test(duplist, [], [])^"\n") handle _ => error_print (2, "duplist");
print("\n3.duplist: "^test(duplist, ["apple","ball","cat"], ["apple","apple","ball","ball","cat","cat"])^"\n") handle _ => error_print (3, "duplist");
print("\n4.mylength: "^test(mylength, [1,2,3], 3)^"\n") handle _ => error_print(4, "mylength");
print("\n5.mylength: "^test(mylength, [1,2,3,4,5], 5)^"\n") handle _ => error_print(5, "mylength");
print("\n6.mylength: "^test(mylength, [], 0)^"\n") handle _ => error_print(6, "mylength");
print("\n7.il2absrl: "^test_match(il2absrl, [~1, ~3], [1.0, 3.0])^"\n") handle _ => error_print(7, "il2absrl");
print("\n8.il2absrl: "^test_match(il2absrl,[3,17,~9,34,~7,2],[3.0,17.0,9.0,34.0,7.0,2.0])^"\n") handle _ => error_print(8, "il2absrl") ;
print("\n9.il2absrl: "^test_match(il2absrl,[~3,17,9,~34,~7,2],[3.0,17.0,9.0,34.0,7.0,2.0])^"\n") handle _ => error_print(9, "il2absrl");
print("\n10.myimplode: "^test(myimplode,explode("hello"), "hello")^"\n") handle _ => error_print(10, "myimplode");
print("\n11.myimplode: "^test(myimplode,explode("abracadabra"),"abracadabra")^"\n") handle _ => error_print(11, "myimplode");
print("\n12.myimplode: "^test(myimplode,explode(""),"")^"\n") handle _ => error_print(12, "myimplode");
print("\n13.lconcat: "^test(lconcat, [[1,2],[3,4],[4,5,5]], [1,2,3,4,4,5,5])^"\n") handle _ => error_print(13, "lconcat");
print("\n14.lconcat: "^test(lconcat, [[1,2,3,4,3,5],[5]], [1,2,3,4,3,5,5])^"\n") handle _ => error_print(14, "lconcat");
print("\n15.lconcat: "^test(lconcat, [[1],[2]], [1,2])^"\n") handle _ => error_print(15, "lconcat");
print("\n16.lconcat: "^test(lconcat, [[],[1],[2,3]], [1,2,3])^"\n") handle _ => error_print(16, "lconcat");
print("\n17.lconcat: "^test(lconcat, [[1,2,3,4,5]], [1,2,3,4,5])^"\n") handle _ => error_print(17, "lconcat");
print("\n18.convert: "^test(convert,[(1,3),(5,19),(7,4)],([1,5,7],[3,19,4]))^"\n") handle _ => error_print(18, "convert");
print("\n19.convert: "^test(convert,[(1,2),(3,4)],([1,3],[2,4]))^"\n") handle _ => error_print(19, "convert");
print("\n20.convert: "^test(convert,[("hello","good"),("there","bye")],(["hello","there"],["good","bye"]))^"\n") handle _ => error_print(20, "convert");

let
val sqList = mymap (fn x=>x*x);
in
print("\n21.mymap:sqList: "^test(sqList,[1,2,3,4,5], [1,4,9,16,25])^"\n") handle _ => error_print(22, "mymap:sqList")
end;

let
val incList = mymap (fn x=>x+1);
in
print("\n22.mymap:incList: "^test(incList,[1,2,3,4,5,6], [2,3,4,5,6,7])^"\n") handle _ => error_print(23, "mymap:incList")
end;

let
val decList = mymap (fn x=>x-1);
in print("\n23.mymap:decList: "^test(decList,[1,2,3,4,5,6], [0,1,2,3,4,5])^"\n") handle _ => error_print(24, "mymap:decList")
end;

let
val sumList = myfoldl (fn(a,b) =>a+b) 0;
in
print("\n24.myfoldl:sumList: "^test(sumList,[1,3,7,9,22,38], 80)^"\n") handle _ => error_print(25, "myfoldl:sumList")
end;

let
val subList = myfoldl (op -) 10;
in
print("\n25.myfoldl:subList: "^test(subList,[1,3,7,9,22,38], 30)^"\n") handle _ => error_print(25, "myfoldl:sumList")
end;

