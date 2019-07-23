(*Need to do a subset function first. To do that, I need a way to check if elem exists in a list *)
(*combine my test code of going through integer array with getElem*)
(* basically all it does is goes through each value of list and checks if it is same as elem. If not, it will continue through the rest of list checking *)
let rec getElem myList elem = match myList with
| [] -> false
| hd::tl ->
if hd = elem then true
else getElem tl elem
;;

(* Now, we can check wether a is a subset of b. If the element of current index in a is in b, do subset operation on next *)
let rec subset a b = match a with
| [] -> true
| hd::tl ->
if getElem b hd then subset tl b
else false
;;

(* Now to see if two sets are EXACTLY equal *)
let equal_sets a b = subset a b && subset b a
;;

(* Found this syntax from online docs, though this does not take care of shared values b/t sets  *)
let rec set_union a b = a @ b
;;

(* Ooh, cool one! Get the set intersection this time *)
(* For this one, had to think a bit and got some help online with syntax *)
(* Basically, adds head to a forming list as you recurse through elems and do same
for common and dont do for different *)
let rec set_intersection a b = match a with
| [] -> []
| hd::tl ->
if getElem b hd then hd::(set_intersection tl b)
else set_intersection tl b
;;

(*This is the reverse of the previous one, in a sense...*)
let rec set_diff a b = match a with
| [] -> []
| hd::tl ->
if getElem b hd then set_diff tl b
else hd::(set_diff tl b)
;;

(* Check that x and f(x) are equal by the definition.  *)
(* If not, go through and recursively check for x equal to f(f(f...(x))) *)
(* (f x) == f(x) *)
let rec computed_fixed_point eq f x = 
if eq x (f x) then x
else computed_fixed_point eq f (f x)
;;


(* Time for the big one, grammar parsing! *)

(* Some random typesetting for the test cases and for allowing flexibility *)
type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal
;;
let leftOfTup (r, _) = r;;
let rightOfTup (_, r) = r;;

(* A really basic parsing of the grammar rules passed to the function in a way that differentiates between terminal and non terminal *)
let rec parseNonT rules =
match rules with
| [] -> []
| N top::remRs -> (top)::(parseNonT remRs)
| T top::remRs -> parseNonT remRs
;;

(* This recursive function is gonna get us the list of all reachable/valid terminals and, in a way, non terminals *)
let rec buildParseTree currReachNonT rulesTup = match rulesTup with
| [] -> currReachNonT
| currRuleTup::remRuleTup ->
if (getElem currReachNonT (leftOfTup currRuleTup))
then buildParseTree (set_union currReachNonT (parseNonT (rightOfTup currRuleTup))) remRuleTup
else buildParseTree currReachNonT remRuleTup
;;

(* This sort of nested structure is what will validate the idea that the grammar is valid or invalid *)
(* If they are equal, it's valid and so return fs. Otherwise, continue and go through next exps till you get to the e\
nd/terminal symbols *)
let rec reachableNonT currReachNonT rulesTup =
let fs = (buildParseTree currReachNonT rulesTup) in
let ns = (buildParseTree fs rulesTup) in
if equal_sets fs ns then fs else reachableNonT ns rulesTup
;;

(* the first func that will give us the setup for removing unreachable grammer *)
(* need some way to filter: according to List, filter is there. needs to satisfy a predicate *)
(* f will represent the temp var for the process of removing invalid grammar rules *)
(* going off of the current reachable non-terminals *)
let rec validRules startExp grammarRules = 
let f = (reachableNonT [startExp] grammarRules) in
List.filter ( fun p -> getElem f (leftOfTup p )) grammarRules
;;

(* What this means is that if I'm given a grammar, with a start expression and list of grammar rules, *)
(* I want to return the start expression with a list of only the valid/reachable grammar rules *)
let filter_reachable g = match g with
| (startExp, grammar_Rules) -> (startExp, validRules startExp grammar_Rules)
;;
