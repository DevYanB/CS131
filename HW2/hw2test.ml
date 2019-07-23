type test_nonterms =
  | Sentence | NP | VP | Preposition | Noun

(* This is a grammar with many rules and the returned derivation must be the first possible one *)
let test_grammar = 
  (Sentence,
  function
    | Sentence -> [[N VP; T "thiq"]; [N NP; T "thiq"]; [N NP; N VP; T "thiq"]]
    | VP -> [[N Preposition]]
    | NP -> [[N Noun]]
    | Preposition -> [[T "close to"]]
    | Noun -> [[T "booty"]])
;;

let t = ["booty"; "close to" ; "thiq"]
;;

let accept_all string = Some string;;
;;

let make_matcher_test = ((make_matcher test_grammar accept_all t) = Some [])
;;

let make_parser_test = match make_parser test_grammar t with 
| Some tree -> parse_tree_leaves tree = t
| _ -> false
;;

