# #use "hw2test.ml";;
type test_nonterms = Sentence | NP | VP | Preposition | Noun
val test_grammar :
  test_nonterms * (test_nonterms -> (test_nonterms, string) symbol list list) =
  (Sentence, <fun>)
val t : (test_nonterms, string) parse_tree option =
  Some
   (Node (Sentence,
     [Node (NP, [Node (Noun, [Leaf "booty"])]);
      Node (VP, [Node (Preposition, [Leaf "close to"])]); Leaf "thiq"]))
# let checker opt = match opt with
  | Some c -> c
  ;;
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
None
val checker : 'a option -> 'a = <fun>
# checker t ;;
- : (test_nonterms, string) parse_tree =
Node (Sentence,
 [Node (NP, [Node (Noun, [Leaf "booty"])]);
  Node (VP, [Node (Preposition, [Leaf "close to"])]); Leaf "thiq"])
# let pass_to_parse = checket t;;
Error: Unbound value checket
Hint: Did you mean checker?
# let pass_to_parse = checker t;;
val pass_to_parse : (test_nonterms, string) parse_tree =
  Node (Sentence,
   [Node (NP, [Node (Noun, [Leaf "booty"])]);
    Node (VP, [Node (Preposition, [Leaf "close to"])]); Leaf "thiq"])
# parse_tree_leaves pass_to_parse;;
- : string list = ["booty"; "close to"; "thiq"]
#