(* Some basic definitions out of the way first! *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let left (x,_) = x;;
let right (_,x) = x;;


(* Question 1: converting from HW1 type grammar to HW2 type grammar *)
let rec convert_rules g1 retGram pl = match g1 with
| [] -> retGram
| g1Val::g1Rest -> match g1Val with
| (nt, ts) -> 
if nt = pl then convert_rules g1Rest (retGram@[ts]) pl
else convert_rules g1Rest retGram pl 
;;

let convert_grammar gram1 = match gram1 with
| (startExp, grammarRules) -> (startExp, convert_rules grammarRules [])
;;


(* Question 2: reads parse tree left to right and prints out the leaves *)
let parse_tree_leaves t = 
(* need to differentiate b/t node and leaf *)
(* Problem is that With only one function, it can't get a list, only returning one *)
(* I realize I need a go thru in order to be able to add to list elems instead of just going through returning list *)
let rec go_thru = function
| [] -> []
| hd::tl -> (helper hd) @ (go_thru tl)
and helper = function
| Leaf x -> [x]
| Node (dc, next) -> (go_thru next)
in go_thru [t]
;;


(* Question 3: make_matcher gram that returns a matcher for the grammar gram *)
let rec go_thru_nonterminals overall_gram rule acceptor frag = match rule with
| [] -> None (* IF grammar is empty, of course, return none *)
(* This is what gives the check for an iteration, if there is a match just return that *)
(* Else go through part_rule_check for the rest of the grammar section *)
| hd::tl -> (match part_rule_check overall_gram hd acceptor frag with 
    | None -> go_thru_nonterminals overall_gram tl acceptor frag
    | r -> r)

(* Function for part_rule_check looks this way to keep track but also check for specific rule *)
and part_rule_check overall_gram rule acceptor frag = match rule with 
(* if empty just return the acceptor function with the fragment passed to it*)
| [] -> acceptor frag 
(* Otherwise go through the passed frag *)
| _ -> (match frag with 
   | [] -> None
   | hd::tl -> (match rule with
        |[] -> None
        (* If this is a terminal, if it matches within the frag, then continues on, else returns none *)
        |(T termn)::rem -> if hd = termn then (part_rule_check overall_gram rem acceptor tl) else None
	(* IF nonterminal, then has to take into account the nonterminal and go through while tracking this value *)
	(* This was a confusing logic to understand, but (overall_gram noneterm) gets the section in the grammar *)
	(* containing the nonterminal rule. The rest is getting the acceptor and checking with the remaining. *)
	(* Had to guess and check 3rd term from idea of going through rest of the rule and check with frag(s)*)
        (* While still keeping track of previous *)
        |(N nontermn)::rem -> (go_thru_nonterminals overall_gram (overall_gram nontermn) 
                              (part_rule_check overall_gram rem acceptor) frag)))
;;

let make_matcher gram = match gram with
(* grammar startExp essentially goes to the location in the grammar where startExp is *)
| (startExp, grammar) -> fun acceptor frag -> go_thru_nonterminals grammar (grammar startExp) acceptor frag
;;


(* Question 4: make_parser gram that returns a parser for the grammar gram *)
(* Realized I had to change up the above a little bit, so need to rewrite it here...super inefficient but whatever *)

let rec mod_go_thru_nonterminals overall_gram rule acceptor frag = match rule with
| [] -> None
| hd::tl -> (match mod_part_rule_check overall_gram hd acceptor frag with 
    | None -> mod_go_thru_nonterminals overall_gram tl acceptor frag
    | Some r -> Some (hd::r)) (* THIS IS ALL I HAD TO REPLACE *)

and mod_part_rule_check overall_gram rule acceptor frag = match rule with 
| [] -> acceptor frag 
| _ -> (match frag with 
   | [] -> None
   | hd::tl -> (match rule with
        |[] -> None
        |(T termn)::rem -> if hd = termn then (mod_part_rule_check overall_gram rem acceptor tl) else None
        |(N nontermn)::rem -> (mod_go_thru_nonterminals overall_gram (overall_gram nontermn) 
                              (mod_part_rule_check overall_gram rem acceptor) frag)))
;;


let rec along_tree root rules = 
match root with 
| [] -> (rules, []) 
| hd::tl -> (match (down_tree hd rules) with 
    | (a,b) -> (match along_tree tl a with 
        | (y,z) -> (y, b::z))) 
and down_tree root rules =
match root with 
| (T cur) -> (match rules with 
    | [] -> ([], Leaf cur) 
    | hd::tl -> (hd::tl, Leaf cur)) 
    | (N cur) -> (match rules with 
        | [] -> ([], Node (cur, [])) 
        | hd::tl -> (match along_tree hd tl with 
            | (a,b) -> (a, Node (cur, b))))
;; 

let only_empty suffix = match suffix with
| [] -> Some []
| _ -> None
;;

let make_parser gram = match gram with
| (startExp, grammar) -> fun frag -> match (mod_go_thru_nonterminals grammar (grammar startExp) only_empty frag) with
| None -> None 
| Some [] -> None 
| Some x -> (match along_tree [N startExp] x with 
    | (_,t) -> (match t with 
        | [] -> None 
      	| hd::tl -> Some hd))
;;
