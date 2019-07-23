(*Test Cases for"hw1.ml"*)

let my_subset_test0 = subset [] [1;5]
let my_subset_test1 = subset [2;4] [6;8;2;4]
let my_subset_test2 = subset [6;2;2] [2;4;6]

let my_equal_sets_test0 =  equal_sets [5;10;15] [15;10;5]
let my_equal_sets_test1 = not (equal_sets [1;3;2] [2;4;3])
let my_equal_sets_test2 = not (equal_sets [] [1])

let my_set_union_test0 = equal_sets (set_union [2;3;4] []) [2;3;4]
let my_set_union_test1 = equal_sets (set_union [1;3] [5;7]) [1;3;5;7]
let my_set_union_test2 = equal_sets (set_union [] []) []

let my_set_intersection_test0 = equal_sets (set_intersection [6;4;2] []) []
let my_set_intersection_test1 = equal_sets (set_intersection [2;4;10;3] [3;4;9]) [3;4]
let my_set_intersection_test2 = equal_sets (set_intersection [4;10] [4;8]) [4]

let my_set_diff_test0 = equal_sets (set_diff [6;4;2] []) []
let my_set_diff_test1 = equal_sets (set_diff [] []) []
let my_set_diff_test2 = equal_sets (set_diff [4;10] [4;8]) [10]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 3) 299 = 0

type nonterminals =
| A | B | C | D


let rules =
[ (A, [N B; N C; N D]);
(B, [N C; N D]);
(C, [N D]);
(D, [T "YE"])]

let b_grammar = (B, [(B, [N C; N D]); (C, [N D]); (D, [T "YE"])])
let c_grammar = (C, [(C, [N D]); (D,[T "YE"])])
let d_grammar = (D, [(D,[T "YE"])])

let my_filter_reachable_test0 = filter_reachable (B, rules) = b_grammar
let my_filter_reachable_test1 = filter_reachable (C, rules) = c_grammar
let my_filter_reachable_test2 = filter_reachable (D, rules) = d_grammar
