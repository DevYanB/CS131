  let rec print_list_int mylist = match mylist with
  | [] -> print_endline "This be the end of the int list boii"
  | head::body ->
  begin
  print_int head;
  print_endline "";
  print_list_int body
  end
  ;;

val getElem : 'a list -> 'a -> bool = <fun>
val subset : 'a list -> 'a list -> bool = <fun>
val equal_sets : 'a list -> 'a list -> bool = <fun>
val set_union : 'a list -> 'a list -> 'a list = <fun>
val set_intersection : 'a list -> 'a list -> 'a list = <fun>
val set_diff : 'a list -> 'a list -> 'a list = <fun>
val computed_fixed_point : ('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> 'a =
  <fun>