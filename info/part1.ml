type delta = M | Z | P
type avl = Vide | N of delta * int * avl * avl
let parcours_infixe tree = let rec aux a acc = match a with
| Vide -> acc
| N(_,x,g,d) -> aux g (x::(aux d acc))
in aux tree [];;
let verifie_ABR tree = let rec is_sorted l = match l with
| [] -> true
| x::next -> match next with
    | [] -> true
    | xp::nextp -> x < xp && (is_sorted next)
in is_sorted (parcours_infixe tree);;
exception IllegalArgumentException of int
let verifie_equilibre tree = let map_value balance = match balance with
| -1 -> M
| 0 -> Z
| 1 -> P
| _ -> raise (IllegalArgumentException balance)
in let max a b = if a > b then a else b in let rec aux a = match a with
| Vide -> (true,-1)
| N(delta,_,g,d) -> let (left_state,left_height) = aux g in let (right_state,right_height) = aux d in (left_state && right_state && (try map_value (right_height - left_height) = delta with
    | IllegalArgumentException(_) -> false),(max left_height right_height) + 1)
in let (state,_) = aux tree in state;;
let verifie_AVL tree = verifie_ABR tree && verifie_equilibre tree;;