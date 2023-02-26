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
let rec cherche a elem = match a with
| Vide -> false
| N(_,x,g,d) -> if x = elem then true else cherche (if elem < x then g else d) elem;;
let rec hauteur tree = match tree with
| Vide -> -1
| N(delta,_,g,d) -> 1 + hauteur (if delta == M then g else d);;
let rec repare_MM a = match a with
| N(M,x,N(Z,y,ll,lr),r) -> (N(P,y,ll,N(M,x,lr,r)),true)
| N(M,x,N(M,y,ll,lr),r) -> (N(Z,y,ll,N(Z,x,lr,r)),false)
| N(M,x,N(P,y,ll,N(M,c,lrl,lrr)),r) -> (N(Z,c,N(Z,y,ll,lrl),N(P,x,lrr,r)),false)
| N(M,x,N(P,y,ll,N(P,c,lrl,lrr)),r) -> (N(Z,c,N(M,y,ll,lrl),N(Z,x,lrr,r)),false)
| N(M,x,N(P,y,ll,N(Z,c,lrl,lrr)),r) -> (N(Z,c,N(Z,y,ll,lrl),N(Z,x,lrr,r)),false)
| _ -> raise (IllegalArgumentException 0);;
let rec repare_PP a = match a with
| N(P,x,l,N(Z,z,rl,rr)) -> (N(M,z,N(P,x,l,rl),rr),true)
| N(P,x,l,N(P,z,rl,rr)) -> (N(Z,z,N(Z,x,l,rl),rr),false)
| N(P,x,l,N(M,z,N(M,c,rll,rlr),rr)) -> (N(Z,c,N(Z,x,l,rll),N(P,z,rlr,rr)),false)
| N(P,x,l,N(M,z,N(P,c,rll,rlr),rr)) -> (N(Z,c,N(M,x,l,rll),N(Z,z,rlr,rr)),false)
| N(P,x,l,N(M,z,N(Z,c,rll,rlr),rr)) -> (N(Z,c,N(Z,x,l,rll),N(Z,z,rlr,rr)),false)
| _ -> raise (IllegalArgumentException 0);;

let rec print_int_list l = match l with
| [] -> Printf.printf "\n"
| x::next -> Printf.printf "%d " x; print_int_list next;;

let test_tree = N(P,50,N(Z,10,Vide,Vide),N(M,100,N(Z,75,N(Z,60,Vide,Vide),N(Z,80,Vide,Vide)),N(Z,120,Vide,Vide))) in Printf.printf "%b\n" (verifie_AVL (let (a,b) = repare_PP test_tree in a));;