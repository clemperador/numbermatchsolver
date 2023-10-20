let rec liste_alea c = match c with 
  | 0 -> []
  | t -> (1+Random.int(9))::liste_alea(t-1);;

let rec rm c l = match l with (*Generates a random matrix of c columns and l rows.*)
| 0 -> []
| t -> (liste_alea c)::rm c(t-1);;

open Printf;;


let pm mat = (* Prints a list of lists as a matrix.*)
  List.iter (
    fun x -> List.iter (printf "%d ") x;print_newline()
    ) mat;;

let valid i j = 
  if i + j = 10 || i = j then true else false;;

let mpdf = 
  [[8;6;7;5;8;5;2;1;2];[1;5;2;9;3;1;4;3;4];[7;6;3;4;8;5;6;9;2]]
;;

let element mat i j = (* Finds element (i,j) on a matrix*)
  if j < 0 || i < 0 ||i >= List.length(mat) ||
    j >= List.length ((List.nth mat i)) then 
       -1
  else (List.nth (List.nth mat i) (j))
;;

let ligne mat j = List.nth mat j;;

let colonne mat j =
  let rec aux x = 
    if x = List.length(mat) then [] else
    let l = List.nth(mat) (x) in
    (List.nth (l) (j))::aux (x+1)
  in aux 0
;;

let next mat ix jx= (* Finds eastern neighbour. Checks previous rows if needed.*)
  let n = (List.length mat) in 
  let m = List.length (List.nth mat ix) in 
  let rec l i j =
  if i = n then -1,(-2,-2) else
    if j = m then l (i+1) 0
    else if element mat i j = 0 then l i (j+1) else 
      (element mat i j),(i,j)
    in l ix (jx+1)
;;

let previous mat ix jx= (*Finds western neighbour. Checks following rows if needed.*)
  let m = List.length (List.nth mat ix) in 
  let ix = if jx = 0 then (ix-1) else ix in let jx = if jx = 0 then (m-1) else (jx-1) in
  let rec l i j =
  if i = -1 then -1,(-2,-2) else
    if j = -1 then l (i-1) (m-1)
    else if element mat i j = 0 || element mat i j = -1 then l i (j-1) else 
      (element mat i j),(i,j)
    in l ix jx
;;

let neighbours mat i j = (*Returns list of neighbours in all 8 directions and its coordinates, starting from top left clockwise. -1 if not in matrix Ignores zeroes*)
  let rec v1 i1 j1 = 
    if element mat (i-i1) (j-j1) = -1 then [] 
    else if element mat (i-i1) (j-j1) = 0 then v1 (i1+1) (j1+1) 
    else [(element mat (i-i1) (j-j1), (i-i1,j-j1))] in
  let rec v2 i1 j1 =
    if element mat (i-i1) (j) = -1 then [] 
    else if element mat (i-i1) (j) = 0 then v2 (i1+1) (j1+1)
    else [(element mat (i-i1) (j), (i-i1, j))] in
  let rec v3 i1 j1 =
    if element mat (i-i1) (j+j1) = -1 then []
    else if element mat (i-i1) (j+j1) = 0 then v3 (i1+1) (j1+1) 
    else [(element mat (i-i1) (j+j1), (i-i1, j+j1))] in
  let v4 =
    if fst(next mat i j) = -1 then [] 
    else [next mat i j] in
  let rec v5 i1 j1 =
    if element mat (i+i1) (j+j1) = -1 then [] 
    else if element mat (i+i1) (j+j1) = 0 then v5 (i1+1) (j1+1) 
    else [element mat (i+i1) (j+j1), (i+i1,j+j1)] in
  let rec v6 i1 j1 =
    if element mat (i+i1) (j) = -1 then [] 
    else if element mat (i+i1) (j) = 0 then v6 (i1+1) (j1+1) 
    else [element mat (i+i1) (j), (i+i1,j)] in
  let rec v7 i1 j1 =
    if element mat (i+i1) (j-j1) = -1 then [] 
    else if element mat (i+i1) (j-j1) = 0 then v7 (i1+1) (j1+1) 
    else [element mat (i+i1) (j-i1), (i+i1,j-j1)] in
  let v8= if fst(previous mat i j) = -1 then [] else [previous mat i j] in
  v1 1 1@v2 1 1@v3 1 1@v4@v5 1 1@v6 1 1@v7 1 1@v8
;;

let compatibles mat i j = (* Checks compatibility for every neighbour of a given case.*)
  let x = element mat i j in 
  let rec aux l =  match l with
  | [] -> []
  | t::q -> if valid (fst t) x then t::aux q else aux q
  in aux (neighbours mat i j)
;;

let replace l pos a  = 
List.mapi (fun i x -> if i = pos then a else x) l;;
let switch mat j i x = 
  List.mapi (fun k e -> if k = j then replace (List.nth mat j) i x else e) mat;;
;;    

let remaining mat = (* Returns list of unmatched numbers.*)
  let n = (List.length mat) in 
  let rec l i j = 
  if i = n then [] else
    let m = List.length (List.nth mat (i)) in 
    if j = m then l (i+1) 0
    else if element mat i j <> 0 && element mat i j <> -1 then element mat i j::(l i (j+1)) else l (i) (j+1)
    in l 0 0
;;

let remove a b mat = (* a and b are coordinates, their respective cells are replaced by zeroes. (Matched)*)
  let i1 = fst a in let j1 = snd a in let i2 = fst b in let j2 = snd b in
  if (valid (element mat i1 j1) (element mat i2 j2)) = false then
    assert false
  else let mat2 = switch mat i1 j1 0 in switch mat2 i2 j2 0
;;

let possiblematchesred mat = (* Returns all erasable couples.*)
  let n = (List.length mat) in
  let m = List.length (List.nth mat 0) in
  let rec l i j =
    if i = n then [] else
      if j = m then l (i+1) 0
      else if List.length (compatibles mat i j) = 0 then l i (j+1) else 
        let rec aux l = match l with
        | [] -> []
        | t::q -> ((i,j), snd(t))::(aux q)
      in (aux (compatibles mat i j))@(l i (j+1))
    in l 0 0
;;

let rec redundancy x l = match l with (* For x = (a,b), checks if (b,a) is in l.*)
| [] -> false
| t::q -> if fst(x) = snd(t) && fst(t) = snd(x) then true else redundancy x q;;

let posmatches mat = (* Sends all compatible couples on a given matrix, omitting any rendundancies.*)
  let rec aux l = match l with
  | [] -> []
  | t::q -> if redundancy t l then aux q else t::(aux q)
in aux (possiblematchesred mat)
;;

let rec won mat = (*Checks if game has been won.*)
  let rec aux r = match r with
  | [] -> true
  | t::q -> if t <> 0 && t <> -1 then false else aux q
in aux (remaining mat);;

let rec lupto l i = match l with (* Returns l splitted in two at index i*)
  | [] -> [],[]
  | t::q -> if i = -1 then ([],t::q) else (t::fst(lupto q (i-1))),snd(lupto q (i-1))
;;

let rec removeline mat i = match mat with
| [] -> [[]]
| t::q -> if i = 0 then q else t::(removeline (q) (i-1))
;;

let rec removeuseless mat = match mat with
| [] -> []
| t::q -> if (List.for_all (fun x -> x = -1) (t)) || 
  List.for_all (fun x -> x = 0) t ||
 t = [] then removeuseless q else t::(removeuseless q)
;;

let to_add mat l = 
  let mat = removeuseless mat in
  let rec aux i r =   
  let c = (List.length (List.nth mat 0)) in
  match r with
  | [] -> [[]]
  | t::q -> if i = 0 then [(t::q)] else (fst (lupto r (c-1)))::(aux (i-1) (snd(lupto r (c-1)))) 
  in aux (List.length(remaining mat) / List.length (List.nth mat 0)) l
;;

let addnum mat = (* Appends the unmatched numbers at the end of the matrix, respecting its structure.*)
  let mat = removeuseless mat in
  let li = List.nth mat (List.length mat -1) in
  let c = List.length (List.nth mat 0) in
  let j = c - List.length li in
  let adnum = to_add mat (snd (lupto (remaining mat) (j-1))) in
  (removeline mat (List.length mat-1))@[li@(fst ((lupto (remaining mat) (j-1))))]@adnum;;
  

let rec removeall mat = (* Removes all compatible matches, searching by index.*)
  let aux mat lc =
  if lc = [] then mat else
    removeall (remove (fst(List.hd(lc))) (snd(List.hd(lc))) mat)
  in aux mat (posmatches mat)
;;

let rec removeallrand mat = (*Removes all compatible matches, searching randomly.*)
  let aux mat lc =
    if lc = [] then mat else
      let r = Random.int (List.length lc) in
      removeall (remove 
      (fst(List.nth lc r)) (snd(List.nth lc r)) mat) 
 in aux mat (posmatches mat)
;;

let rec randgame matrix = (* Plays number match, randomly. Returns number of times 'removeall' or 'addnum' was applied.*)
  let mat = matrix in
  let rec aux a c mat =
    let mat = removeuseless mat in
    pm mat; print_newline();
    if won mat then (c) else
    if a = 0 then begin print_string("==== RETRY ===="); print_newline(); (c+randgame(matrix)); end
    else if posmatches mat = [] then
      aux (a-1) (c+1) (addnum mat)
    else aux (a) (c+1) (removeallrand mat)
  in aux (List.length(matrix)*List.length(List.nth matrix 0)/6) 0 (mat)
;;

let fullgame matrix = (*Plays number match, by index. Returns number of times 'removeall' or 'addnum' was applied.*)
  let mat = matrix in
  let rec aux a c mat=
  let mat = removeuseless mat in
  pm mat; print_newline();
  if won mat then (c)
  else if a = 0 then begin
  print_newline();
  print_string("===== RETRY ======");print_newline();
  (c+randgame(matrix)); end
  else if posmatches mat = [] then aux (a-1) (c+1) (addnum mat)
  else aux a (c+1) (removeall mat)
in 
aux (List.length(matrix)*List.length(List.nth matrix 0)/4) 0 (mat)
;;

let avg m n k = (* Plays number match, with k random matrices of m by n. Returns the average return value.*)
  let rec aux i a =
  if i = k then float_of_int(a) /. float_of_int(k) else
    begin print_newline();print_string ("============");print_newline();print_newline();
    let x = (fullgame (rm m n))
  in aux (i+1) (a+x)
    end;
  in aux 0 0
;;



