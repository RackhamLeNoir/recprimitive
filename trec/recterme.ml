(**************************************************************************)
(*  Copyright 2004 Pietrzak Thomas                                        *)
(*                                                                        *)
(*    This file is part of Trec.                                          *)
(*                                                                        *)
(*    Trec is free software; you can redistribute it and/or modify        *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    Trec is distributed in the hope that it will be useful,             *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with Trec; if not, write to the Free Software                 *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

#open "utils";;

exception en_forme_normale;;


(* calcule les variables libres d'un terme de De Bruijn *)
let libres_db t =
  let rec aux p = function
    | Zero -> []
    | Su tt -> aux p tt
    | Index i when i < p -> []
    | Index i -> [i - p]
    | Recur (t, b, s) -> (aux p t) @ (aux p b) @ (aux (p + 2) s)
in
aux 0 t;;


(* convertit un recterme de De Bruijn en recterme *)
let rt_of_rtdb t =
  let libres = libres_db t in
  let rec aux p k l liees = function
    | Zero -> RECZ
    | Su tt -> RECSucc (aux p k l liees tt)
    | Index i when i < p -> Var (var_of_int (nieme i liees))
    | Index i -> Var (var_of_int (i - p))
    | Recur (tt, b, s) ->
      if mem k libres or mem k liees then
        aux p (k + 1) l liees (Recur (tt, b, s))
      else if l <= k or mem l libres or mem l liees then
        aux p k (l + 1) liees (Recur (tt, b, s))
      else
        let t' = aux p (k + 1) (l + 1) liees tt
        and b' = aux p (k + 1) (l + 1) liees b
        and x = k
        and y = l
        in
        let s' = aux (p + 2) (k + 1) (l + 1) (y :: x :: liees) s
        in
        R (t', b', var_of_int x, var_of_int y, s')
  in
  aux 0 0 1 [] t;;


(* convertit un recterme en recterme de De Bruijn*)
let rtdb_of_rt t =
  let rec aux liees = function
    | RECZ -> Zero
    | RECSucc tt -> Su (aux liees tt)
    | Var v -> Index (try index v liees with Not_found -> list_length liees + int_of_var v)
    |R (tt, b, x, y, s)-> Recur (aux liees tt, aux liees b, aux (y :: x :: liees) s)
in
aux [] t ;;


(* substitue une liste de variables par des terme *)
let substit_db t listsub =
  let decale d t' =
    let rec aux_d p = function
      | Zero -> Zero
      | Su tt -> Su (aux_d p tt)
      | Index i when i < p -> Index (i)
      | Index i -> Index (i + d)
      | Recur (tt, b, s) ->
        Recur(aux_d p tt, aux_d p b, aux_d (p + 2) s)
    in
    aux_d 0 t'
  in
  let rec aux p = function
    | Zero -> Zero
    | Su tt -> Su (aux p tt)
    | Index i when i < p -> Index (i)
    | Index i ->
      begin try decale p (assoc (i-p) listsub)
      with Not_found -> Index i
      end
    | Recur (tt, b, s) ->
      Recur (aux p tt, aux p b, aux (p + 2) s)
  in
  aux 0 t ;;


(* transforme le terme en chaine *)
let rec string_of_recterme = function
  | RECZ -> "Z"
  | RECSucc t -> "S(" ^ (string_of_recterme t) ^ ")"
  | Var x -> x
  | R (t, b, x, y, s) ->
    "Rec(" ^ (string_of_recterme t) ^ "," ^ (string_of_recterme b)
    ^ ",(" ^ x ^ "," ^ y ^ ")" ^ (string_of_recterme s) ^ ")";;

(* affiche un terme *)
let print_recterme t = print_string (string_of_recterme t);;


(* retourne le terme numéral qui correspond à l'entier passé en paramètres *)
let rec recterme_of_int = function
  | 0 -> RECZ
  | x -> RECSucc (recterme_of_int (x - 1)) ;;


(* renvoit l'entier correcpondant au terme numéral passé en paramètre *)
let rec int_of_recterme = function
  | RECZ -> 0
  | RECSucc (x)->(int_of_recterme x) + 1
  | _ -> raise pas_numeral ;;


(* renvoit la liste des variables libres du terme donné en paramètres *)
let rec libres = function
  | RECZ -> []
  | RECSucc t -> libres t
  | Var x -> [x]
  | R (t, b, x, y, s) -> union (union (libres t) (libres b)) (subtract (libres s) [x; y]) ;;


(* substitue des variables par des termes *)
let substit t listsub =
  let list_db =
    let aux = function x,y -> (int_of_var x), (rtdb_of_rt y) in
    map aux listsub
  in
  rt_of_rtdb (substit_db (rtdb_of_rt t) list_db) ;;


(* calcule la sémantique d'un recterme dans un environnement *)
let rec semantique terme envir =
  match terme with
  | RECZ -> 0
  | RECSucc (t) -> 1 + semantique t envir
  | Var (x) -> assoc x envir
  | R (t, b, x, y, s) ->
    let n = semantique t envir in
    let rec vn = function
      | 0 -> semantique b envir
      | k -> let v = vn (k-1) in
        semantique s ((x, k - 1) :: (y, v) :: envir)
    in vn n ;;


(* effectue une réduction du terme passé en paramètre. La stratégie est
   la stratégie gauche : on réduit le terme le plus à gauche.
   On ne regarde pas si le terme est une variable car il ne doit pas y en
   avoir vu que le terme doit être clos. => le matching est non exhaustif *)
let rec fleche = function
  | RECZ -> raise en_forme_normale
  | RECSucc (t) -> RECSucc (fleche t)
  | R (t, b, x, y, s) ->
    match t with
    | RECZ -> b
    | RECSucc (tt)-> let s' = R(tt, b, x, y, s) in
      substit s [(x, tt); (y, s')]
    | _ -> R (fleche t, b, x, y, s) ;;


(* normalisation d'un terme c'est à dire transformation en numéral *)
let rec normalise t = try
  normalise (fleche t)
with en_forme_normale -> t ;;
