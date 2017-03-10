(**************************************************************************)
(*  Copyright 2004 Pietrzak Thomas                                        *)
(*                                                                        *)
(*    This file is part of Crec.                                          *)
(*                                                                        *)
(*    crec is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    crec is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with Crec; if not, write to the Free Software                 *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

#open "utils";;

(* Conversion d'un combinateur en chaine de caractères *)
let rec string_of_combinateur = function
  | Z -> "Z"
  | Succ -> "Succ"
  | Pi (i,j) -> "Pi(" ^ (string_of_int i) ^ "," ^ (string_of_int j) ^ ")"
  | S (c,n) ->
    let rec string_of_lst = function
      | [] -> ""
      | [x] -> (string_of_combinateur x)
      | x :: l ->(string_of_combinateur x) ^  "," ^ (string_of_lst l)
    in
    "S(" ^ (string_of_combinateur c) ^ ";" ^ (string_of_lst n) ^ ")"
  | Rec (b,s) -> "Rec(" ^ (string_of_combinateur b) ^ "," ^ (string_of_combinateur s) ^ ")";;


(* Affichage d'un combinateur *)
let print_combin c = print_string (string_of_combinateur c);;


(* convertit l'arité en chaine *)
let string_of_arite = function
  | Tout -> "*"
  | Arit (x) -> string_of_int x;;

(* convertit un entier en combinateur *)
let comb_of_int n =
  let rec aux = function
    | 0 -> Z
    | n -> S(Succ,[aux (n - 1)])
  in
  S (aux n,[]);;


(* Fonction qui calcule et vérifie l'arité d'un combinateur
   dans le cas du rec on regarde si le premier combinateur a une arité à "Tous" : dans ce cas
   il faut calculer l'arité en fonction du deuxième
   dans le cas su s on vérifie que l'arité du premier combinateur est égal au nombre de
   combinateurs restants ou que l'arité est "Tout"
   la fonction intermédiaire ariteliste renvoit l'arité d'une liste de combinateurs. Elle
   renvoit "Tout" dans le cas de la liste vide car l'arité peut être n'importe quoi. Son
   utilité est de vérifier que tous les combinateurs ont la même arité. Une exception est
   déclenchée si ce n'est pas le cas.
*)
let rec arite = function
  | Z -> Arit (0)
  | Succ -> Arit (1)
  | Pi (_, x) -> Arit (x)
  | S (c, l) ->
    let rec ariteliste = function
      | [] -> Tout
      | x :: [] -> arite x
      | x :: m ->
        begin match arite x with
        | Tout -> ariteliste m
        | Arit (x) ->
          begin match ariteliste m with
          | Tout -> Arit(x)
          | Arit (y) ->
            if x = y then Arit (x)
            else raise (err_S_ar_m (S (c, l)))
          end
        end
    in
    begin match arite c with
    | Tout -> ariteliste l
    | Arit (x) ->
      if x != list_length l then
        raise (err_S_ar_n (c, l))
      else
        ariteliste l
    end
  | Rec (b, s) ->
    begin match arite b,arite s with
    | Tout, Tout -> Tout
    | Tout, Arit (x) -> Arit (x - 1)
    | Arit (x), _ -> Arit (x + 1)
    end;;


(*
	calcule la sémantique d'un combinateur c'est à dire calcule
	Dans le cas du rec, le match sur l'argument ne prend pas en compte le cas de la liste vide
	car un combinateur rec a une arité supérieure ou égale à 1 => l'argument ne peut pas être
	une liste vide
*)
let rec sem combin arg =
  match combin with
  | Z -> 0
  | Succ -> hd arg + 1
  | Pi (i, n) -> nieme (i - 1) arg
  | S (c, l) -> sem c (maplist sem l arg)
  | Rec (b, s) ->
    begin match arg with
    | 0 :: l -> sem b l
    | x :: l ->
      let step = sem (Rec (b, s)) ((x - 1) :: l) in
      sem s ((x - 1) :: step :: l)
    end;;
