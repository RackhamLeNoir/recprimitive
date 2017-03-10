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

(* Conversion d'une liste d'entiers en chaine de caractères *)
let string_of_intlist = function
  | [] -> "[]"
  | x :: l -> "[" ^ (string_of_int x) ^ (string_of_reste l) ^ "]"
  where rec string_of_reste = function
    | [] -> ""
    | x :: l -> "," ^ (string_of_int x) ^ (string_of_reste l);;


(* Affichage d'une liste d'entiers *)
let print_intlist l = print_string (string_of_intlist l);;


(* Conversion d'une liste de chaines de caractères en chaine de caractères *)
let string_of_stringlist = function
  | [] -> "()"
  | x :: l -> "(" ^ x ^ (string_of_reste l) ^ ")"
  where rec string_of_reste = function
    | [] -> ""
    | x :: l -> "," ^ x ^ (string_of_reste l);;


(* Affichage d'une liste d'entiers *)
let print_stringlist l = print_string (string_of_stringlist l);;



(* fonction qui renvoie le nieme élément d'une liste *)
let rec nieme n l =
  match n,l with
  | _, [] -> raise Not_found
  | 0, x :: _ -> x
  | _, _ :: l -> nieme (n - 1) l;;


(* cette fonction applique la fonction f avec l'argument g à chaque élément de la liste *)
let maplist f liste arg  =
  let rec aux = function
    | [] -> []
    | x :: l -> (f x arg) :: (aux l)
  in aux liste;;


(* fonction qui ajoute un objet et le nom qui lui est associé dans la liste des identificateurs *)
let rec ajoute_objet nom o liste =
  let rec aux = function
    | [] -> [(nom, o)]
    | (a,b) :: l when nom = a -> (a, o) :: l
    | (a,b) :: l -> (a, b) :: (aux l)
  in
  liste := aux !liste;;

(* fonction qui donne un nom de variable en fonction d'un entier *)
let rec var_of_int = function
  | x when x < 26 -> (make_string 1 (char_of_int (x mod 26 + int_of_char `a`)))
  | x -> var_of_int (x / 26 - 1) ^ (make_string 1 (char_of_int (x mod 26 + int_of_char `a`)));;


(* fonction qui donne un entier en fonction d'un nom de variable *)
let rec int_of_var s =
  match string_length s with
  | 1 -> int_of_char (nth_char s 0) - (int_of_char `a`)
  | x -> int_of_char (nth_char s (x - 1)) - (int_of_char `a`) + 26 * (1 + int_of_var (sub_string s 0 (x - 1)));;
