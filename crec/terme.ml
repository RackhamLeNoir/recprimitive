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

#open "combin" ;;
#open "utils" ;;

(* Conversion d'un terme en chaine de caractères *)
let rec string_of_terme t =
  let aux = function
    | [] -> "[]"
    | x :: l -> "[" ^ (string_of_terme x) ^ (string_of_reste l) ^ "]"
  where rec string_of_reste = function
    | [] -> ""
    | x :: l-> "," ^ (string_of_terme x) ^ (string_of_reste l)
  in
  (string_of_combinateur t.comb) ^ (aux t.termes) ;;


(* Affichage d'un terme *)
let print_terme l = print_string (string_of_terme l) ;;


(* Conversion d'un entier en terme *)
let rec terme_of_int = function
 | 0 -> { comb = Z ; termes = [] }
 | x -> { comb = Succ ; termes = [terme_of_int (x - 1)] } ;;


(* Conversion d'un terme en entier *)
let rec int_of_terme t =
  match t.comb with
  | Z -> 0
  | Succ ->
    begin try 1 + int_of_terme(hd (t.termes)) with
    | _ -> raise err_conversion
    end
  | _ -> raise err_conversion ;;

(* fonction qui vérifie que le terme est valide, c'est à dire que
   l'arité du combinateur est égal à l'arité de la liste.
*)
let rec valide t =
  let rec valideliste = function
    | [] -> true
    | y :: l -> (valide y) & (valideliste l)
  in
  match arite t.comb with
  | Tout -> valideliste t.termes
  | Arit (x) -> (x = list_length t.termes) & valideliste t.termes ;;


let maplisttermes liste arg  =
  let rec aux = function
    | [] -> []
    | x :: l -> { comb = x ; termes = arg } :: (aux l)
in aux liste ;;

(* calcule la réduction d'un terme, c'est à dire la flèche simple *)
let rec fleche t =
  match t.comb with
  | Z -> raise forme_normale
  | Succ -> { comb = Succ ; termes = [fleche(hd t.termes)] }
  | Pi (i,n) -> nieme (i - 1) (t.termes)
  | S (c,l) -> { comb = c ; termes = (maplisttermes l t.termes)}
  | Rec (b,s) ->
    begin match t.termes with
    | x :: l ->
      begin match x.comb with
      | Z -> { comb = b ; termes = l }
      | Succ ->
        let u = hd x.termes in
        let s' = { comb = Rec(b,s) ; termes = (u::l) } in
        {
          comb = s ;
          termes = u :: s' :: l
        }
      | _ -> { comb = Rec(b,s) ; termes = (fleche x)::l }
      end
    end ;;

(* calcule un terme, c'est à dire le réduit en forme normale *)
let rec calcul t = try
  calcul (fleche t)
with forme_normale -> t ;;
