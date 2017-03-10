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

type recterme =
  | RECZ
  | RECSucc of recterme
  | Var of string
  | R of recterme * recterme * string * string * recterme
;;
type recterme_db =
  | Zero
  | Su of recterme_db
  | Index of int
  | Recur of recterme_db * recterme_db * recterme_db
;;

exception erreur_taille;;
exception pas_numeral;;

(* fonctions pour les termes de de bruijn *)
value libres_db : recterme_db -> int list
and rt_of_rtdb : recterme_db -> recterme
and rtdb_of_rt : recterme -> recterme_db
and substit_db : recterme_db -> (int * recterme_db) list -> recterme_db
(* fonctions pour les termes normaux *)
and string_of_recterme : recterme -> string
and print_recterme : recterme -> unit
and recterme_of_int : int -> recterme
and int_of_recterme : recterme -> int
and libres : recterme -> string list
and substit : recterme -> (string * recterme) list -> recterme
and semantique : recterme -> (string * int) list -> int
and normalise : recterme -> recterme;;
