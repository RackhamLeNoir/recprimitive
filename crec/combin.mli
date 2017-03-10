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

(* définition d'un combinateur de la récursion primitive *)
type combinateur = 
  | Z
  | Succ
  | Pi of int * int
  | S of combinateur * combinateur list
  | Rec of combinateur * combinateur;;

type t_arite =
  | Tout
  | Arit of int;;

(* Exceptions pour gérer les erreurs de calcul ou d'arité *)
exception erreur_taille of combinateur * int list;;
exception err_S_ar_n of combinateur * combinateur list;;
exception err_S_ar_m of combinateur;;

value string_of_combinateur : combinateur -> string
and print_combin : combinateur -> unit
and comb_of_int : int -> combinateur
and string_of_arite : t_arite -> string
and arite : combinateur -> t_arite
and sem : combinateur -> int list -> int
;;
