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
type terme =
  {
    comb : combin__combinateur ;
    termes : terme list
  };;

(* exceptions diverses *)
exception err_conversion;;
exception terme_non_valide;;
exception forme_normale;;

value string_of_terme : terme -> string
and print_terme : terme -> unit
and terme_of_int : int -> terme
and int_of_terme : terme -> int
and valide : terme -> bool
and fleche : terme -> terme
and calcul : terme -> terme;;
