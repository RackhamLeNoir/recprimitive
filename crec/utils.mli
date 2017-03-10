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

value string_of_intlist : int list -> string
and print_intlist : int list -> unit
and string_of_stringlist : string list -> string
and print_stringlist : string list -> unit
and nieme : int -> 'a list -> 'a
and maplist : ('a -> 'b -> 'c) -> 'a list -> 'b -> 'c list
and ajoute_objet : 'a -> 'b -> ('a * 'b) list ref -> unit
and var_of_int : int -> string
and int_of_var : string -> int;;
