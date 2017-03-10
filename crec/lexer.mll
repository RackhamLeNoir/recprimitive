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

{
#open "parser";;
#open "combin";;

exception Fin;;
exception EOF;;
exception ouvre of string;;
exception err_id of string;;
exception err_pi of int * int;;
exception err_pi_2;;
exception err_rec of combinateur * combinateur;;
exception err_rec_tout;;
exception err_rec_s of combinateur;;
}
rule lexeme = parse
  [` ` `\t`]						{ lexeme lexbuf } (* supprime les blancs*)
  | `\n`						{ FINDELIGNE }
  | `Z`|`z`						{ ZZ }
  | "Succ"|"succ"|"SUCC"				{ SUCC }
  | "Pi"|"pi"|"PI"					{ PI }
  | `S`|`s`						{ SS }
  | "Rec"|"rec"|"REC"					{ REC }
  | `(`							{ PARENG }
  | `)`							{ PAREND }
  | `[`							{ CROCHG }
  | `]`							{ CROCHD }
  | `,`							{ VIRG }
  | `;`							{ PTVIRG }
  | `\034` [`A`-`Z` `a`-`z` `/` `.` `_` `-` `0`-`9`]* `\034`
    {
      let chaine = get_lexeme lexbuf in
      FILE(sub_string chaine 1 ((string_length chaine) - 2))
    }
  | `=`							{ EGAL }
  | "Comb"|"COMB"|"comb"				{ COMB }
  | "Terme"|"TERME"|"terme"				{ TERME }
  | "Norm"|"NORM"|"norm"				{ NORM }
  | "Ouvre"|"OUVRE"|"ouvre"				{ OUVRE }
  | "Sauve"|"SAUVE"|"sauve"				{ SAUVE }
  | "Quitter"|"QUITTER"|"quitter"			{ raise Fin }
  | ([`a`-`z` `A`-`Z`])([`a`-`z` `A`-`Z` `0`-`9`])*	{ IDENT(get_lexeme lexbuf) }
  | [`0`-`9`]+						{ INT(int_of_string(get_lexeme lexbuf)) }
  | "#"							{ Commentaire lexbuf; lexeme lexbuf }
  | eof							{ raise EOF }
  | _							{ AUTRE }
and Commentaire = parse
  `\n`						{ () }
  | eof							{ () }
  | _							{ Commentaire lexbuf }
;;
