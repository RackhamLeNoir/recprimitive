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

{
#open "parser";;

exception Fin;;
exception EOF;;
exception ouvre of string;;
exception err_id of string;;
exception erreur_env of string list;;
}
rule lexeme = parse
  [` ` `\t`]						{ lexeme lexbuf }
  | `\n`						{ FINDELIGNE }
  | `Z`							{ ZZ }
  | `S`							{ SUCC }
  | "Rec"|"REC"						{ REC }
  | `(`							{ PARENG }
  | `)`							{ PAREND }
  | `[`							{ CROCHG }
  | `]`							{ CROCHD }
  | `{`							{ ACCOLG }
  | `}`							{ ACCOLD }
  | `,`							{ VIRG }
  | `\034` [`A`-`Z` `a`-`z` `/` `.` `0`-`9`]* `\034`
    {
      let chaine = get_lexeme lexbuf in
      FILE(sub_string chaine 1 ((string_length chaine) - 2))
    }
  | `=`							{ EGAL }
  | `<`							{ FLECHE }
  | "Terme"|"TERME"					{ TERME }
  | "Norm"|"NORM"					{ NORM }
  | "Ouvre"|"OUVRE"					{ OUVRE }
  | "Sauve"|"SAUVE"					{ SAUVE }
  | "Quitter"|"QUITTER"					{ raise Fin }
  | ([`A`-`Z`])([`A`-`Z` `0`-`9`])*			{ IDENT(get_lexeme lexbuf) }
  | [`0`-`9`]+						{ INT(int_of_string(get_lexeme lexbuf)) }
  | [`a`-`z`]+						{ VAR(get_lexeme lexbuf) }
  | "#"							{ Commentaire lexbuf ; lexeme lexbuf }
  | eof							{ raise EOF }
  | _							{ AUTRE }
and Commentaire = parse
  `\n`							{ () }
  | eof							{ () }
  | _							{ Commentaire lexbuf }
;;
