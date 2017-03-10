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

#open "recterme";;
#open "parsing";;
#open "utils";;

print_string "Bienvenue dans Trec !";;

let rec interprete channel =
  while true do
    print_newline();
    print_string "-> ";
    flush std_out;
    begin try
      let resultat = parser__ligne lexer__lexeme channel in
      print_string resultat ;
    with
    (* un identificateur inconnu a été spécifié *)
    | lexer__err_id n -> print_string ("Identificateur " ^ n ^ " inconnu")

    (* Erreur soulevée quand le parseur n'a pas reconnu le motif d'entrée *)
    | Parse_error -> print_string ("Erreur de syntaxe")

    (* erreurs de saisie *)
    | erreur_taille -> print_string ("Erreur de taille sur l'entrée");
    | lexer__erreur_env e ->
      print_string ("Ces variables ne sont pas dans l'environnement : ");
      print_stringlist e;

    (* ouverture d'un fichier *)
    | lexer__ouvre fichier ->
      begin try
        let ic = open_in_bin (fichier ^ ".trec") in
        begin try
          let lexbuf = lexing__create_lexer_channel ic in
          interprete lexbuf
        with
        | lexer__EOF -> close_in ic
        end
      with
      | sys__Sys_error s -> print_string s
      end

    (* Exception ne devant pas êtres levées *)
    | Failure m -> print_string ("Fail : " ^ m)
    | Not_found -> print_string ("Erreur de dépassement de liste")
    end
  done;;


try
  let tampon = lexing__create_lexer_channel std_in in
  interprete tampon
with lexer__Fin | lexer__EOF -> print_string "Ciao !" ; print_newline();;

