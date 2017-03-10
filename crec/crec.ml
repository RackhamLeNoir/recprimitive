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

#open "combin";;
#open "terme";;
#open "parsing";;
#open "utils";;

print_string "Bienvenue dans Crec !";;

let rec interprete channel =
  while true do
    print_newline();
    print_string "-> ";
    flush std_out;
    begin try
      let resultat = parser__ligne lexer__lexeme channel in
        print_string resultat
    with
    (* un identificateur inconnu a été spécifié *)
    | lexer__err_id n -> print_string ("Identificateur " ^ n ^ " inconnu")

    (* Erreur soulevée quand le parseur n'a pas reconnu le motif d'entrée *)
    | Parse_error -> print_string ("Erreur de syntaxe")

    (* on a essayé d'appliquer un combinateur a un argument incompatible*)
    | erreur_taille (c, l) ->
      print_string ("Erreur : combinateur " ^ (string_of_combinateur c) ^
      " d'arité " ^ (string_of_arite (arite c)) ^ " appliqué à l'argument " ^
      (string_of_intlist l) ^ " de longueur " ^ (string_of_int (list_length l)))

    (* ouverture d'un fichier *)
    | lexer__ouvre fichier ->
      begin try
        let ic = open_in_bin (fichier ^ ".crec") in
        begin try
          let lexbuf = lexing__create_lexer_channel ic in
          interprete lexbuf
        with
        | lexer__EOF -> close_in ic
        end
      with
      | sys__Sys_error s -> print_string s
      end

    (* erreur dans l'arité *)
    | lexer__err_pi (i1, i2) ->
      print_string ("Erreur dans Pi : "
      ^ (string_of_int i1) ^ " > " ^ (string_of_int i2))
    | lexer__err_pi_2  -> print_string ("Erreur dans Pi : paramètre nul")
    | combin__err_S_ar_n (c, l) ->
      print_string ("Erreur d'arité dans S : combinateur "
      ^ (string_of_combinateur c) ^ " d'arité " ^ (string_of_arite (arite c))
      ^ " avec liste de longueur " ^ (string_of_int (list_length l)))
    | combin__err_S_ar_m c ->
      print_string ("Erreur d'arité dans S : la liste du combinateur "
      ^ (string_of_combinateur c) ^ " contient des combinateurs d'arité différentes ")
    | lexer__err_rec (b, s) ->
      print_string ("Erreur d'arité dans Rec : " ^ (string_of_combinateur b) ^ " d'arité "
      ^ (string_of_arite (arite b)) ^ " et " ^ (string_of_combinateur s) ^ " d'arité "
      ^ (string_of_arite (arite s)))
    | lexer__err_rec_tout ->
      print_string ("Erreur d'arité dans Rec : au moins une des arités doit être fixée")
    | lexer__err_rec_s s ->
      print_string ("Erreur d'arité dans Rec : le s " ^ (string_of_combinateur s) ^ " est d'arité "
      ^ (string_of_arite (arite s)) ^ " or il doit avoir une arité >= 2")
    | terme_non_valide ->
      print_string ("le terme n'est pas valide")

    (* Exception ne devant pas êtres levées *)
    | Failure m -> print_string ("Fail : " ^ m)
    | Not_found -> print_string ("objet non trouvé")
    end
  done;;

try
  let tampon = lexing__create_lexer_channel std_in in
  interprete tampon
with lexer__Fin | lexer__EOF -> print_string "Ciao !" ; print_newline();;
