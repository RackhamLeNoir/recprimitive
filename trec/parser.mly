/**************************************************************************/
/*  Copyright 2004 Pietrzak Thomas                                        */
/*                                                                        */
/*    This file is part of Trec.                                          */
/*                                                                        */
/*    Trec is free software; you can redistribute it and/or modify    */
/*    it under the terms of the GNU General Public License as published   */
/*    by the Free Software Foundation; either version 2 of the License,   */
/*    or (at your option) any later version.                              */
/*                                                                        */
/*    Trec is distributed in the hope that it will be useful,         */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*    GNU General Public License for more details.                        */
/*                                                                        */
/*    You should have received a copy of the GNU General Public License   */
/*    along with Trec; if not, write to the Free Software                 */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               */
/*    MA  02111-1307  USA                                                 */
/*                                                                        */
/**************************************************************************/

%{
#open "recterme";;
#open "utils";;

let liste_rectermes = ref [];;

(* écrit une liste dans un fichier *)
let enregistre_rectermeliste fichier liste =
 let rec aux = function
   | [] -> ()
   | (x, y) :: l ->
     output_string fichier ("terme " ^ x ^ " = " ^ (string_of_recterme y) ^ "\n") ;
     aux l
 in aux liste;;
%}

%token <int> INT
%token <string> FILE
%token <string> IDENT
%token <string> VAR
%token ZZ SUCC REC
%token COMB TERME NORM
%token PARENG PAREND
%token CROCHG CROCHD
%token ACCOLG ACCOLD
%token VIRG EGAL FLECHE
%token FINDELIGNE OUVRE SAUVE AUTRE

%start ligne             /* the entry point */
%type <string> ligne
%type <recterme__recterme> rect
%type <recterme__recterme> rectid
%type <recterme__recterme> rectsubst
%type <(string * int) list> env
%type <(string * int) list> suiteenv
%type <(string * recterme__recterme) list> substlst
%type <(string * recterme__recterme) list> suitesubstlst
%%

ligne:
  expression FINDELIGNE					{ $1 }
  | FINDELIGNE						{ "" }
;
expression :
  /* ouverture d'un fichier*/
  OUVRE FILE						{ raise (lexer__ouvre $2) }

  /* sauvegarde dans un fichier*/
  | SAUVE FILE
    {
      try
        let fichier = $2 ^ ".trec" in
        let outfile = open_out_bin fichier in
        enregistre_rectermeliste outfile !liste_rectermes ;
        close_out outfile ;
        "enregistrement effectué dans fichier " ^ fichier
      with
        sys__Sys_error s -> s
    }

  /* affichage du terme associé à l'identificateur entré */
  | IDENT
  {
    try
      $1 ^ " : " ^ (string_of_recterme (assoc $1 !liste_rectermes))
      ^ " : " ^ (string_of_stringlist (libres (assoc $1 !liste_rectermes)))
    with Not_found -> raise (lexer__err_id $1)
  }

  /* affichage d'un terme */
  | rectsubst
  {
    (string_of_recterme $1)
    ^ " : " ^ (string_of_stringlist (libres $1))
  }

  /* Calcul d'un terme dans un environnement : on vérifie que
    toutes les variables sont dans l'environnement */
  | rectsubst env
  {
    let reste = subtract (libres $1) (map fst $2) in
    if reste != [] then raise (lexer__erreur_env reste)
    else string_of_int (semantique $1 $2)
  }

  /* Définition d'un nouveau terme */
  | TERME IDENT EGAL rectsubst
  {
    ajoute_objet $2 $4 liste_rectermes;
    $2 ^ " : " ^ (string_of_recterme (assoc $2 !liste_rectermes))
      ^ " : " ^ (string_of_stringlist (libres (assoc $2 !liste_rectermes)))
  }

  /* normalisation d'un  terme clos */
  | NORM rectsubst
  {
    (* on vérifie si le terme est clos *)
    let lib = libres $2 in
    if list_length (lib) != 0 then
      "le terme " ^ (string_of_recterme $2)
      ^ " n'est pas clos. Variables libres : "
      ^ (string_of_stringlist lib)
    else
      let n = normalise $2 in
        (string_of_recterme n) ^ ":" ^ (string_of_int (int_of_recterme n))
  }

  /* cas d'erreur */
  | AUTRE							{ "erreur de saisie" }
;

/* recterme */
rect:
  ZZ								{ RECZ }
  | SUCC PARENG rectsubst PAREND				{ RECSucc $3 }
  | VAR								{ Var $1 }
  | REC PARENG rectsubst VIRG rectsubst VIRG PARENG VAR VIRG VAR PAREND rectsubst PAREND
    { R($3,$5,$8,$10,$12) }
;

/* recterme ou identificateur : renvoit une erreur
  si l'identificateur n'est associé à aucun recterme */
  rectid:
  rect								{ $1 }
  | IDENT
  {
    try assoc $1 !liste_rectermes
    with Not_found -> raise (lexer__err_id $1)
  }
  | INT								{ recterme_of_int $1 }
;

/* recterme ou identificateur : renvoit une erreur
  si l'identificateur n'est associé à aucun recterme */
rectsubst:
  rectid							{ $1 }
  | rectid substlst						{ substit $1 $2 }
;

/* un environnement c'est à dire une liste d'association
  entre une chaine (variable) et un entier (valeur) */
env:
  ACCOLG suiteenv						{ $2 }
;
suiteenv:
  VAR FLECHE INT VIRG suiteenv					{ ($1,$3)::$5 }
  | VAR FLECHE INT ACCOLD					{ [($1,$3)] }
  | ACCOLD							{ [] }
;

/* substitutions : listes d'association entre une
  chaine (variable) et un recterme qui vient la remplacer */
substlst:
  CROCHG suitesubstlst						{ $2 }
;
suitesubstlst:
  VAR FLECHE rectsubst VIRG suitesubstlst			{ ($1,$3)::$5 }
  | VAR FLECHE rectsubst CROCHD					{ [($1,$3)] }
  | CROCHD							{ [] }
;
