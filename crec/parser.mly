/**************************************************************************/
/*  Copyright 2004 Pietrzak Thomas                                        */
/*                                                                        */
/*    This file is part of Crec.                                          */
/*                                                                        */
/*    crec is free software; you can redistribute it and/or modify    */
/*    it under the terms of the GNU General Public License as published   */
/*    by the Free Software Foundation; either version 2 of the License,   */
/*    or (at your option) any later version.                              */
/*                                                                        */
/*    crec is distributed in the hope that it will be useful,         */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*    GNU General Public License for more details.                        */
/*                                                                        */
/*    You should have received a copy of the GNU General Public License   */
/*    along with Crec; if not, write to the Free Software                 */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               */
/*    MA  02111-1307  USA                                                 */
/*                                                                        */
/**************************************************************************/

%{
#open "combin";;
#open "terme";;
#open "utils";;

let liste_combs = ref [];;
let liste_termes = ref [];;

(* écrit une liste dans un fichier *)
let enregistre_termeliste fichier liste =
 let rec aux = function
   | [] -> ()
   | (x, y) :: l ->
     output_string fichier ("terme " ^ x ^ " = " ^ (string_of_terme y) ^ "\n") ;
     aux l
 in aux liste;;

(* écrit une liste dans un fichier *)
let enregistre_combinliste fichier liste =
 let rec aux = function
   | [] -> ()
   | (x, y) :: l ->
     output_string fichier ("comb " ^ x ^ " = " ^ (string_of_combinateur y) ^ "\n") ;
     aux l
 in aux liste;;

%}

%token <int> INT
%token <string> FILE
%token <string> IDENT
%token ZZ SUCC PI SS REC
%token COMB TERME NORM
%token PARENG PAREND
%token CROCHG CROCHD
%token PTVIRG VIRG EGAL
%token FINDELIGNE OUVRE SAUVE AUTRE

%start ligne             /* the entry point */
%type <string> ligne
%type <string> expression
%type <combin__combinateur> combinateur
%type <combin__combinateur list> suiteS
%type <combin__combinateur> cbid
%type <terme__terme> terme
%type <terme__terme> termeid
%type <int list> lst
%type <int list> suitelst
%type <terme__terme list> termelst
%type <terme__terme list> suitetermelst

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
      let fichier = $2 ^ ".crec" in
      let outfile = open_out_bin fichier in
      enregistre_combinliste outfile !liste_combs ;
      enregistre_termeliste outfile !liste_termes ;
      close_out outfile ;
      "enregistrement effectué dans fichier " ^ fichier
    with
      sys__Sys_error s -> s
  }

  /* affichage du combinateur ou du terme associé à l'identificateur entré */
  | IDENT
  {
    try
    $1 ^ " : " ^ (string_of_combinateur (assoc $1 !liste_combs))
    ^ " : " ^ (string_of_arite (arite (assoc $1 !liste_combs)))
    with Not_found ->
      try
      $1 ^ " : " ^ (string_of_terme (assoc $1 !liste_termes))
      with Not_found -> raise (lexer__err_id $1)
  }

  /* affichage d'un combinateur */
  | combinateur
  {
    (string_of_combinateur $1) ^ " : " ^ (string_of_arite (arite $1))
  }

  /* affichage d'un terme */
  | terme						{ (string_of_terme $1) }

  /* affichage d'une liste d'entiers : utilisé juste pour faire des tests */
  | lst							{ string_of_intlist $1 }

  /* Calcul d'un combinateur appliqué à une liste d'entiers : on vérifie la taille de l'argument */
  | cbid lst
  {
    match arite $1 with
    | Tout -> string_of_int (sem $1 $2)
    | Arit (x) ->
      if x != list_length $2 then raise (erreur_taille ($1, $2))
      else string_of_int (sem $1 $2)
  }

  /* Normalisation d'un terme */
  | NORM termeid
  {
    let final = calcul $2 in
      (string_of_terme final) ^ " : " ^ (string_of_int (int_of_terme final))
  }
  /* Définition d'un nouveau combinateur */
  | COMB IDENT EGAL cbid
  {
    ajoute_objet $2 $4 liste_combs;
    $2 ^ " : " ^ (string_of_combinateur (assoc $2 !liste_combs))
    ^ " : " ^ (string_of_arite (arite (assoc $2 !liste_combs)))
  }

  /* Définition d'un nouveau terme */
  | TERME IDENT EGAL termeid
  {
    (ajoute_objet $2 $4 liste_termes;
    $2 ^ " : " ^ (string_of_terme (assoc $2 !liste_termes)))
  }

  /* cas d'erreur */
  | AUTRE						{ "erreur de saisie" }
;

combinateur:
  ZZ							{ Z }
  | SUCC						{ Succ }
  | PI PARENG INT VIRG INT PAREND
  {
    (* On vérifie si les paramètres du Pi sont correctes *)
    if $3 > $5 then 
      raise (lexer__err_pi ($3, $5)) 
    else if $3 = 0 then
      raise lexer__err_pi_2
    else
      Pi ($3, $5)
  }
  | SS PARENG cbid PTVIRG suiteS			{ S ($3, $5) }
  | REC PARENG cbid VIRG cbid PAREND
  {
    match arite $3, arite $5 with
    | Tout, Tout -> raise lexer__err_rec_tout
    | Arit (_), Tout -> Rec ($3, $5)
    | Tout, Arit (y) ->
      if y < 2 then raise (lexer__err_rec_s ($5))
      else Rec ($3, $5)
    | Arit(x), Arit(y) ->
      if y != x + 2 then raise (lexer__err_rec ($3,$5))
        else Rec ($3, $5)
  }
;

/* liste de combinateurs */
suiteS:
  cbid VIRG suiteS					{ $1 :: $3 }
  | cbid PAREND						{ [$1] }
  | PAREND						{ [] }
;

terme:
  cbid termelst
    {
      let res = { comb = $1 ; termes = $2 } in
        if valide res then
          res
        else
          raise terme_non_valide
    }
;

/* combinateur ou identificateur : renvoit une erreur si l'identificateur n'est associé à aucun combinateur */
cbid:
  combinateur						{ $1 }
  | IDENT
  {
    try assoc $1 !liste_combs
    with Not_found -> raise (lexer__err_id $1)
  }
  | INT							{ comb_of_int $1 }
;

/* terme ou identificateur : renvoit une erreur si l'identificateur n'est associé à aucun terme */
termeid:
  terme							{ $1 }
  | IDENT
  {
    try assoc $1 !liste_termes
    with Not_found -> raise (lexer__err_id $1)
  }
  | INT							{ terme_of_int $1 }
;

/* liste d'entiers */
lst:
  PARENG suitelst					{ $2 }
;
suitelst:
  INT VIRG suitelst					{ $1 :: $3 }
  | INT PAREND						{ [$1] }
  | PAREND						{ [] }
;

/* liste de termes */
termelst:
  CROCHG suitetermelst					{ $2 }
;
suitetermelst:
  termeid VIRG suitetermelst				{ $1 :: $3 }
  | termeid CROCHD					{ [$1] }
  | CROCHD						{ [] }
;
