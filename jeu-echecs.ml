
type player = 
  |Player1
  |Player2

type pionProperties = 
  {
    x : int; (* Couple de position x,y *)
    y : int; 
    played : int; (* Indique le nombre de fois que ce pion a été joué depuis le début *)
    player: player
  }

type pion = 
  |Pion of pionProperties
  |Cavalier of pionProperties
  |Reine of pionProperties
  |Roi of pionProperties
  |Tour of pionProperties
  |Fou of pionProperties
  |Vide

exception CoupImpossible of (int*int) (* Exception qui dit si un coup est impossible ( pion pas capable de faire un tel coup, pion qui saute 5 cases, pion en travers du chemin de la dame jusqu'à une pos)  *)

type plateau = {
  joueur1 : pion list; (* les pions mangés par le joueur 1 *)
  joueur2 : pion list; (* les pions mangés par le joueur 2 *)
  joueur1Pieces : pion array;
  joueur2Pieces : pion array;
  plateau : pion array array; (* Le plateau de jeu *)
}

(* Fonction de checkage pour le fou*)
let rec diagonal_path_possible start_pos end_pos plateau = 
  let (x1, y1) = start_pos in
  let (x2, y2) = start_pos in
  let dx = if x2>x1 then 1 else -1 in
  let dy = if y2>y1 then 1 else -1 in
  if x1=x2 && y1=y2 then true
  else
    let proch_X = x1 + dx in
    let proch_Y = y1 + dy in
    match plateau.(proch_X).(proch_Y), (proch_X, proch_Y) with
      |Vide _, _ -> diagonal_path_possible (proch_X, proch_Y) end_pos plateau
      |_, (x, y) when x = x2 && y = y2 -> true
      |_ -> false


(* fonction de checkage pour la dame : Horizontal, Vertical ou Diagonal*)
let rec diagonal_horizontal_vertical_path_possible start_pos end_pos plateau = 
  let (x1, y1) = start_pos in
  let (x2, y2) = start_pos in
  let dx = if x2=x1 then 0 else if x2>x1 then 1 else -1 in
  let dy = if y2=y1 then 0 else if y2>y1 then 1 else -1 in
  if x1=x2 && y1=y2 then true
  else 
    let proch_X = x1 + dx in
    let proch_Y = y1 + dy in
    match plateau.(proch_X).(proch_Y), (proch_X, proch_Y) with
      |Vide _, _ -> diagonal_horizontal_vertical_path_possible (proch_X, proch_Y) end_pos plateau
      |_, (x,y) when x = x2 && y = y2 -> true
      |_ -> false

(* Fonction pour le roi *)
let all_moves1_path_possible start_pos end_pos = 
  let (x1, y1) = start_pos in
  let (x2, y2) = end_pos in
  if ((x2-x1 <= 1 && x2-x1 >= 0)  || (x1-x2 <= 1 && x1-x2 >= 0)) then true else false

(* Fonction pour le cavalier (vérification) *)
let cavalier_path_possible start_pos end_pos =
  let (x1, y1) = start_pos in
  let (x2, y2) = end_pos in
  let dx = abs (x2 - x1) in
  let dy = abs (y2 - y1) in
  (dx = 2 && dy = 1) || (dx = 1 && dy = 2)

(*Fonction pour la Tour*)
let rec horizontal_vertical_path_possible start_pos end_pos plateau =
  let (x1, y1) = start_pos in
  let (x2, y2) = end_pos in
  let dx = if x2 = x1 then 0 else if x2 > x1 then 1 else -1 in
  let dy = if y2 = y1 then 0 else if y2 > y1 then 1 else -1 in
  if x1 = x2 && y1 = y2 then true
  else
    let proch_X = x1 + dx in
    let proch_Y = y1 + dy in
    match plateau.(proch_X).(proch_Y), (proch_X, proch_Y) with
    | Vide, _ -> horizontal_vertical_path_possible (proch_X, proch_Y) end_pos plateau
    | _, (x, y) when x = x2 && y = y2 -> true
    | _ -> false

(*Fonction pour le pion renvoie vrai que si le pion est orienté dans le bon sens de bas vers haut*)
let pion_path_possible start_pos end_pos myPion =
  let (x1,y1) = start_pos in
  let (x2, y2) = end_pos in
  let dx = abs(x1-x2) in
  let dy = y2-y1 in
  if myPion.played > 0 then (dy = 1) && (dx = 0 || dx = 1) else (dy = 1 || dy = 2) && (dx = 0 || dx = 1)


  let coup_valide plateau start_pos end_pos =
    try
      let (x1, y1) = start_pos in
      let (x2, y2) = end_pos in
      match plateau.(x1).(y1) with
      | Vide -> raise (CoupImpossible (x1, y1))  (* Pas de pièce à cet endroit *)
      | Pion props ->
          if pion_path_possible start_pos end_pos props then
            if plateau.(x2).(y2) = Vide || 
              (* Capturer la pièce adverse *)
              (match plateau.(x2).(y2) with
               | Pion {player = p} when p <> props.player -> true
               | _ -> false)
            then true
            else raise (CoupImpossible (x2, y2))
          else raise (CoupImpossible (x1, y1))
      | Cavalier props ->
          if cavalier_path_possible start_pos end_pos then true
          else raise (CoupImpossible (x1, y1))
      | Reine props ->
          if diagonal_horizontal_vertical_path_possible start_pos end_pos plateau then true
          else raise (CoupImpossible (x1, y1))
      | Roi props ->
          if all_moves1_path_possible start_pos end_pos then true
          else raise (CoupImpossible (x1, y1))
      | Tour props ->
          if horizontal_vertical_path_possible start_pos end_pos plateau then true
          else raise (CoupImpossible (x1, y1))
      | Fou props ->
          if diagonal_path_possible start_pos end_pos plateau then true
          else raise (CoupImpossible (x1, y1))
    with
    | Invalid_argument _ -> raise (CoupImpossible start_pos)  (* Position hors des limites *)
  



let deplacer_piece (plateau: plateau) start_pos end_pos player =
  if coup_valide plateau.plateau start_pos end_pos then
    let (x1, y1) = start_pos in
    let (x2, y2) = end_pos in
    let piece = plateau.plateau.(x1).(y1) in
    let plateau = match plateau.plateau.(x2).(y2) with
    | Vide -> plateau  (* Aucun changement si la case est vide *)
    | p -> (* Ajouter la pièce capturée à la liste du joueur *)
        if player = Player1 then 
          { plateau with joueur1 = p :: plateau.joueur1 }  (* Joueur 1 capture une pièce *)
        else 
          { plateau with joueur2 = p :: plateau.joueur2 }  (* Joueur 2 capture une pièce *)
  in
    plateau.plateau.(x2).(y2) <- piece;  (* Déplace la pièce vers la nouvelle position *)
    plateau.plateau.(x1).(y1) <- Vide;  (* Vide l'ancienne position *)
    (* Mettez à jour les propriétés du pion, par exemple, si c'est un pion *)
    match piece with
    | Pion props -> plateau.plateau.(x2).(y2) <- Pion {props with x = x2; y = y2; played = props.played + 1}
    | _ -> () (* Pour les autres pièces, la mise à jour est plus simple *)
  else raise (CoupImpossible end_pos)


(* On initialise les propriétés des différents pions du plateau *)
let pionProp = fun x y player -> {x=x;y=y; played=0; player=player}
let otherpionProp = fun x y player -> {x=y;y=y;played =0;player=player}
(* Je compte changer les pionsProps(actions) par quelque chose qui ne dépend pas d'un nombre 'simple' pour mieux tester ensuite si un pion est sur le chemin
de la Dame par exemple. le champ actions renverrait donc une 'fonction' mathématique linéaire -> La dame peut se déplacer en diagonale donc on choisit la diagonale 
gauche la dame évolue en x en même temps qu'en y avec x et y les 'axes' du tableau plateau.plateau*)

let init_test =

  {joueur1=[];joueur2=[];joueur1Pieces=[|Vide|];joueur2Pieces=[|Vide|];plateau=Array.make_matrix 8 8 (Pion {x=0;y=0;played=0;player=Player1})}


(*
Fonction 'efface' la console
*)
let clear () =
  for i=0 to 200 do
    print_newline ();
  done;
;;
(*
Fonction qui affiche la grille des echecs dans le terminal
*)


let showGrid grid =
  let rec show indx = 
    match indx with
    | i when i >= Array.length grid.plateau -> ()  (* Arrêter quand on dépasse le dernier index *)
    | _ ->
      for i = 0 to Array.length grid.plateau.(indx) - 1 do
        match grid.plateau.(indx).(i) with
        | Pion _ -> print_string "♙  "
        | Cavalier _ -> print_string "♘  "
        | Reine _ -> print_string "♕  "
        | Roi _ -> print_string "♔  "
        | Tour _ -> print_string "♖  "
        | Fou _ -> print_string "♗  "
        | Vide -> print_string "·  "
      done;
      print_string "\n";
      print_string "\n";
      show (indx + 1)
  in 
  show 0
;;



let init () =
  let plateau = Array.make_matrix 8 8 Vide in

  (* Placement des pions pour chaque joueur *)
  for i = 0 to 7 do
    plateau.(1).(i) <- Pion (pionProp i 1 Player2);  (* Pions du joueur 2 *)
    plateau.(6).(i) <- Pion (pionProp i 6 Player1);  (* Pions du joueur 1 *)
  done;

  (* Placement des tours *)
  plateau.(0).(0) <- Tour (otherpionProp 0 0 Player2);
  plateau.(0).(7) <- Tour (otherpionProp 7 0 Player2);
  plateau.(7).(0) <- Tour (otherpionProp 0 7 Player1);
  plateau.(7).(7) <- Tour (otherpionProp 7 7 Player1);

  (* Placement des cavaliers *)
  plateau.(0).(1) <- Cavalier (otherpionProp 1 0 Player2);
  plateau.(0).(6) <- Cavalier (otherpionProp 6 0 Player2);
  plateau.(7).(1) <- Cavalier (otherpionProp 1 7 Player1);
  plateau.(7).(6) <- Cavalier (otherpionProp 6 7 Player1);

  (* Placement des fous *)
  plateau.(0).(2) <- Fou (otherpionProp 2 0 Player2);
  plateau.(0).(5) <- Fou (otherpionProp 5 0 Player2);
  plateau.(7).(2) <- Fou (otherpionProp 2 7 Player1);
  plateau.(7).(5) <- Fou (otherpionProp 5 7 Player1);

  (* Placement des reines *)
  plateau.(0).(3) <- Reine (otherpionProp 3 0 Player2);
  plateau.(7).(3) <- Reine (otherpionProp 3 7 Player1);

  (* Placement des rois *)
  plateau.(0).(4) <- Roi (otherpionProp 4 0 Player2);
  plateau.(7).(4) <- Roi (otherpionProp 4 7 Player1);

  (* Initialisation des pièces pour chaque joueur *)
  let joueur1Pieces = Array.init 16 (fun i ->
    match i with
    | 0 | 7 -> Tour (otherpionProp (i / 7 * 7) 7 Player1)  (* Tours *)
    | 1 | 6 -> Cavalier (otherpionProp i 7 Player1)     (* Cavaliers *)
    | 2 | 5 -> Fou (otherpionProp i 7 Player1)              (* Fous *)
    | 3 -> Reine (otherpionProp 3 7 Player1)                (* Reine *)
    | 4 -> Roi (otherpionProp 4 7 Player1)                   (* Roi *)
    | _ -> Pion (pionProp (i - 8) 6 Player1)           (* Pions *)
  ) in

  let joueur2Pieces = Array.init 16 (fun i ->
    match i with
    | 0 | 7 -> Tour (otherpionProp (i / 7 * 7) 0 Player2)  (* Tours *)
    | 1 | 6 -> Cavalier (otherpionProp i 0 Player2)     (* Cavaliers *)
    | 2 | 5 -> Fou (otherpionProp i 0 Player2)              (* Fous *)
    | 3 -> Reine (otherpionProp 3 0 Player2)                (* Reine *)
    | 4 -> Roi (otherpionProp 4 0 Player2)                   (* Roi *)
    | _ -> Pion (pionProp (i - 8) 1 Player2)           (* Pions *)
  ) in

  { joueur1 = []; joueur2 = []; joueur1Pieces; joueur2Pieces; plateau }

let lire_position () =
  print_endline "Entrez la position de départ (x puis y) :";
  let x1 = read_int () in
  let y1 = read_int () in
  print_endline "Entrez la position d'arrivée (x puis y) :";
  let x2 = read_int () in
  let y2 = read_int () in
  ((x1, y1), (x2, y2))

(*
  Fonction qui initialise le tableau de jeu avec les éléments aux bons endroits
*)

(* Fonction permettant de vérifier si un mouvement est juste à partir de la stratégie de mouvement exemple (Mouvement de Dame, Roi)
La fonction throw une erreur si le coup n'est pas possible avec l'emplacement de la case problématique
*)

(*
Fonction pour créer une stratégie de mouvement par Exemple:
    - Roi : ajouter 1 ou retirer à x ou y
    - Dame : Bouger sur l'axe x ou y de la case ou ajouter (plus ou moins 1, plus ou moins 1)*k (k fois)
    - Cavalier : 
    - Pion : Si c'est la première fois que le pion est joué on peut sauter 2 cases sinon on va tout droit, si il y a un pion sur les cases à y+1, x plus ou moins 1
    - Tour : Bouger sur l'axe x ou y de la case 
    - Fou : (+- k, +- k) Vecteur
*)

(*
Fonction qui agit comme la boucle de jeu et qui donne l'action à chacun des joueurs tour à tour
*)

(*
Fonction qui récupère l'entrée positions xy du pion à déplacer
*)

