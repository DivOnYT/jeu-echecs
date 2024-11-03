
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
  let (x1, y1) = start_pos in
  let (x2, y2) = end_pos in
  let dx = abs (x2 - x1) in
  let dy = y2 - y1 in
  if myPion.player = Player1 then
    if myPion.played = 0 && dx = 0 && (dy = -1 || dy = -2) then true
    else if dx = 0 && dy = -1 then true
    else if dx = 1 && dy = -1 then true  (* Capture diagonale *)
    else false
  else
    if myPion.played = 0 && dx = 0 && (dy = 1 || dy = 2) then true
    else if dx = 0 && dy = 1 then true
    else if dx = 1 && dy = 1 then true  (* Capture diagonale *)
    else false


let coup_valide plateau start_pos end_pos =
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


let deplacer_piece (plateau: plateau) start_pos end_pos player =
  try
    if coup_valide plateau.plateau start_pos end_pos then
      let (x1, y1) = start_pos in
      let (x2, y2) = end_pos in
      let piece = plateau.plateau.(x1).(y1) in
      
      (* Gérer la capture si nécessaire *)
      let plateau =
        match plateau.plateau.(x2).(y2) with
        | Vide -> plateau
        | p when p <> Vide && (match p with | Pion {player = p_player} -> p_player <> player | _ -> false) ->
          if player = Player1 then { plateau with joueur1 = p :: plateau.joueur1 }
          else { plateau with joueur2 = p :: plateau.joueur2 }
        | _ -> plateau
      in

      (* Mise à jour des positions *)
      plateau.plateau.(x2).(y2) <- piece;
      plateau.plateau.(x1).(y1) <- Vide;
      
      (* Mettez à jour les propriétés de la pièce déplacée *)
      (match piece with
      | Pion props -> plateau.plateau.(x2).(y2) <- Pion {props with x = x2; y = y2; played = props.played + 1}
      | Cavalier props -> plateau.plateau.(x2).(y2) <- Cavalier {props with x = x2; y = y2}
      | Reine props -> plateau.plateau.(x2).(y2) <- Reine {props with x = x2; y = y2}
      | Roi props -> plateau.plateau.(x2).(y2) <- Roi {props with x = x2; y = y2}
      | Tour props -> plateau.plateau.(x2).(y2) <- Tour {props with x = x2; y = y2}
      | Fou props -> plateau.plateau.(x2).(y2) <- Fou {props with x = x2; y = y2}
      | Vide -> ());
      plateau  (* On retourne le plateau mis à jour *)
    else raise (CoupImpossible end_pos)
  with
  | Invalid_argument _ -> Printf.printf "Mauvaise case. Inexistante"; plateau
  | CoupImpossible (x, y) -> Printf.printf "Coup impossible (%d, %d)" x y; plateau

        


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
  print_endline "Entrez la position de départ (y puis x) inversé :";
  let x1 = read_int () in
  let y1 = read_int () in
  print_endline "Entrez la position d'arrivée (y puis x) :";
  let x2 = read_int () in
  let y2 = read_int () in
  ((x1, y1), (x2, y2))


let trouver_roi (plateau : pion array array) (player : player) =
  let roi_pos = ref None in
  for i = 0 to 7 do
    for j = 0 to 7 do
      match plateau.(i).(j) with
      | Roi {player = p; _} when p = player -> roi_pos := Some (i, j)
      | _ -> ()
    done
  done;
  match !roi_pos with
  | Some pos -> pos
  | None -> failwith "Roi non trouvé sur le plateau"

  let est_en_echec (plateau: pion array array) (player: player) =
    let roi_pos = trouver_roi plateau player in
    let en_echec = ref false in
    for i = 0 to 7 do
      for j = 0 to 7 do
        match plateau.(i).(j) with
        | Vide -> ()
        | Pion props | Cavalier props | Reine props | Roi props | Tour props | Fou props ->
          if props.player <> player then
            (* Si une pièce adverse peut atteindre la position du roi *)
            if coup_valide plateau (i, j) roi_pos then en_echec := true
      done
    done;
    !en_echec


(* Vérifie si une case (x, y) est sous l'attaque d'une pièce adverse *)
let est_sous_attaque (plateau : pion array array) (x : int) (y : int) (adversaire : player) : bool =
  (* Parcourt chaque case du plateau *)
  let sous_attaque = ref false in
  for i = 0 to 7 do
    for j = 0 to 7 do
      match plateau.(i).(j) with
      | Pion props when props.player = adversaire -> 
          (* Si la pièce appartient à l'adversaire et peut atteindre la case (x, y) *)
          if coup_valide plateau (i, j) (x, y) then
            sous_attaque := true
      | _ -> ()
    done;
  done;
  !sous_attaque

let est_echec_et_mat (plateau : plateau) (player : player) =
  if not (est_en_echec plateau.plateau player) then
    false  (* Le joueur n'est pas en échec, donc pas de mat possible *)
  else
    let x, y = trouver_roi plateau.plateau player in
    let mouvements_roi_possibles =
      [ (1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (-1, -1); (1, -1); (-1, 1) ] in

    (* Vérifie si le roi peut se déplacer vers une case non attaquée *)
    let roi_peut_se_deplacer =
      List.exists (fun (dx, dy) ->
        let newX, newY = (x + dx, y + dy) in
        if newX >= 0 && newX < 8 && newY >= 0 && newY < 8 then
          try
            (* Vérifie si le mouvement est possible pour le roi *)
            all_moves1_path_possible (x, y) (newX, newY) &&
            (* Vérifie que la case de destination n'est pas sous attaque *)
            not (est_sous_attaque plateau.plateau newX newY (if player = Player1 then Player2 else Player1))
          with CoupImpossible _ -> false
        else
          false
      ) mouvements_roi_possibles
    in

    (* Si le roi ne peut pas se déplacer et est sous attaque, alors c'est un échec et mat *)
    not roi_peut_se_deplacer


(* Boucle principale du jeu *)
let rec boucle_jeu (plateau : plateau) (tour : player) =
  clear ();
  showGrid plateau;
  let joueur_str = match tour with Player1 -> "Joueur 1" | Player2 -> "Joueur 2" in
  print_endline (joueur_str ^ ", c'est à vous de jouer !");
  
  (* Lire la position de départ et d'arrivée *)
  let (start_pos, end_pos) = lire_position () in
  
  (* Essayer de déplacer la pièce *)
  try
    let new_plateau = deplacer_piece plateau start_pos end_pos tour in
    (* Passer au joueur suivant *)
    let prochain_tour = if tour = Player1 then Player2 else Player1 in
    
    (* Vérifier la condition d'échec et mat *)
    if est_echec_et_mat new_plateau prochain_tour then
      print_endline (joueur_str ^ " a gagné !")
    else
      boucle_jeu new_plateau prochain_tour
  with
    | CoupImpossible (x, y) -> print_endline ("Coup impossible en (" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")");
      boucle_jeu plateau tour



  


(* Démarrage du jeu *)
let lancer_jeu () =
  let plateau_init = init () in
  boucle_jeu plateau_init Player1


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

