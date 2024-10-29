
type player = 
  |Player1
  |Player2

type coup = (* Type pour définir si un coup est faisable indéfiniment *)
  |Infini (* Un coup qui est possible indéfiniment *)
  |PremierCoup (* Possible de jouer le coup si c'est le premier -> Pion au début *)
  |Injouable (* Plus possible de jouer le coup*)

type pionProperties = 
  {
    x : int; (* Couple de position x,y *)
    y : int;
    actions : (int * int * coup) array; (* Indique les actions possibles à partir  (x,y,nb) avec x,y les coordonnées d'un vecteur de déplacement et nb à partir de quand l'action est possible*)
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


(* On initialise les propriétés des différents pions du plateau *)
let pionProp = fun x y -> {x=x;y=y;actions=[| (0,1,Infini);(0,2,PremierCoup) |]; played=0; player=Player1}
let cavalierProp = fun x y -> {x=y;y=y;actions=[| (1,2, Infini);(-2,1,Infini);(2,-1,Infini);(-1,-2,Infini) |];played =0;player=Player1}
let roiProp = fun x y -> {x=x;y=y;actions=[| (1,0,Infini);(-1,0,Infini);(1,1,Infini);(-1,-1,Infini);(0,1,Infini);(0,-1,Infini);(1,-1,Infini);(-1,1,Infini) |];played=0;player=Player1}
(* Je compte changer les pionsProps(actions) par quelque chose qui ne dépend pas d'un nombre 'simple' pour mieux tester ensuite si un pion est sur le chemin
de la Dame par exemple. le champ actions renverrait donc une 'fonction' mathématique linéaire -> La dame peut se déplacer en diagonale donc on choisit la diagonale 
gauche la dame évolue en x en même temps qu'en y avec x et y les 'axes' du tableau plateau.plateau*)

let init_test =

  {joueur1=[];joueur2=[];joueur1Pieces=[|Vide|];joueur2Pieces=[|Vide|];plateau=Array.make_matrix 8 8 (Pion {x=0;y=0;actions=[| (0,1,Infini);(0,2,PremierCoup) |]; played=0;player=Player1})}


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
      |i when i >= Array.length grid.plateau -1 -> print_string "\n";
      |_ -> for i = 0 to Array.length grid.plateau.(indx) -1 do
        match grid.plateau.(indx).(i) with
          |Pion _ -> print_string "♙  "; 
          | Cavalier _ -> print_string"♘  "
          | Reine _ -> print_string "♕  "
          | Roi _ -> print_string "♔  "
          | Tour _ -> print_string "♖  "
          | Fou _ -> print_string "♗  "
          | Vide -> print_string "·  "

      done;
      print_string "\n";
      print_string "\n";
      show (indx+1)
  in 
  show 0;;
showGrid init_test;;


let init () =
  (* Initialisation des pièces pour chaque joueur sur le plateau *)
  let plateau = Array.make_matrix 8 8 Vide in

  (* Placement des pions *)
  for i = 0 to 7 do
    plateau.(1).(i) <- Pion (pionProp i 1);  (* Pions du joueur 2 *)
    plateau.(6).(i) <- Pion (pionProp i 6);  (* Pions du joueur 1 *)
  done;

  (* Placement des tours *)
  plateau.(0).(0) <- Tour (pionProp 0 0);
  plateau.(0).(7) <- Tour (pionProp 7 0);
  plateau.(7).(0) <- Tour (pionProp 0 7);
  plateau.(7).(7) <- Tour (pionProp 7 7);

  (* Placement des cavaliers *)
  plateau.(0).(1) <- Cavalier (cavalierProp 1 0);
  plateau.(0).(6) <- Cavalier (cavalierProp 6 0);
  plateau.(7).(1) <- Cavalier (cavalierProp 1 7);
  plateau.(7).(6) <- Cavalier (cavalierProp 6 7);

  (* Placement des fous *)
  plateau.(0).(2) <- Fou (pionProp 2 0);
  plateau.(0).(5) <- Fou (pionProp 5 0);
  plateau.(7).(2) <- Fou (pionProp 2 7);
  plateau.(7).(5) <- Fou (pionProp 5 7);

  (* Placement des reines *)
  plateau.(0).(3) <- Reine (pionProp 3 0);
  plateau.(7).(3) <- Reine (pionProp 3 7);

  (* Placement des rois *)
  plateau.(0).(4) <- Roi (roiProp 4 0);
  plateau.(7).(4) <- Roi (roiProp 4 7);

  (* Initialisation des pièces de chaque joueur *)
  let joueur1Pieces = Array.init 16 (fun i ->
    match i with
    | 0 | 7 -> Tour (pionProp (i / 7 * 7) 7)  (* Tours aux coins *)
    | 1 | 6 -> Cavalier (cavalierProp i 7)     (* Cavaliers à côté des tours *)
    | 2 | 5 -> Fou (pionProp i 7)              (* Fous *)
    | 3 -> Reine (pionProp 3 7)                (* Reine *)
    | 4 -> Roi (roiProp 4 7)                   (* Roi *)
    | _ -> Pion (pionProp (i - 8) 6)           (* Pions sur la 7ème rangée *)
  ) in

  let joueur2Pieces = Array.init 16 (fun i ->
    match i with
    | 0 | 7 -> Tour (pionProp (i / 7 * 7) 0)  (* Tours aux coins *)
    | 1 | 6 -> Cavalier (cavalierProp i 0)     (* Cavaliers à côté des tours *)
    | 2 | 5 -> Fou (pionProp i 0)              (* Fous *)
    | 3 -> Reine (pionProp 3 0)                (* Reine *)
    | 4 -> Roi (roiProp 4 0)                   (* Roi *)
    | _ -> Pion (pionProp (i - 8) 1)           (* Pions sur la 2ème rangée *)
  ) in

  { joueur1 = []; joueur2 = []; joueur1Pieces; joueur2Pieces; plateau }


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

