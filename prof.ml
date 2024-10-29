
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
          |Cavalier a->()
          |Reine a -> ()
          |Roi c -> ()
          |Tour a -> ()
          |Fou c -> ()
          |Vide -> () ;

      done;
      print_string "\n";
      print_string "\n";
      show (indx+1)
  in 
  show 0;;
showGrid init_test;;


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