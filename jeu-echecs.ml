type coup = (* Type pour définir si un coup est faisable indéfiniment *)
  |Infini (* Un coup qui est possible indéfiniment *)
  |PremierCoup (* Possible de jouer le coup si c'est le premier -> Pion au début *)
  |Injouable (* Plus possible de jouer le coup*)

type pionProperties = 
  {
    x : int; (* Couple de position x,y *)
    y : int;
    actions : (int * int * coup) array; (* Indique les actions possibles à partir  (x,y,nb) avec x,y les coordonnées d'un vecteur de déplacement et nb à partir de quand l'action est possible*)
    played : int (* Indique le nombre de fois que ce pion a été joué depuis le début *)
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
let pionProp = fun x y -> {x=x;y=y;actions=[| (0,1,Infini);(0,2,PremierCoup) |]; played=0}
let cavalierProp = fun x y -> {x=y;y=y;actions=[| (1,2, Infini);(-2,1,Infini);(2,-1,Infini);(-1,-2,Infini) |];played =0}
let roiProp = fun x y -> {x=x;y=y;actions=[| (1,0,Infini);(-1,0,Infini);(1,1,Infini);(-1,-1,Infini);(0,1,Infini);(0,-1,Infini);(1,-1,Infini);(-1,1,Infini) |];played=0}


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

