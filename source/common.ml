open Types

(**transforme une direction en chaîne de caractère**)
let string_of_direction d=
  match d with
    | Est -> "Est"
    | Ouest -> "Ouest"
    | Sud -> "Sud"
    | Nord -> "Nord"

(** écrit dans le fichier log **)
let output_log monde s=
  match monde.params.outchannel with
    |None -> ()
    |Some oc -> output_string oc s

let output_log_action monde action=
  match action with
    |Prends l -> output_log monde "Prends"; List.iter (fun id -> output_log monde (string_of_int id); output_log monde "") l; output_log monde "\n";
    |Pose l   -> output_log monde "Pose"; List.iter (fun id -> output_log monde (string_of_int id); output_log monde "") l; output_log monde "\n";
    |Deplace dir -> output_log monde ("Deplace "^(string_of_direction dir)^"\n")

(**joue en se souvenant de l'action executée pour voir si on perd ou on gagne le duel**)
let joue monde mise commande mechant_list ultime=
  let action=match commande with
              |Deplace dir -> Abouge dir
              |Prends l -> Apris l
              |Pose l -> Apose l
  in
  monde.moi.strategie.ajuste_mise<- Some(mise,action,mechant_list,ultime);
  (mise,commande)

(**transforme un vecteur en une direction**)
let get_direction v=
  match v with
    |(1,0)  -> Est
    |(-1,0) -> Ouest
    |(0,1)  -> Sud
    |(0,-1) -> Nord
    | _ -> Nord

(**transforme une direction en un vecteur**)
let get_vector d=
  match d with
    | Est -> (1,0)
    | Ouest -> (-1,0)
    | Sud -> (0,1)
    | Nord -> (0,-1)

(**calcul la mise max que l'on peut miser pour pousser un méchant **)
let max_mise monde=
  let nbr_tours_restant= Int64.of_int (max 1 (monde.moi.strategie.max_tours - monde.nbr_tours)) in
  let eau_pour_combat= Int64.sub (Int64.of_int (monde.moi.bosse)) nbr_tours_restant in  
  let nbr_tours = Int64.of_int monde.nbr_tours in
  let nbr_combats = Int64.of_int monde.nbr_combats in
  Int64.to_int (Int64.div (Int64.mul eau_pour_combat nbr_tours) (Int64.mul nbr_tours_restant nbr_combats))

(**ajuste la mise optimale en cas de victoire**)
let ajuste_mise_win monde mechant mise ultime=
  let mise= abs mise in
  if ultime then
  begin
    output_log monde  ("J'ai gagné un duel à mort contre "^(string_of_int mechant.id_mechant)^", j'avais misé "^(string_of_int mise)^".\n");
    mechant.comportement.mise_ultime<-int_of_float((float_of_int mechant.comportement.mise_ultime)*. monde.moi.strategie.ajuste_win);
    mechant.comportement.mise_ultime<-min mechant.comportement.mise_ultime (max 2 (max_mise monde))
  end
  else
  begin 
    output_log monde  ("J'ai gagné contre "^(string_of_int mechant.id_mechant)^", j'avais misé "^(string_of_int mise)^".\n");  
    if mise<mechant.comportement.mise_optimale && mise<> 1 then 
      mechant.comportement.mise_optimale<-mise
    else
      if mise<>1 then
        mechant.comportement.mise_optimale<-int_of_float((float_of_int mechant.comportement.mise_optimale)*. monde.moi.strategie.ajuste_win);
    mechant.comportement.mise_optimale<-max 2 mechant.comportement.mise_optimale;
  end

(**ajuste la mise optimale en cas de défaite**)
let ajuste_mise_loose monde mechant mise ultime=
  let mise= abs mise in
  if ultime then
  begin
    output_log monde  ("J'ai perdu un duel à mort contre "^(string_of_int mechant.id_mechant)^", j'avais misé "^(string_of_int mise)^".\n");
    (*si on a perdu un duel à mort c'est qu'on est du bon coté de l'omega: l'autre a résité*)
    mechant.comportement.mise_ultime<-int_of_float((float_of_int mise)*. monde.moi.strategie.ajuste_loose);
  end
  else
  begin
    output_log monde  ("J'ai perdu contre "^(string_of_int mechant.id_mechant)^", j'avais misé "^(string_of_int mise)^".\n");
    mechant.comportement.mise_optimale<-int_of_float((float_of_int mise)*. monde.moi.strategie.ajuste_loose);
    mechant.comportement.mise_optimale<-min mechant.comportement.mise_optimale (max 2 (max_mise monde))
  end

(**renvoit vrai ssi si on peut se déplacer vers dir, ie s'il n'y a pas de palmier ou d'omega**)
and depl_possib monde dir= 
  let x,y=monde.moi.suis_la in
  let nx,ny= (*les éventuelles nouvelles coordonnées*)
    match dir with
      |Nord -> x,(y-1)
      |Sud  -> x,(y+1)
      |Est  -> (x+1),y
      |Ouest-> (x-1),y
  in
   try
     match Jouer.case_carte monde.desert_hostile (nx,ny) with
      |Sable   |Base  -> true
      |Palmier |Omega -> false 
   with
     |_-> false


(**Fonctions pour l'attaque et la défense, en fait, surtout pour l'attaque**)

type probabilite =    (*la probabilite qu'un méchant soit sur une case au prochain tour*)
  | MAINTENANT of int (*le méchant est sur la case actuellement*)
  | TOUR_DAPRES of int(*le méchant n'est pas sur la case actuellement*)
  | ZERO

type securite = (*sécurité d'une case du désert: dans l'ordre de priorité*)
  | DANGEREUX   (*c'est case est dangereuse: si on y va, on risque d'être poussé par un autre dans un omega*)
  | POUSSE of direction*probabilite   (*case strategique: on peut pousser un méchant dans l'omega*)
  (*POUSSE([direction vers laquelle on pousse],[proba que le méchant soit sur la case])*)
(*POUSSE([direction dans laquelle il faut pousser])*)
  | RIEN

type ramasse =    (*le gain potentiel que l'on peut attendre en lterm allant sur la case *)
  | BASE          (*c'est une base*)
  | SABLE of int  (*somme des tailles des ltermes à rammasser*)

type case = (*une case du damier*)
  |INFRANCHISSABLE (*soit elle est infranchissable*)
  |FRANCHISSABLE of securite*probabilite*ramasse  (*soit elle est plus interessane et franchissable*)
 (*FRANCHISSABLE([securite de la case],[proba qu'un méchant aille sur la case],[ce qu'on peut rammaser])*)

(*respecte les priorités: d'abord DANGEREUX ensuite POUSSE ensuite RIEN*)
let rec set_securite t i j new_s=
  match t.(i).(j) with
    |INFRANCHISSABLE -> ()
    |FRANCHISSABLE (s,p,r) ->
      if (cmp_securite t new_s s ((i,j)=(3,3))) = 1 then t.(i).(j)<-FRANCHISSABLE (new_s,p,r)
(*respecte les priorités: d'abord MAINTENANT ensuite TOUR_DAPRES ensuite ZERO*)
and set_probabilite t i j new_p=
  match t.(i).(j) with
    |INFRANCHISSABLE -> ()
    |FRANCHISSABLE (s,p,r) ->
    begin
      match p,new_p with
        |MAINTENANT p,MAINTENANT new_p -> t.(i).(j)<-FRANCHISSABLE (s,MAINTENANT (p+new_p),r)
        |_,MAINTENANT _ -> t.(i).(j)<-FRANCHISSABLE (s,new_p,r)
        |TOUR_DAPRES p,TOUR_DAPRES new_p -> t.(i).(j)<-FRANCHISSABLE (s,TOUR_DAPRES (p+new_p),r)
        |ZERO,TOUR_DAPRES _ -> t.(i).(j)<-FRANCHISSABLE (s,new_p,r)
        |_-> ()
    end
(*renvoit proba qu'un méchant aille sur une case*)
and get_probabilite t i j=
  if i<0 || j<0 || i>6 || j>6 then None
  else
    match t.(i).(j) with
    |INFRANCHISSABLE -> None
    |FRANCHISSABLE (s,p,r) -> Some p
(*renvoit 1 si s1>s2 0 si s1=s2 et -1 si s1<s2*)
and cmp_securite t s1 s2 centred=
  match s1,s2 with  
    |DANGEREUX,DANGEREUX -> 0
    |DANGEREUX,_         -> 1
    |_,DANGEREUX         -> (-1)
    |POUSSE (_,proba1),POUSSE (_,proba2) -> begin
      match cmp_probabilite (Some proba1) (Some proba2) with
        | 1 -> if centred then 1 else (-1)
        | (-1) -> if centred then (-1) else 1
        |_-> 0
      end
    |POUSSE (_,MAINTENANT _),RIEN   -> if centred then 1 else (-1)
    |POUSSE (_,TOUR_DAPRES _),RIEN  -> 1
    |RIEN,POUSSE (_,MAINTENANT _)   -> if centred then (-1) else 1
    |RIEN,POUSSE (_,TOUR_DAPRES _)  -> (-1)
    |_                              -> 0     
(*renvoit 1 si p1>p2 0 si p1=p2 et -1 si p1<p2*)
and cmp_probabilite p1 p2=
  match p1,p2 with
    |None,None -> 0
    |_,None    -> 1
    |None,_    -> (-1)
    |Some p1,Some p2 ->
    begin
      match p1,p2 with
        |MAINTENANT p1,MAINTENANT p2 -> compare p1 p2
        |MAINTENANT _,_              -> 1
        |_,MAINTENANT _              -> (-1)
        |TOUR_DAPRES p1,TOUR_DAPRES p2 -> compare p1 p2
        |TOUR_DAPRES p1,_              -> 1
        |_,TOUR_DAPRES p2              -> (-1)
        |ZERO,ZERO                     -> 0
    end
(*renvoit 1 si c1>c2 0 si c1=c2 et -1 si c1<c2*)
(*on préfère dans l'ordre croissant:
INFRANCHISSABLE
FRANCHISSABLE(DANGEREUX,_,_)
FRANCHISSABLE(RIEN,_,SABLE _)
FRANCHISSABLE(RIEN,_,BASE)
FRANCHISSABLE(POUSSE,MAINTENANT _,_)
FRANCHISSABLE(POUSSE,TOUR_DAPRES _,_)
*)
and cmp_case t i1 j1 i2 j2=
  match t.(i1).(j1),t.(i2).(j2) with
    |INFRANCHISSABLE,INFRANCHISSABLE -> 0
    |INFRANCHISSABLE,_ -> (-1)
    |_,INFRANCHISSABLE -> 1
    |FRANCHISSABLE(s1,_,r1),FRANCHISSABLE(s2,_,r2)->
    begin
      match s1,s2 with
        |DANGEREUX,DANGEREUX -> 0
        |DANGEREUX,_         -> (-1)
        |_,DANGEREUX         -> 1
        |_ ->
        begin
          match cmp_securite t s1 s2 ((i1,j1)=(3,3)) with
            | 0 ->
            begin
              match r1,r2 with
                |BASE,BASE -> 0
                |BASE,_    -> 1
                |_,BASE    -> (-1)
                |SABLE r1,SABLE r2 -> compare r1 r2 
            end
            |x -> x
        end
    end
(* renvoit true ssi un méchant est en (i,j) *)
and mechant_on t i j=
  match t.(i).(j) with
    |FRANCHISSABLE(_,MAINTENANT _,_) -> true
    | _ -> false
    
(**renvoit un tableau 7*7, centré sur notre chameau, indiquant les cases   
   dangereuses et la proba qu'un chameau aille sur telle ou telle case,    
   ainsi que la somme des tailles de ltermes qu'on porte et leur nombre **)
let calcule_alentours monde mechant_list=
  let t=Array.make_matrix 7 7 INFRANCHISSABLE in  (*tableau des alentours*)
  let ramasse_t=Array.make_matrix 7 7 (0,0) in  (*tableau des lterms que l'on peut ramasser*)  
  let (x_moi,y_moi)=monde.moi.suis_la in
  let xd=x_moi-3 and xf=x_moi+3
  and yd=y_moi-3 and yf=y_moi+3
  in
  (** marque la sécurité des cases **)
  let rec marque_omega i j=
    List.iter (fun (dx,dy,direction_pousse)-> 
      let x=i-xd+dx in let x2=x+dx in
      let y=j-yd+dy in let y2=y+dy in
      if x2<0 || y2<0 || x2>6 || y2>6 then () (*trop loin, on laisse tomber*)
      else
      match t.(x).(y) with 
        | INFRANCHISSABLE -> () (* du type ~#. : on peut pas pousser dans l'omega *)
        | FRANCHISSABLE (_,p,_) -> 
          if p=ZERO then () (*si personne peut aller ici, c'est pas interessant*)
          else
          begin
            begin
            match t.(x2).(y2) with
              |FRANCHISSABLE (_,MAINTENANT _,_) |FRANCHISSABLE (_,TOUR_DAPRES _,_) ->
                set_securite t x y DANGEREUX
              | _ -> ()
            end;
            set_securite t x2 y2 (POUSSE (direction_pousse,p)); 
          end           
              ) [(1,0,Ouest);(-1,0,Est);(0,1,Nord);(0,-1,Sud)]
  (** taille potentielle du lambda terme qu'on peut voler **)
  and vol_potentiel mechant=
    (*somme de ceux qu'on peut voler / nombre de lambda termes en tout *)
    let somme=ref 0.0
    and nbr = ref 0
    in List.iter (fun id -> let term = Hashtbl.find monde.victimes id in incr nbr;
      match term.taille with
        |None -> somme:= !somme +. (float_of_int monde.moi.cerveau)/. 2.0 
                                  (*=taille_moyenne*proba de le prendre=capacité/2*cerveau/capacité*)
        |Some taille -> if taille<=monde.moi.cerveau then somme:= !somme+. (float_of_int taille)
                 ) mechant.otages;
     let vol= !somme /. (float_of_int !nbr) in
     int_of_float vol
  (**Calcule la proba qu'un méchant soit sur telle case**)
  and marque_proba mechant=
    (*on calcule x et y: coord du méchant dans le tableau t*)
    let (x_mech,y_mech)=mechant.est_la
    in
    let x=x_mech-x_moi+3
    and y=y_mech-y_moi+3
    in
    (*le poids de chaque direction: [|N;O;S;E|]*)
    let poids=[|2;2;2;2|] in(*au début, un quart de proba que le chameau aille d'un coté*)
    let poids_total= ref 9 in (*9 parce que le méchant peut aussi rester sur place*)
    (*le tableau des décalages associé aux poids*)
    let d=[|(0,-1);(-1,0);(0,1);(1,0)|] in
    (*si le chameau a avancé la dernière fois, il est plus probable qu'il continue dans la même direction*)
    for k=0 to 3 do
      let (dx,dy)=d.(k) in
        if (x_mech+dx,y_mech+dy)=mechant.comportement.last_pos then
        begin
          poids.(k)<- 1;   (*le chameau ne fera probablement pas de retour en arrière*)
          (*il continuera plus probablement tout droit*)
          if poids.((k+2) mod 4)<>0 then poids.((k+2) mod 4)<- 3 else poids_total:= !poids_total-1 
        end;
        let i=x+dx
        and j=y+dy
        in if i<0 || j<0 || i>6 || j>6 then begin poids_total:= !poids_total-poids.(k); poids.(k)<- 0 end
           else match t.(i).(j) with
                  |INFRANCHISSABLE -> poids_total:= !poids_total-poids.(k); poids.(k)<- 0;
                  | _ -> ()
    done;
    (*on calcule ce qu'on peut voler à ce méchant*)
    let vol = vol_potentiel mechant in
    (* enfin on calcule la proba et on l'ajoute a la proba déjà là*)
    for k=0 to 3 do
      let (dx,dy)=d.(k) in
      let i=x+dx
      and j=y+dy
      in if i<0 || j<0 || i>6 || j>6 then ()
         else 
         begin
           set_probabilite t i j (TOUR_DAPRES (poids.(k)*100/ !poids_total));
           let (som,nbr)=ramasse_t.(i).(j) in 
             ramasse_t.(i).(j)<-(som+poids.(k)*vol/ !poids_total,nbr+1)            
         end             
    done;
    set_probabilite t x y (MAINTENANT (100/ !poids_total));
    let (som,nbr)=ramasse_t.(x).(y) in 
      ramasse_t.(x).(y)<-(som+vol/ !poids_total,nbr+1) 
  (**on regarde si telle case est franchissable ou pas, et on se souvient des omegas**)
  and marque_terrain ()=
    let omega_pos = ref [] in   
    for i=xd to xf do
      for j=yd to yf do
        try
          match Jouer.case_carte monde.desert_hostile (i,j) with
            | Sable   -> t.(i-xd).(j-yd)<-FRANCHISSABLE(RIEN,ZERO,SABLE 0)
            | Base    -> t.(i-xd).(j-yd)<-FRANCHISSABLE(RIEN,ZERO,BASE)                    
            | Palmier -> ()
            | Omega   -> omega_pos:= (i,j):: !omega_pos;
        with
          |_-> ()
      done
    done;
    !omega_pos
  (**on calcule les gains potentiels**)
  and marque_ramasse ()=
    let somme_sac_a_bosse=ref 0
    and nbr_sac_a_bosse = ref 0
    in
      List.iter (fun id -> let term = Hashtbl.find monde.victimes id in incr nbr_sac_a_bosse;
        match term.taille with
          |None -> ()
          |Some taille -> somme_sac_a_bosse:= !somme_sac_a_bosse + taille
                ) monde.moi.sac_a_bosse;
    for i=0 to 6 do
      for j=0 to 6 do
        match t.(i).(j) with
          |FRANCHISSABLE(s,p,SABLE _) ->
            let (som,nbr)=ramasse_t.(i).(j) in
              if p=ZERO then 
                (*on gagne la somme et on perd aucun lterm*) 
                t.(i).(j)<-FRANCHISSABLE(s,p,SABLE som)
              else
              (*on gagne la somme mais on va perdre un lterm si l'autre nous pousse*)
              begin
                let tot_som = som+ !somme_sac_a_bosse
                and tot_nbr = nbr+ !nbr_sac_a_bosse
                in
                  let real_som=som-tot_som/tot_nbr in
                  t.(i).(j)<-FRANCHISSABLE(s,p,SABLE real_som)
              end
           |_-> ()
      done
    done;
    (!somme_sac_a_bosse, !nbr_sac_a_bosse)
  in
    (*on regarde si les cases sont franchissables ou pas*)
    let omega_pos = marque_terrain () in
    (*on vérifie que en poussant, on ne se mette pas dans une situation dangereuse*)
    for i=0 to 6 do
      for j=0 to 6 do
        match t.(i).(j) with
          |FRANCHISSABLE(POUSSE (dir,_),p,r) -> begin
            let (dx,dy)=get_vector dir in  
            match t.(i+dx).(j+dy) with
              |FRANCHISSABLE(DANGEREUX,_,_) -> t.(i).(j)<-FRANCHISSABLE(RIEN,p,r)
              |_ -> ()
            end
          |_ -> ()
      done
    done;
    (*on calcule les proba de déplacement*)
    List.iter marque_proba mechant_list;
    (*on calcule la securité des 8 cases aux alentours des omegas*)
    List.iter (fun (i,j) -> marque_omega i j) omega_pos;
    (*on s'interesse aux lterms dans le coin*)
    Hashtbl.iter (fun _ term ->
      let (xt,yt) = term.la in
      (*si le terme est dans le tableau 7*7*)
      if (abs (xt-x_moi))<=3 && (abs (yt-y_moi))<=3 then
        let (som,nbr)=ramasse_t.(xt-x_moi+3).(yt-y_moi+3) in
        let taille = match term.taille with |None -> monde.moi.capacite/2 |Some taille -> taille in
        if som+taille<=monde.moi.cerveau then
          ramasse_t.(xt-x_moi+3).(yt-y_moi+3)<-(som+taille,nbr+1)
                 ) monde.victimes;
   (*on calcule les gains potentiels*)
    let (som_bosse,nbr_bosse)=marque_ramasse () in
  (*Graphics.open_graph "";
  let size_rect = 24 in
  for i=0 to 6 do
    for j=0 to 6 do
      let (color,ramasse)=
        match t.(i).(j) with
          |INFRANCHISSABLE              -> Graphics.black,ZERO
          |FRANCHISSABLE (RIEN,p,r) -> Graphics.yellow,p
          |FRANCHISSABLE (POUSSE _,p,r) -> Graphics.green,p
          |FRANCHISSABLE (DANGEREUX,p,r) -> Graphics.red,p
      in
      (*let ramasse = match ramasse with
                  |BASE -> "@"
                  |SABLE i -> if i=0 then "" else (string_of_int i)*)
      let ramasse = match ramasse with
                  |ZERO -> "0"
                  |MAINTENANT p -> string_of_int p
                  |TOUR_DAPRES p-> string_of_int p
      in
      Graphics.set_color color;
      let x=size_rect*i
      and y=size_rect*(6-j)
      in
        Graphics.fill_rect x y (size_rect-1) (size_rect-1);
        Graphics.set_color Graphics.black;
        Graphics.moveto (x+4) (y+4);
        Graphics.draw_string ramasse;
    done
  done;
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in 
    Graphics.close_graph ();*)
  (t,som_bosse,nbr_bosse)
  

