open Types
open Common


(*on joue rien: on préfère sauver ou défendre*)
let rien = 0,Prends[]

(**renvoit un couple pari,commande: si pari est différent de 0, il y a une opportunité d'attaque en éxecutant commande**)
let rec joue_attaque monde choix_sauve=
  if monde.moi.strategie.attaque_bloquee then 
  begin
    (*on ne peut pas attaquer indéfiniment: évite les situations de blocage*)
    if monde.moi.strategie.nbr_attaques_successives=0 then monde.moi.strategie.attaque_bloquee<-false
    else monde.moi.strategie.nbr_attaques_successives<-monde.moi.strategie.nbr_attaques_successives-1;
    rien
  end
  else
  begin
  (** TODO: faire attention à la quantité d'eau restante ???**)
    let mechant_list=ref []
    and mechant_dist_1 = ref []
    and mechant_dist_1_ou_2=ref []
    and max_mise = ref 1
    in Hashtbl.iter (fun _ mechant -> let dist=IA_sauve_chemin.distance_sable monde monde.moi.suis_la mechant.est_la in
                                       if dist<3 then
                                       begin
                                         mechant_list:=mechant:: !mechant_list;
                                         mechant_dist_1_ou_2:= mechant:: !mechant_dist_1_ou_2;
                                         if !max_mise<mechant.comportement.mise_optimale then max_mise:=mechant.comportement.mise_optimale;
                                         if dist=1 then mechant_dist_1:= mechant:: !mechant_dist_1
                                       end
                                       else if dist=3 then mechant_list:=mechant:: !mechant_list
                    ) monde.pas_moi;
    if !mechant_list=[] then rien
    else
    begin
      let (alentours,som_bosse,nbr_bosse)=(Common.calcule_alentours monde !mechant_list) in
      let (pousser_maintenant,pousser_tour_dapres,direction) =
        match alentours.(3).(3) with
          |FRANCHISSABLE(POUSSE (dir,proba),_,_) -> 
            begin
              match proba with
                |MAINTENANT _  -> true,false,dir
                |TOUR_DAPRES _ -> false,true,dir
                | _-> print_string "IA_attaque.ml: Situation incongrue"; false,false,Nord
            end
          |_-> false,false,Nord
      in
        (** 1. : si on peut pousser quelqu'un au contact: on le fait*)
        if pousser_maintenant then
          Common.joue 
                monde 
                (mise_pousse_omega monde mechant_dist_1_ou_2) 
                (Deplace direction) 
                !mechant_dist_1_ou_2
                true
        else (* on regarde si on peut pousser dans l'omega en moins de monde.moi.strategie.max_dist_omega coups *)
        let omega=ref false
        and dir = ref Nord in
        List.iter (fun mechant -> if !omega then () else
          let (xm,ym)=mechant.est_la
          and (x ,y )=monde.moi.suis_la
          in
          let dx=xm-x
          and dy=ym-y
          in
          let i=ref (x+dx)
          and j=ref (y+dy)
          in
            try
              for k=1 to monde.moi.strategie.max_dist_omega do
                match Jouer.case_carte monde.desert_hostile (!i,!j) with
                  |Omega -> omega:=true; raise Not_found
                  |_ -> i:= !i+dx; j:= !j+dy
              done;
              raise Not_found
            with |_-> 
              if !omega then dir:= Common.get_direction (dx,dy)
        ) !mechant_dist_1; 
        (** 2. : si on peut pousser dans un omega en moins de monde.moi.strategie.max_dist_omega coups: on le fait*)
        if !omega then
          Common.joue 
             monde 
             (2* !max_mise)
             (Deplace !dir) 
             !mechant_dist_1_ou_2
             false
        (** 3. : si on peut pousser quelqu'un qui n'est pas au contact mais qui va peut être y aller: on le fait*)
        else if pousser_tour_dapres then
          Common.joue 
             monde 
             (-(mise_pousse_omega monde mechant_dist_1_ou_2))
             (Deplace direction) 
             !mechant_dist_1_ou_2
             true
        else (*on sélectionne la meilleur case des voisins*)
        let dir= ref Nord in
        let (i,j)=List.fold_left 
           (fun (i,j) ((dx,dy) as d)-> if (cmp_case alentours (3+dx) (3+dy) i j)=1 then begin dir:=Common.get_direction d; (3+dx,3+dy) end else (i,j))
           (4,3)
           [(-1,0);(0,1);(0,-1)]
        in
          let best_case = alentours.(i).(j) in
          match best_case with
        (** 4. : si on peut pousser quelqu'un au prochain tour: on essaye **)
            |FRANCHISSABLE(POUSSE _,_,_) -> if !max_mise=1 then 1,Deplace !dir
                                                           else 
              Common.joue 
                 monde 
                 !max_mise
                 (Deplace !dir) 
                 !mechant_dist_1_ou_2
                 false
            |_ ->
        begin let action =
          match choix_sauve with
        (** 5. : si Sauve préconise de ramasser des ltermes: voyons si ca vaut le coup **)
          | Prends l when l<>[] -> 
            let nbr = ref 0
            and som = ref 0
            in
            List.iter (fun id -> 
              let term = Hashtbl.find monde.victimes id in match term.taille with |None -> () |Some taille -> incr nbr; som:= !som+taille)
              l;
            let nbr_mechant_contact = (List.length !mechant_dist_1) in
            let gain =
              if nbr_mechant_contact = 0 then !som
                                         else !som - (!som+som_bosse)/(!nbr+nbr_bosse)
            in
              if gain>monde.moi.strategie.pourcent_gain_min*monde.moi.capacite/100 then
              begin
                if nbr_mechant_contact = 0 then (1,choix_sauve)
                                           else
                  Common.joue 
                     monde 
                     !max_mise
                     choix_sauve 
                     !mechant_dist_1_ou_2
                     false
              end 
              else rien
          | _ -> rien
        in if not (action=rien) then action
        else
        match best_case with
        (** 6. : si un méchant est sur une base: on le jarrte parce que les bases, c'est pour nous **)
        |FRANCHISSABLE(_,_,BASE) -> begin
          let (dx,dy)=Common.get_vector !dir in
          let (x,y)=monde.moi.suis_la in
          if mechant_on alentours (3+dx) (3+dy) then
            try match Jouer.case_carte monde.desert_hostile (x+2*dx,y+2*dy) with
              |Palmier -> rien
              |Omega |Base | Sable ->
                Common.joue 
                   monde 
                   !max_mise
                   (Deplace !dir) 
                   !mechant_dist_1_ou_2
                   false
            with |_-> rien
          else rien
        end
        (** 7. : si le gain est suffisant, on peut pousser un méchant pour récupérer ses lambdas termes **)
        |FRANCHISSABLE(_,_,SABLE gain) ->
            if gain>monde.moi.strategie.pourcent_gain_min*monde.moi.capacite/100 then begin
              let (dx,dy)=Common.get_vector !dir in
              let (x,y)=monde.moi.suis_la in 
              if mechant_on alentours (3+dx) (3+dy) then
              try match Jouer.case_carte monde.desert_hostile (x+2*dx,y+2*dy) with
                |Palmier -> rien
                |Omega |Base | Sable ->
                  Common.joue 
                     monde 
                     !max_mise
                     (Deplace !dir) 
                     !mechant_dist_1_ou_2
                     false
              with |_-> rien
              else rien
            end
            else
              rien
        |_ -> rien
      end
    end
  end

(** si on a une chance de pousser un adversaire dans l'omega: on met la mise max vu qu'il va miser beaucoup aussi**)
and mise_pousse_omega monde mechant_list=
  let max_ultime = ref 0 in
    List.iter (fun mechant -> if mechant.comportement.mise_ultime> !max_ultime then max_ultime:=mechant.comportement.mise_ultime)
      !mechant_list;
    let mise_ultime = max !max_ultime (10*Common.max_mise monde) in
    let nbr_tours_restant= Int64.of_int (max 1 (monde.moi.strategie.max_tours - monde.nbr_tours)) in
    let eau_pour_combat= Int64.sub (Int64.of_int (monde.moi.bosse)) nbr_tours_restant in
    let eau_combat_80_pct = Int64.to_int (Int64.div (Int64.mul eau_pour_combat (Int64.of_int 80)) (Int64.of_int 100)) in
    max 2 (min eau_combat_80_pct mise_ultime)


