open Types

let rec joue_sauve monde=
  match id_lterm_a_destination monde with
    |[] -> (*On ne peut sauver aucun lambda term:*)      
      begin
        match monde.moi.strategie.ramasse_lterm monde with
          |[] -> (*On peut ramasser personne:*)
            begin 
              match monde.moi.strategie.chemin_a_suivre with
                |[] -> (*On n'a pas de chemin de prévu*)
                  begin 
                    match monde.moi.sac_a_bosse with
                      |[]-> (*On n'a personne à sauver: on va se charger un peu en allant chercher des ltermes*)
                        let (chemin1,longueur)= monde.moi.strategie.calcule_chargement monde in
                        if chemin1=[] then Prends [] (*personne à sauver*)
                        else
                          begin
                            let chemin2=monde.moi.strategie.ameliore_chemin monde chemin1 (max 1 (min 20 (longueur*monde.moi.strategie.pourcent_ameliore_chemin/100))) in
                              monde.moi.strategie.chemin_a_suivre<-chemin2;
                              joue_sauve monde;                         
                          end
                      | _-> (*On a quelqu'un à sauver*)
                        let (chemin1,longueur)= monde.moi.strategie.calcule_livraison monde in
                          let chemin2=monde.moi.strategie.ameliore_chemin monde chemin1 
                                          (max 1 (min 30(longueur*monde.moi.strategie.pourcent_ameliore_chemin/100)))
                          in
                          monde.moi.strategie.chemin_a_suivre<-chemin2;
                          joue_sauve monde;
                  end
                |(new_x,new_y)::q-> (*On suit notre itinéraire*)
                  begin
                    monde.moi.strategie.chemin_a_suivre<-q;
                    let (x,y)=monde.moi.suis_la in
                      match (new_x-x,new_y-y) with
                        |(1,0)   -> Deplace Est
                        |((-1),0)-> Deplace Ouest
                        |(0,1)   -> Deplace Sud
                        |(0,(-1))-> Deplace Nord
                        |_ ->
                          (*on a été poussé ou on a attaqué ou on s'est défendu et on n'a pas suivit le chemin prévu*)
                          (*on se déplace de un par tour: si le chemin est erroné c'est qu'on n'est pas allé dans la direction prévue*)
                            monde.moi.strategie.chemin_a_suivre<-
                              (let (chemin,_)=monde.moi.strategie.trouve_chemin monde monde.moi.suis_la (new_x,new_y) in chemin)@q;
                            joue_sauve monde;
                  end
            end
          |id_list -> (*On peut ramasser: on ramasse*)
            Prends id_list;
      end
    |lterms  -> (*On peut sauver des lambda-termes: on les sauve*)
      Pose lterms
(********** renvoit la liste des ltermes dont la destination est la case où l'on se trouve **********)
and id_lterm_a_destination monde= 
  List.filter (fun id -> 
                 let lterm = Hashtbl.find monde.victimes id in 
                   (Some monde.moi.suis_la)=lterm.vers
              ) monde.moi.sac_a_bosse

