open Types

(** initialise les paramètres de l'IA:         **)
(** prend en argumet un couple (w,h) où w et h **)
(** sont respectivement la largeur et la       **)
(** de la carte                                **)
let set_strategie (w,h) = 
  let aire_carte=w*h in  (*description plus détaillée dans types.mli*)
{
   (************* paramètres réglables ************)
              max_tours=3000;
              pourcent_mise_opt_initiale=20;  (*mise optimale en duel initialisée à 20% de l'eau réservée au combat*)
              max_attaques_successives=6;     (*on évite de rester trop longtemps en mode attaque, ca sert à rien*)
              max_aller_retour=5;             (*à partir de quand on peut dire qu'on est bloqué*)
              ajuste_win = 0.9;               (*quand on gagne, on multiplie la mise par ce coeff: ca sera la prochaine mise*)
              ajuste_loose = 3.1;             (*quand on perd, on multiplie la mise par ce coeff: ca sera la prochaine mise*)
              pourcent_ameliore_chemin = 10;  (*couloir de 10% de la longueur du chemin *)
              pourcent_taille_min = 20;       (*on évite de ramasser les lterm qui font moins de 20% de la capacité*)
              pourcent_gain_min = 15;    (*on attaque si on est vide à 70%*)              
              max_dist_omega = 4; (*distance max d'un omega dans lequel on essaye de pousser quelqu'un*)

              (* Stratégies pour sauver les ltermes *)              
              trouve_chemin=IA_sauve_chemin.chemin_A_star; (* chemin_pas_si_naif/chemin_dijkstra/chemin_A_star *)
              ramasse_lterm=IA_sauve_ramasse.ramasse_ratio_borne_inf; (* ramasse_stupide/ramasse_moins_stupide/ramasse_ratio/ramasse_ratio_borne_inf/ramasse_ratio_borne_inf_mieux *)
              calcule_livraison=IA_sauve_livre.depose_moins_stupide;(* depose_stupide/depose_moins_stupide/depose_ratio *)
              ameliore_chemin=IA_sauve_chemin.ameliore_chemin_stupide; (* ameliore_chemin_stupide/ameliore_chemin_intelligent *)
              calcule_chargement=IA_sauve_livre.meilleur_ratio; (* base_la_plus_proche/meilleur_ratio *)
              distance=IA_sauve_chemin.distance_sable; (* distance_sable/distance_dijkstra/distance_A_star *)

              (*variables internes, ne pas toucher*)
              nbr_attaques_successives=0;
              attaque_bloquee=false;
              cible=None;
              last_direction=None;
              nbr_aller_retour=0;
              ajuste_mise=None;
              chemin_a_suivre=[];
              last_moi_pos=(0,0);
              somme_ratio_bases=0.0;
              nbr_bases=0;
              pred_dij_depuis_moi=Hashtbl.create aire_carte;
              waiting_depuis_moi=Hashtbl.create aire_carte;
              restant_depuis_moi=(Tas_fibo.new_t ((0,0),(0,0),0) 0);
              pred_dij_autre=Hashtbl.create aire_carte;
              waiting_autre=Hashtbl.create aire_carte;
}


(** renvoit une commande à jouer **)
let rec joue monde = 
  let sauve= 
    match monde.moi.strategie.nbr_aller_retour with 
       |n when n<monde.moi.strategie.max_aller_retour -> IA_sauve.joue_sauve monde
       |_ -> Common.output_log monde "(Tentative de déblocage)\n";
            monde.moi.strategie.nbr_attaques_successives<- 3;
            monde.moi.strategie.attaque_bloquee<-true;
            match monde.moi.strategie.last_direction with (* si trop d'aller retours *)
              |None -> (IA_defendre.commande_sure monde [Nord;Sud;Est;Ouest]) (* pourquoi pas? *)
              |Some d when d=Est || d=Ouest -> (IA_defendre.commande_sure monde [Nord;Sud;d;if d=Est then Ouest else Est])
              |Some d -> (IA_defendre.commande_sure monde [Ouest;Est;d;if d=Nord then Sud else Nord])
 in 
  let (mise_defense1,defense1)= IA_defendre.danger_de_mort monde sauve in
    (*On est en danger de mort, on réagit*)
    if mise_defense1<>0 then
    begin
      Common.output_log monde "Défendre:";
      Common.output_log_action monde defense1;
      (mise_defense1,defense1)
    end  
    else 
    let (mise_attaque,attaque) = IA_attaque.joue_attaque monde sauve in
      let (mise,action) =
        (*On peut attaquer un ennemi: on s'y donne à coeur joie*)
        if mise_attaque<>0 then
        begin
          monde.moi.strategie.nbr_attaques_successives<-monde.moi.strategie.nbr_attaques_successives+1;
          if monde.moi.strategie.nbr_attaques_successives=monde.moi.strategie.max_attaques_successives then
            monde.moi.strategie.attaque_bloquee<-true;
            Common.output_log monde "Attaque:";
            Common.output_log_action monde attaque;
            (mise_attaque,attaque)
        end
        else
        begin
          monde.moi.strategie.nbr_attaques_successives<- 0;
          let (mise_defense2,defense2)= IA_defendre.danger_prochain_tour monde sauve in
          (*on risque quelque chose, on joue en conséquence*)
          if mise_defense2<>0 then
          begin
            Common.output_log monde "Défendre:";
            Common.output_log_action monde defense2;
            (mise_defense2,defense2)
          end  
          else
          begin
            (*on joue normalement*)
            Common.output_log monde "Sauve:";
            Common.output_log_action monde sauve;
            1,sauve
          end
        end
    in 
    (mise,action)    
