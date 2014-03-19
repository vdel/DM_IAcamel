open Types

(*********** Pour déposer les ltermes *********)
(*dépose le premier de la liste*)
let rec depose_stupide monde= 
  match monde.moi.sac_a_bosse with
    |[] -> print_string "Sac vide, c'est pas normal, j'abandonne. (IA_Sauve_livre.ml)"; exit(-1)
    |id::q ->
      begin 
        let term = Hashtbl.find monde.victimes id in
          match term.vers with
            |None -> print_string "Direction inconnue, c'est pas normal, j'abandonne. (IA_Sauve_livre.ml)"; exit(-1)
            |Some pos -> monde.moi.strategie.trouve_chemin monde monde.moi.suis_la pos
      end
 
(*on dépose d'abord celui qui est le plus près*) 
let rec depose_moins_stupide monde =
  (*trouve le lambda-terme dont la destination est la plus proche de p *)
   let dist_min= ref max_int in
   let plus_proche= ref (0,0) in
     List.iter (fun id -> let lt = Hashtbl.find monde.victimes id in 
                                         match lt.vers with 
                                          |None -> ()
                                          |Some pos -> (
                                                let dist=monde.moi.strategie.distance monde monde.moi.suis_la pos in
                                                if dist< !dist_min then begin dist_min:=dist; plus_proche:=pos end
                                            )
               ) monde.moi.sac_a_bosse;
    monde.moi.strategie.trouve_chemin monde monde.moi.suis_la !plus_proche


(*on dépose d'abord ceux qui ont le meilleur ratio taille/distance*)  
let depose_ratio monde =
  let sac = List.map (fun id ->
    let term= Hashtbl.find monde.victimes id in
      match term.taille,term.vers with
        |None,_ |_,None -> print_string "Bug dans ramasse_vite: on devrait connaitre la taille du lambda terme"; ((0,0),0.0,0)
        |Some taille,Some dest -> let dist=monde.moi.strategie.distance monde monde.moi.suis_la dest in
      (dest,(float_of_int taille)/.(float_of_int dist),dist)
                           ) monde.moi.sac_a_bosse in
    let max_ratio = ref (-1.0)
    and pos = ref (0,0)
    and min_dist = ref max_int
    in
    List.iter (fun (dest,ratio,dist) ->
      if ratio= !max_ratio then
      begin
        if !min_dist>dist then   
          begin pos:=dest; min_dist:=dist end
      end
      else if ratio> !max_ratio then begin max_ratio:=ratio; pos:=dest; min_dist:=dist end
    ) sac;
    monde.moi.strategie.trouve_chemin monde monde.moi.suis_la !pos
    
(*********** Pour s'approvisionner en ltermes *********)
let rec base_la_plus_proche monde=
  match monde.bases with
    |[]-> (* on cherche le lambda terme le plus proche*)
      let dist_min_sol=ref 1000000000
      and pos_sol = ref (0,0)
      and dist_min_chameau = ref 1000000000
      and id_chameau = ref (-1)
      in
        Hashtbl.iter (fun id term ->
                        let dist=  
                            monde.moi.strategie.distance monde monde.moi.suis_la term.la 
                          in
                          match term.sur with
                            |SOL -> if dist<= !dist_min_sol then begin dist_min_sol:=dist; pos_sol:=term.la end
                            |CHAMEAU id -> if dist<= !dist_min_chameau then begin dist_min_chameau:=dist; id_chameau:=id end
                     ) monde.victimes;
        if !pos_sol=(0,0) then
        begin
          if !id_chameau=(-1) then
            [],0
          else
          begin
           monde.moi.strategie.calcule_chargement<-(traque_mechant base_la_plus_proche);
           traque_mechant base_la_plus_proche monde        
          end
        (* on se dirige vers le lterm le plus proche*)
        end else monde.moi.strategie.trouve_chemin monde monde.moi.suis_la !pos_sol
    |t::q -> 
          let (pos,_) = List.fold_left (fun (pos_min,dist_min) new_pos -> 
               let new_dist=monde.moi.strategie.distance monde monde.moi.suis_la new_pos in 
                 if new_dist<=dist_min then (new_pos,new_dist)
                                       else (pos_min,dist_min)
                         ) ((0,0),1000000000) monde.bases
          in
            if pos=(0,0) then [],0
                      else monde.moi.strategie.trouve_chemin monde monde.moi.suis_la pos

(*pour s'approvisionner: calcule le rendement moyen des bases et des lterms et prend le meilleur*)
and meilleur_ratio monde=
  let (w,h)=Jouer.taille_carte monde.desert_hostile in
  let somme_ratio_bases=
    if monde.moi.strategie.nbr_bases=0 then 1000
    else (int_of_float (10.0*.monde.moi.strategie.somme_ratio_bases))/monde.moi.strategie.nbr_bases in
  let max_ratio=ref (-1)
  and dist_min_chameau = ref 1000000000
  and position = ref (0,0) in
  List.iter (fun pos -> let dist=monde.moi.strategie.distance monde monde.moi.suis_la pos +1 in
              let ratio=somme_ratio_bases/dist in
              if !max_ratio<ratio then begin max_ratio:=ratio; position:=pos end
            ) monde.bases;
  let termes=Hashtbl.create 300 in
  let add la taille dest=
    let dist =monde.moi.strategie.distance monde la dest +1 in
    try 
      let ratio=Hashtbl.find termes la in
        Hashtbl.replace termes la (ratio+.(float_of_int taille)/.(float_of_int (dist)));
    with |_ ->  Hashtbl.add termes la ((float_of_int taille)/.(float_of_int (dist)))
  in
    Hashtbl.iter (fun id term ->
      match term.taille,term.vers with
        |Some taille,Some dest when term.sur=SOL->
          add term.la taille dest
        |None,None when term.sur=SOL->
          add term.la (monde.moi.capacite/2) (w/2+1,h/2+1)
        | _,_ when term.sur=CHAMEAU id -> let dist=monde.moi.strategie.distance monde monde.moi.suis_la term.la in
           if dist< !dist_min_chameau then begin dist_min_chameau:=dist end
        |_ -> ()
    ) monde.victimes;
    Hashtbl.iter (fun pos ratio -> let ratio=(int_of_float (10.0*.ratio/.(float_of_int(monde.moi.strategie.distance monde monde.moi.suis_la pos +1)))) in if ratio > !max_ratio then begin max_ratio:=ratio; position:=pos end) termes;   
    if !max_ratio=(-1) then 
    begin
      if !dist_min_chameau=1000000000 then
        [],0
      else
      begin
        monde.moi.strategie.calcule_chargement<-(traque_mechant meilleur_ratio);
        traque_mechant meilleur_ratio monde        
      end
      (* on se dirige vers le coin le plus rentable*)
      end else monde.moi.strategie.trouve_chemin monde monde.moi.suis_la !position    
    
(**traque les méchants pour les pousser dans les trous**)
and traque_mechant restore_func monde=
  match monde.moi.strategie.cible with
    |None -> (*on cherche le chameau le plus proche*)
      let dist_min = ref 1000000000 in 
      let id_chameau = ref (-1) in
      Hashtbl.iter (fun id mechant -> 
            let dist = IA_sauve_chemin.distance_sable monde mechant.est_la monde.moi.suis_la in
            if dist < !dist_min then
            begin dist_min:=dist; id_chameau:=id end
                   ) monde.pas_moi;
      if !id_chameau= (-1) then 
      begin 
        monde.moi.strategie.calcule_chargement<-restore_func;
        base_la_plus_proche monde;
      end 
      else 
      begin
        monde.moi.strategie.cible <- Some !id_chameau;
        traque_mechant restore_func monde;
      end
    |Some id -> try
      let mechant = Hashtbl.find monde.pas_moi id in
      let rec extract_etape chemin longueur=
        match chemin with
          |[] -> []
          |t::q -> if longueur=0 then [] else t::(extract_etape q (longueur-1))
        in            
        let (chemin,longueur)=monde.moi.strategie.trouve_chemin monde monde.moi.suis_la mechant.est_la in
        let new_lg = longueur / (if longueur>=3 then 3 else 1) in
        (extract_etape chemin (new_lg),new_lg)
      with |_ -> (* le méchant est tombé dans un lambda terme*)
      begin
        monde.moi.strategie.cible<- None;
        traque_mechant restore_func monde;
      end
