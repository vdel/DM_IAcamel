(* Fichier main initial pour les élèves. À compléter...
 * À compiler avec 'make'
 *
 * puis './client -help'
 *)

open Types

(* Ligne de commande *)
let hote = ref "localhost"
let port = ref 1231
let log = ref ""
let anon_fun s =
    raise (Arg.Bad "Trop d'arguments!")
let usage_msg = "Client pour le DM2 de prog 2007"
let speclist = Arg.align [
  "-h",Arg.Set_string hote,
  " Hôte auquel se connecter (défaut: "^ !hote^")";
  "-p",Arg.Set_int port,
  " Numéro de port à utiliser (défaut: "^string_of_int !port^")";
  "-log",Arg.Set_string log,
  " Fichier log à utiliser (défaut: aucun)";
]

(*lit les paramètres de la ligne de commande*)
let lit_parametres ()=
{
  outchannel=if !log="" then None else Some (open_out !log);
} 
  
(* Procédure principale *)
let _ = 
  Arg.parse speclist anon_fun usage_msg ; (* récupérer l'hôte et le port *)
  let la_carte,
    (id_client,capacite,eau),
    positions =
      Jouer.init !hote !port
  in
  let strategie_IA=IA.set_strategie (Jouer.taille_carte la_carte) in
  let struct_moi={id_moi=id_client;capacite=capacite; suis_la=(List.assoc id_client positions);cerveau=capacite;bosse=eau;sac_a_bosse=[];a_mes_pieds=[];strategie=strategie_IA} in
  let nb_chameaux = List.length positions in 
  let struct_mechant=Hashtbl.create nb_chameaux in
    List.iter (fun (id,pos) ->
                 if id<>id_client then
                   let chameau_mechant = {id_mechant=id;est_la=pos;otages=[];comportement=
			{last_pos=pos; mise_optimale=max 2 ((eau-strategie_IA.max_tours)*strategie_IA.pourcent_mise_opt_initiale/100);mise_ultime=1}}
                   in Hashtbl.add struct_mechant id chameau_mechant
                 else ()
              ) positions;
  let trouve_bases carte=
    let w,h = Jouer.taille_carte carte in
    let bases_list=ref [] in
    for i=1 to w do
      for j=1 to h do
        if(Jouer.case_carte carte (i,j))=Base then
          bases_list:=(i,j):: !bases_list
      done
    done;
    !bases_list
  in
  let monde={desert_hostile=la_carte; bases=(trouve_bases la_carte); moi=struct_moi; pas_moi=struct_mechant; victimes=Hashtbl.create (Array.length la_carte); nbr_tours=0; nbr_combats=1(*surtout pas 0 !! cf Common.ajuste_mise_win*);params=lit_parametres (); termes_bannis=Hashtbl.create 100} in  
 (*pour gérer les lterms qu'on ne peut pas sauver, on modifie la carte en mettant des palmier aux endroits inatteignables*)
 let w,h = Jouer.taille_carte la_carte in
 let t=Array.make_matrix w h false in
   let bords = ref [monde.moi.suis_la] in
     while !bords<>[] do
       let new_bords = ref [] in
       List.iter (fun ((x,y) as pos) -> 
         if 1<=x && 1<=y && x<=w && y<=h then
         if t.(x-1).(y-1)=false then
           match Jouer.case_carte monde.desert_hostile pos with
             |Base |Sable -> t.(x-1).(y-1)<-true;
               new_bords:= (x-1,y)::(x+1,y)::(x,y-1)::(x,y+1):: !new_bords
             |Palmier | Omega -> t.(x-1).(y-1)<-true;
       ) !bords;
       bords:= !new_bords
     done;
   for i=0 to w-1 do
     for j=0 to h-1 do
       if not t.(i).(j) then Jouer.change_case_carte monde.desert_hostile (i+1,j+1) Palmier
     done
   done;
 (*toutes les bases du monde ne sont pas forcément accessibles, de même que tous les méchants: on supprime ceux qui ne le sont pas*)
 monde.bases <- List.filter (fun (x,y) -> t.(x-1).(y-1)) monde.bases;
 Hashtbl.iter (fun id mechant -> let (x,y)=mechant.est_la in
   if not t.(x-1).(y-1) then
     Hashtbl.remove monde.pas_moi id
 ) monde.pas_moi;
 (*pour mettre a jour la position en fonction de la direction qu'a pris le chameau *)
 let update_pos p d = let (a,b)=p in
  match d with
  |Nord  -> (a, b-1)	
  |Est   -> (a+1, b)
  |Ouest -> (a-1, b)
  |Sud   -> (a, b+1) in
  Common.output_log monde ("Mon id: "^(string_of_int monde.moi.id_moi)^"\n");
  try
    (* Boucle infernale, c'est ici qu'il faut intervenir si on veut
     * que le chameau fasse des choses intelligentes (ou stupides) *) 
    (*    Unix.set_nonblock Unix.stdin ;*)
    while true do
      Common.output_log monde ("Tour "^(string_of_int monde.nbr_tours)^", eau:"^(string_of_int monde.moi.bosse)^"\n");
      let pari,ordre= IA.joue monde in
      if pari<>1 then monde.nbr_combats<-monde.nbr_combats+1;
      monde.moi.bosse <- monde.moi.bosse-pari;
      let actions, termes = Jouer.joue_un_tour pari ordre in
      let mouvement_de_notre_chameau=ref None in   (*pour éviter les blocages*)
      (* on met à jour le monde *)
      (* on met à jour l'ancienne position de chaque chameau *)
      Hashtbl.iter (fun _ mechant -> mechant.comportement.last_pos<-mechant.est_la) monde.pas_moi;
      let rec process_actions actions chameaux_actifs=
        match actions with
          |[]-> Hashtbl.iter 
                (fun id -> 
                  (fun mechant ->  
                     if not(List.mem id chameaux_actifs) then (*si le chameau n'a pas joué: il est mort, on le supprime*)
                       begin
                         Hashtbl.remove monde.pas_moi id; (* on supprime le chameau *)
                         List.iter (fun id-> Hashtbl.remove monde.victimes id) mechant.otages (*les lambda termes portés par un chameau mort meurent*)
                       end
                  )
                ) monde.pas_moi 
          |(id,actions_list)::q ->
     try
     if id=monde.moi.id_moi then () else begin let _ = Hashtbl.find monde.pas_moi id in () end;
     begin     
     (*vérifie que l'action effectuée par notre chameau est bien celle attendue*)
       match monde.moi.strategie.ajuste_mise with
         |None -> ()
         |Some (mise,action,mechant_list,ultime) -> 
           if id=monde.moi.id_moi then 
           begin
             begin
             match actions_list with
               |[] -> List.iter (fun mechant -> Common.ajuste_mise_loose monde mechant mise ultime) mechant_list
               |t::_ -> if t<>action then List.iter (fun mechant ->Common.ajuste_mise_loose monde mechant mise ultime) mechant_list
                                     else List.iter (fun mechant ->Common.ajuste_mise_win monde mechant mise ultime) mechant_list
             end;
             monde.moi.strategie.ajuste_mise<-None
           end;  
     end;
     let rec process_actions_list al=
       match al with
        |[] -> ()
        |(Abouge(dir))::q -> (* un chameau a bougé *)
          begin
            if id = monde.moi.id_moi then (* si c'est moi *)
            begin
              begin
                match dir,monde.moi.strategie.last_direction with
                  |Nord,Some Sud |Sud,Some Nord |Ouest,Some Est |Est,Some Ouest -> 
                    monde.moi.strategie.nbr_aller_retour<-monde.moi.strategie.nbr_aller_retour+1
                  |_ -> monde.moi.strategie.nbr_aller_retour<- 0
              end;
              mouvement_de_notre_chameau:=Some dir;
              monde.moi.strategie.last_direction<-Some dir;
              monde.moi.suis_la <- update_pos monde.moi.suis_la dir;  (*je met à jour ma position*)
              if(Jouer.case_carte monde.desert_hostile monde.moi.suis_la)=Base then
              begin
                Jouer.change_case_carte monde.desert_hostile monde.moi.suis_la Sable;
                monde.bases <- List.filter (fun pos_base-> not (monde.moi.suis_la=pos_base)) monde.bases;
                monde.moi.strategie.somme_ratio_bases<-monde.moi.strategie.somme_ratio_bases+.(List.fold_left (fun s (_,dest,taille) -> s+.((float_of_int taille)/.(float_of_int (monde.moi.strategie.distance monde monde.moi.suis_la dest)))) 0.0 termes);
                monde.moi.strategie.nbr_bases<-monde.moi.strategie.nbr_bases+1
              end;
              List.iter (fun id_lterme ->                             (*je met à jour la position de tous les ltermes que je porte*)
                           let lt = Hashtbl.find monde.victimes id_lterme in
                             lt.la <- monde.moi.suis_la;
                        ) monde.moi.sac_a_bosse;  
            end
            else
            begin
              try (*try: on ne vas pas forcément trouver le méchant dans la table de hachage
                    par exemple s'il est poussé dans un omega, on va le supprimer et s'il a perdu un lterm on 
                    va le chercher alors qu'il y est pas...*)
                let mcht = (Hashtbl.find monde.pas_moi id) in (* si c'est un ennemi *)
                  mcht.est_la <- update_pos mcht.est_la dir;  (* je met à jour sa position *)
                  match Jouer.case_carte monde.desert_hostile mcht.est_la with
                  | Omega -> 
                           Hashtbl.remove monde.pas_moi id;
                           List.iter (fun id-> Hashtbl.remove monde.victimes id) mcht.otages (*les lambda termes portés par un chameau mort meurent*)
                  | _ ->
                    List.iter (fun id_lterme ->                 (* je met à jour la position des ltermes qu'il porte *)
                                 let lt = Hashtbl.find monde.victimes id_lterme in
                                   lt.la <- mcht.est_la;
                              ) mcht.otages;  
              with |_ -> () (* s'il est mouru on a rien a faire *)
            end;          
            process_actions_list q (*et on continue*)
          end
        |(Apris(id_list))::q-> (* quelqu'un a ramassé des ltermes *)
          begin
            let id_list = List.filter (fun id -> not (Hashtbl.mem monde.termes_bannis id)) id_list in
            let pos=
              if id = monde.moi.id_moi then (* si c'est moi *)
                begin
                  monde.moi.sac_a_bosse <- monde.moi.sac_a_bosse@id_list;  (*je les retiens pour les sauver *)
                  List.iter (fun id_lterme ->                              (*je met à jour ma mémoire    *)
                               let lt = Hashtbl.find monde.victimes id_lterme in
                                 match lt.taille with
                                   |None->()
                                   |Some sz->monde.moi.cerveau <- (monde.moi.cerveau-sz)
                            ) id_list;
                  monde.moi.suis_la
                end
              else
                begin
                  let mcht = (Hashtbl.find monde.pas_moi id) in (*si c'est un méchant*)                    
                    mcht.otages <- mcht.otages@id_list;         (*je me souviens qu'il porte ces ltermes*)
                    mcht.est_la
                end;
            in
              List.iter (fun id_lterme -> (*je retiens sur qui sont les ltermes et si je ne les avais jamais vus je les rajoute au monde*)
                           try 
                             let lt = Hashtbl.find monde.victimes id_lterme in
                             lt.sur <- CHAMEAU id;
                           with Not_found ->
                             Hashtbl.add monde.victimes id_lterme {taille=None;vers=None;la=pos;sur=CHAMEAU id}
                        ) id_list;         
            process_actions_list q (*et on continue*)
          end
        |(Apose(id_list))::q-> 
          begin
            if id = monde.moi.id_moi then 
              begin
                (*j'oublie les lterm *)
                monde.moi.sac_a_bosse <- List.filter (fun id_lterme -> not (List.mem id_lterme id_list)) monde.moi.sac_a_bosse; 
                List.iter (fun id_lterme -> (*du coup, j'ai plein de place dans mon cerveau*)
                             let lt = Hashtbl.find monde.victimes id_lterme in
                               match lt.taille with
                                 |None -> ()
                                 |Some sz -> monde.moi.cerveau <- (monde.moi.cerveau+sz)
                          ) id_list;
              end
            else
              begin
                let mcht = (Hashtbl.find monde.pas_moi id) in (*le mechant a posé des lterms: on s'en souvient*)
                  mcht.otages <- List.filter (fun id_lterme -> not (List.mem id_lterme id_list)) mcht.otages;
              end;         
            List.iter (fun id_lterme -> (*si les lterm sont sauvé, on les efface, sinon on met à jour leur position*)
                         let lt = Hashtbl.find monde.victimes id_lterme in
                           match lt.vers with
                             |None -> 
                               lt.sur<-SOL; (*on sait pas où il doit aller, dans le doute, on l'efface pas*)
                             |Some p -> 
                                if p = lt.la then  (*il doit aller la ou il est: il est sauvé: on l'efface*)
                                  Hashtbl.remove monde.victimes id_lterme
                                else
                                lt.sur<-SOL
                      ) id_list;
            process_actions_list q (*on continue*)
          end
        |(Apparait(pos))::q -> (* un rayon cosmique a touché le serveur*)
          print_string "Serveur débile: il a envoyé une action Apparait"; process_actions_list q
    in
      process_actions_list actions_list; (*on lit les actions du chameau courant*)
      process_actions q (id::chameaux_actifs)  (*on passe au chameau suivant*)
   with
   | _ -> process_actions q chameaux_actifs 
  in
    process_actions actions [];
    (*anti-blocage dans le cas où on bouge jamais*)
    if !mouvement_de_notre_chameau=None then
    begin
      if monde.moi.strategie.last_direction=None then
        monde.moi.strategie.nbr_aller_retour<-monde.moi.strategie.nbr_aller_retour+1;
      monde.moi.strategie.last_direction<-None 
    end;
    (*s'occupe des lambda_termes au pieds de notre chameau*)
    monde.moi.a_mes_pieds <- [];
    List.iter (fun (id,dest,sz) -> 
                 if (Jouer.case_carte monde.desert_hostile dest)<>Palmier then
                 begin
                   monde.moi.a_mes_pieds <- id::monde.moi.a_mes_pieds;  (*on met à jour la liste des ltermes à nos pieds*)
                   try 
                     let lt = Hashtbl.find monde.victimes id in (*on met à jour les info concernant les ltermes*)
                       lt.taille <- Some sz;
                       lt.vers <- Some dest;
                   with 
                     | Not_found -> (*on les ajoute si on les connaissait pas*)
                       let term={taille=Some sz;vers=Some dest;la=monde.moi.suis_la;sur=SOL} in
                       Hashtbl.add monde.victimes id term
                 end
                 else
                 begin
                   Hashtbl.add monde.termes_bannis id ();
                   if Hashtbl.mem monde.victimes id then Hashtbl.remove monde.victimes id
                 end
              ) termes;
   (*on veut enlever du monde les lambda_termes qui devraient être à nos pieds mais on été sauvés donc ne sont plus là*)
   let doit_etre_la = ref [] in (*on commence par faire la liste de ceux qui devraient être à nos pieds*)
     Hashtbl.iter (fun id -> (fun lt -> if lt.la=monde.moi.suis_la && lt.sur=SOL then doit_etre_la:= id:: !doit_etre_la)) monde.victimes;
   let sont_morts = List.filter (fun id-> not (List.mem id monde.moi.a_mes_pieds)) !doit_etre_la in
     List.iter (fun id-> Hashtbl.remove monde.victimes id) sont_morts; (*on retire ceux qui devraient être la et qui n'y sont pas*)
     monde.nbr_tours<- monde.nbr_tours+1;             
  done;    
  with e -> (* On rattrape tout ce qui passe par là *)
    prerr_endline ("Exception lancée: "^ Printexc.to_string e);
    Jouer.stop () ;
    exit 1

