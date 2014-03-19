open Types
 
(* renvoit les lterm qu'il faut ramasser*)
(*ramasse les ltemres dans l'ordre dans lequel ils se présentent*)
let rec ramasse_stupide monde=
  match monde.moi.a_mes_pieds with
    |[] -> []
    |l -> 
      begin 
        let capacite_cerebrale_restante = ref monde.moi.cerveau in
          let rec ramasse_rec lterm_list=
            match lterm_list with
              |[] -> []
              |id::q ->
                begin 
                  let term = Hashtbl.find monde.victimes id in
                    match term.taille with
                      |None -> print_string "Bug dans ramasse_vite: on devrait connaitre la taille du lambda terme"; ramasse_rec q
                      |Some taille -> 
                        if taille<= !capacite_cerebrale_restante then
                          begin
                            capacite_cerebrale_restante:= !capacite_cerebrale_restante-taille;
                            id::(ramasse_rec q)
                          end
                        else
                          ramasse_rec q
                end
          in
            ramasse_rec l
      end

(*ramasse d'abord les plus gros ltermes*)
let rec ramasse_moins_stupide monde=
  match monde.moi.a_mes_pieds with
    |[] -> []
    |l -> 
      begin   
  let par_terre = List.map (fun id -> let term= Hashtbl.find monde.victimes id in (id,term.taille)) monde.moi.a_mes_pieds in
  let par_terre_decroissant = List.fast_sort (fun (_,a) (_,b) -> 
                                                match a,b with
                                                  |None,_ -> 1
                                                  |_,None -> -1
                                                  |Some x,Some y -> y-x
                                             ) par_terre
  in
    let capacite_cerebrale_restante = ref monde.moi.cerveau in
      let rec ramasse_rec lterm_list=
            match lterm_list with
              |[] -> []
              |(id,taille_opt)::q ->
                begin 
                  match taille_opt with
                    |None -> print_string "Bug dans ramasse_vite: on devrait connaitre la taille du lambda terme"; ramasse_rec q
                    |Some taille -> 
                      if taille<= !capacite_cerebrale_restante then
                        begin
                          capacite_cerebrale_restante:= !capacite_cerebrale_restante-taille;
                          id::(ramasse_rec q)
                        end
                      else
                        ramasse_rec q
                end
          in
            ramasse_rec par_terre_decroissant
      end

(*ramasse d'abord les ltermes qui ont le plus grand ratio valeur/distance*)
let rec ramasse_ratio monde=
  match monde.moi.a_mes_pieds with
    |[] -> []
    |l -> 
      begin   
  let par_terre = List.map (fun id ->
    let term= Hashtbl.find monde.victimes id in
      match term.taille,term.vers with
        |None,_ |_,None -> print_string "Bug dans ramasse_vite: on devrait connaitre la taille du lambda terme"; (id,0,0.0)
        |Some taille,Some dest -> let ratio= (float_of_int taille)/.(float_of_int (monde.moi.strategie.distance monde monde.moi.suis_la dest))
          in (id,taille,ratio)
                           ) monde.moi.a_mes_pieds in
  let par_terre_decroissant = List.fast_sort (fun (_,_,a) (_,_,b) -> 
                                               if b>a then 1 else -1
                                             ) par_terre
  in 
    let capacite_cerebrale_restante = ref monde.moi.cerveau in
      let rec ramasse_rec lterm_list=
            match lterm_list with
              |[] -> []
              |(id,taille,_)::q ->
                if taille<= !capacite_cerebrale_restante then
                  begin
                    capacite_cerebrale_restante:= !capacite_cerebrale_restante-taille;
                    id::(ramasse_rec q)
                  end
                else
                  ramasse_rec q
          in
            ramasse_rec par_terre_decroissant
      end

(*Regarde les ltermes par terre et les classe avec le ratio comme avant mais la distance est donnée par rapport à la position en argument
 *sépare les ltermes en plus grand ou plus petit qu'un certain seuil, utile pour les 2 fonctions suivantes*) 
let par_terre_separe monde pos = 
  let par_terre = List.map (fun id ->
    let term= Hashtbl.find monde.victimes id in
      match term.taille,term.vers with
        |None,_ |_,None -> print_string "Bug dans ramasse_vite: on devrait connaitre la taille du lambda terme"; (id,0,0.0,monde.moi.suis_la)
        |Some taille,Some dest -> 
          let ratio= (float_of_int taille)/.(float_of_int (monde.moi.strategie.distance monde pos dest))
          in (id,taille,ratio,dest)
                           ) monde.moi.a_mes_pieds in 
  let (par_terre_grand,par_terre_petit)= (* ltermes en dessous / au dessus du seuil *)
    List.partition (fun (_,taille,_,_) -> taille>=monde.moi.capacite*monde.moi.strategie.pourcent_taille_min/100) par_terre in
  let par_terre_grand_decroissant = List.fast_sort (fun (_,_,a,_) (_,_,b,_) -> 
                                                     if b>a then 1 else -1
                                                   ) par_terre_grand
  in
  let par_terre_petit_decroissant = List.fast_sort (fun (_,_,a,_) (_,_,b,_) -> 
                                                     if b>a then 1 else -1
                                                   ) par_terre_petit 
  in par_terre_petit_decroissant, par_terre_grand_decroissant
  
(*ramasse d'abord les ltermes qui ont le plus grand ratio valeur/distance et dont la taille est plus grande qu'un certain pourcentage de la capacité totale*)
let rec ramasse_ratio_borne_inf monde= 
  if monde.moi.strategie.cible<>None then [] else
  begin
  let petit, grand = par_terre_separe monde monde.moi.suis_la
  in
    let capacite_cerebrale_restante = ref monde.moi.cerveau in
      let rec ramasse_rec lterm_list=
            match lterm_list with
              |[] -> []
              |(id,taille,_,_)::q ->
                if taille<= !capacite_cerebrale_restante then
                  begin
                    capacite_cerebrale_restante:= !capacite_cerebrale_restante-taille;
                    id::(ramasse_rec q)
                  end
                else
                  ramasse_rec q
          in
            let res=ramasse_rec grand in
            if res=[] && monde.moi.sac_a_bosse=[] then ramasse_rec petit
                      else res
  end

(*ramasse d'abord le lterme qui a le plus grand ratio valeur/distance et dont la taille est plus grande qu'un certain pourcentage de la capacité 
 *totale puis recommence mais cette fois la distance est celle prise à partir du premier lterme *)
 (* A DEBUGGER !!! *)
  
 let ramasse_ratio_borne_inf_mieux monde= 
  if monde.moi.strategie.cible<>None then [] else
  let capacite_cerebrale_restante = ref monde.moi.cerveau in

  let rec aux monde pos = 
  match monde.moi.a_mes_pieds with    
    |[] -> [] (* y'a rien à prendre *)
    |l -> 
    let petit, grand = par_terre_separe monde pos in
    let rec ramasse_rec lterm_list=
            match lterm_list with
              |[] -> 0,(0,0)
              |(id,taille,ratio,dest)::q ->
                if taille<= !capacite_cerebrale_restante && ratio>0.0 then
                  begin
                    capacite_cerebrale_restante:= !capacite_cerebrale_restante-taille;
                    monde.moi.a_mes_pieds<- List.filter (fun id_cham -> not(id_cham=id)) monde.moi.a_mes_pieds; (* je l'ai pris, il n'est plus la *)
                    id,dest
                  end
                else
                  ramasse_rec q
     in
     let id, dest =ramasse_rec grand in
      if dest=(0,0) then
        if monde.moi.sac_a_bosse=[] then 
        begin
          let a,b = (ramasse_rec petit) in (* si pas de grands, je regarde dans les ptits *) 
            if b=(0,0) then [] (* si rien non plus alors jprends rien *)
            else a::(aux monde b)
        end
        else []
      else id::(aux monde dest)
      
   in (aux monde monde.moi.suis_la)
     
