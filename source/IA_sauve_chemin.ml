open Types

(********** les notions de distance **********)
(* on utilise une notion de "proche", abstraction de la distance *)
let rec distance_sable monde p1 p2 = let (x1,y1)=p1 and (x2,y2)=p2 in 
  abs(x1-x2)+abs(y1-y2)  (* distance si que du sable... *)
  
(* prend en compte le terrain ie prendre la distance donnée par Dijkstra: plus lent *)
and distance_dijkstra monde p1 p2  = 
  let (_,d) = chemin_dijkstra monde p1 p2 in d

(* prend en compte le terrain ie prendre la distance donnée par A-star: plus lent *)
and distance_A_star monde p1 p2  = 
  let (_,d) = chemin_A_star monde p1 p2 in d

(************ Dijkstra ************)

(* algorithme de Dijkstra du wiki... un peu modifié *)
and dijkstra (adj: 'b -> ('b * int) list) tbl_done waiting_and_done tas_fibo (v1:'b) (v2:'b) =
  let rec loop h =
    if Tas_fibo.is_empty h then raise Not_found;
    let ((v,pred,_),w),h = Tas_fibo.extract_min h in 
    let h = 
      Hashtbl.add tbl_done v pred;
      List.fold_left (fun h (e,d) -> if not (Hashtbl.mem waiting_and_done e) then begin Hashtbl.add waiting_and_done e 0; Tas_fibo.add ((e,v,0), w+d) h end else h) h (adj v)
    in
      if v = v2 then
        h
      else 
        loop h
  in
  loop tas_fibo

(* algorithme de Dijkstra du wiki... un peu modifié *)
(*and dijkstra (adj: 'b -> ('b * int) list) tbl_done waiting_and_done tas_fibo (v1:'b) (v2:'b) =
  Graphics.open_graph "";
  let rec loop h =
    if Tas_fibo.is_empty h then raise Not_found;
    let ((v,pred,_),w),h = Tas_fibo.extract_min h in 
    let h = 
      Hashtbl.add tbl_done v pred;

      let (x,y) = v in
      Graphics.set_color Graphics.green;
      Graphics.fill_rect (3*(x-1)) (3*(y-1)) 2 2;
      Graphics.set_color Graphics.red;

      let h = List.fold_left (fun h (e,d) -> if not (Hashtbl.mem waiting_and_done e) then begin Hashtbl.add waiting_and_done e 0; let (x0,y0)= e in Graphics.fill_rect (3*(x0-1)) (3*(y0-1)) 2 2; Printf.printf "Pos=(%d,%d) (%d,%d) P=%d\n" x y x0 y0 d; Tas_fibo.add ((e,v,0), w+d) h end else h) h (adj v) in

          let _ = Graphics.wait_next_event [Graphics.Key_pressed] in 
          Graphics.set_color Graphics.blue;
          Graphics.fill_rect (3*(x-1)) (3*(y-1)) 2 2 ;
          h
    in
      if v = v2 then
      begin
        Graphics.close_graph ();
        h
      end
      else 
        loop h
  in
  loop tas_fibo*)

(************ A-star ************)
and a_star (adj: 'b -> ('b * int) list) dist tbl_done tbl_waiting tas_fibo (v1:'b) (v2:'b)=
  let add_neighbours h v real_w =
    List.fold_left (fun h (e,d) ->
      let new_real_w=real_w+d in 
        try                 
          let real_w_e=Hashtbl.find tbl_waiting e in
            if new_real_w<real_w_e then
            begin
              Hashtbl.replace tbl_waiting e new_real_w;
              Tas_fibo.add ((e,v,new_real_w), dist e v2) h
            end
          else
            h
        with
          |_-> Hashtbl.add tbl_waiting e new_real_w;
               Tas_fibo.add ((e,v,new_real_w), dist e v2) h
                   ) h (adj v)
  in
  let rec loop h =
    if Tas_fibo.is_empty h then raise Not_found;
    let ((v,pred,real_w),_),h = Tas_fibo.extract_min h in
    let h =
      let min_w = Hashtbl.find tbl_waiting v in
        if min_w<real_w then h
        else
          begin
            Hashtbl.replace tbl_done v pred;
            add_neighbours h v real_w
          end
    in
      if v = v2 then
        h
      else 
        loop h
  in
  loop tas_fibo
(*and a_star (adj: 'b -> ('b * int) list) dist tbl_done tbl_waiting tas_fibo (v1:'b) (v2:'b)=
  let add_neighbours h v real_w =
    List.fold_left (fun h (e,d) -> let (x0,y0)= e in Graphics.fill_rect (3*(x0-1)) (3*(y0-1)) 2 2;
      let new_real_w=real_w+d in 
        try                 
          let real_w_e=Hashtbl.find tbl_waiting e in
            if new_real_w<real_w_e then
            begin
              Hashtbl.replace tbl_waiting e new_real_w;
              Tas_fibo.add ((e,v,new_real_w), dist e v2) h
            end
          else
            h
        with
          |_-> Hashtbl.add tbl_waiting e new_real_w;
               Tas_fibo.add ((e,v,new_real_w), dist e v2) h
                   ) h (adj v)
  in
  let rec loop h =
    if Tas_fibo.is_empty h then raise Not_found;
    let ((v,pred,real_w),_),h = Tas_fibo.extract_min h in
    let h =
      let min_w = Hashtbl.find tbl_waiting v in

      let (x,y) = v in
      Graphics.set_color Graphics.green;
      Graphics.fill_rect (3*(x-1)) (3*(y-1)) 2 2;
      Graphics.set_color Graphics.red;
      let h=
        if min_w<real_w then h
        else
          begin
            Hashtbl.replace tbl_done v pred;
            add_neighbours h v real_w
          end
in let (x,y)=v in
let _ = Graphics.wait_next_event [Graphics.Key_pressed] in 
        Graphics.set_color Graphics.blue;
        Graphics.fill_rect (3*(x-1)) (3*(y-1)) 2 2 ;
h
    in
      if v = v2 then
        h
      else 
        loop h
  in Graphics.open_graph "";
  let res = loop tas_fibo in Graphics.close_graph (); res*)
   
(************* Les chameaux 2: le retour *************)
(*renvoit la liste des voisins d'une case*)
and adjacents monde (x,y)=
  let adj=[((x,y-1),1);((x-1,y),1);((x,y+1),1);((x+1,y),1)] in
  List.filter
    (fun (pos,_) ->
      try
        match Jouer.case_carte monde.desert_hostile pos with
          |Sable |Base -> true
          |_ -> false
      with
        |_-> false
    ) adj

(*remonte la liste des prédécesseurs pour former le chemin*)
and find_path hashtbl pdeb pfin=
  let rec find_path_rec hashtbl pfin acc_path acc_longueur=
    let pred=Hashtbl.find hashtbl pfin in
      if pred=pdeb then
        (pfin::acc_path,1+acc_longueur)
      else find_path_rec hashtbl pred (pfin::acc_path) (1+acc_longueur)
  in
    if pdeb=pfin then ([],0)
    else
      find_path_rec hashtbl pfin [] 0
(*calcule le chemin de p1 à p2 par l'algo alg (=dijkstra ou A-star)*)
and chemin monde alg p1 p2 refresh=
  if p1=monde.moi.suis_la && not refresh then
  begin
    if not (monde.moi.strategie.last_moi_pos=monde.moi.suis_la) then
    begin
      Hashtbl.clear monde.moi.strategie.pred_dij_depuis_moi;
      Hashtbl.clear monde.moi.strategie.waiting_depuis_moi;
      Hashtbl.add monde.moi.strategie.waiting_depuis_moi p1 (distance_sable monde p1 p2);
      monde.moi.strategie.last_moi_pos<-monde.moi.suis_la;
      monde.moi.strategie.restant_depuis_moi<-
           alg monde.moi.strategie.pred_dij_depuis_moi monde.moi.strategie.waiting_depuis_moi (Tas_fibo.new_t (p1,(0,0),0) (distance_sable monde p1 p2)) p1 p2;
      find_path monde.moi.strategie.pred_dij_depuis_moi p1 p2
    end
    else
    begin
      if not (Hashtbl.mem monde.moi.strategie.pred_dij_depuis_moi p2) then
        monde.moi.strategie.restant_depuis_moi<-
   alg monde.moi.strategie.pred_dij_depuis_moi monde.moi.strategie.waiting_depuis_moi monde.moi.strategie.restant_depuis_moi p1 p2;
      find_path monde.moi.strategie.pred_dij_depuis_moi p1 p2
    end
  end
  else
  begin
    Hashtbl.clear monde.moi.strategie.pred_dij_autre;
    Hashtbl.clear monde.moi.strategie.waiting_autre;
    Hashtbl.add monde.moi.strategie.waiting_autre p1 (distance_sable monde p1 p2);
    let _ = alg monde.moi.strategie.pred_dij_autre monde.moi.strategie.waiting_autre (Tas_fibo.new_t (p1,(0,0),0) (distance_sable monde p1 p2)) p1 p2 in
    find_path monde.moi.strategie.pred_dij_autre p1 p2
  end
  
  
(*************** les fonction de chemin *****************)  
(* le plus court chemin entre les deux cases données a l'aide de dijkstra *)
and chemin_dijkstra monde p1 p2 =
  chemin monde (dijkstra (adjacents monde)) p1 p2 false
(* un chemin court entre les deux cases données a l'aide de A-star *) 
and chemin_A_star monde p1 p2 =
  chemin monde (a_star (adjacents monde) (distance_sable monde)) p1 p2 true

(* algo stupide puisqu'il parait qu'il en faut un : va vers la base en contournant les obstacles *) 
and chemin_pas_si_naif monde ((x1,y1) as p1) (x2,y2) =
 let l = ref [] in
  let contourne (x,y) q = 
   match Jouer.case_carte monde.desert_hostile q with
    |Sable |Base -> l:= q::(!l); q
    |Palmier |Omega ->      if Common.depl_possib monde Nord  then begin l:=(x,y-1)::(!l); (x,y-1) end
                       else if Common.depl_possib monde Est   then begin l:=(x+1,y)::(!l); (x+1,y) end
                       else if Common.depl_possib monde Sud   then begin l:=(x,y+1)::(!l); (x,y+1) end
                       else if Common.depl_possib monde Ouest then begin l:=(x-1,y)::(!l); (x-1,y) end
                       else failwith "chameau bloqué... mais c'est impossible!" in
  let rec cons_chemin ((x,y) as p) = 
   match (x-x2),(y-y2) with 
   |(a,b) when a>0 && b>0 -> let p3 = contourne p (x-1,y) in cons_chemin p3
   |(a,b) when a>0 && b<0 -> let p3 = contourne p (x-1,y) in cons_chemin p3
   |(a,b) when a<0 && b>0 -> let p3 = contourne p (x+1,y) in cons_chemin p3
   |(a,b) when a<0 && b<0 -> let p3 = contourne p (x+1,y) in cons_chemin p3
   |(a,b) when a>0 && b=0 -> let p3 = contourne p (x-1,y) in cons_chemin p3
   |(a,b) when a<0 && b=0 -> let p3 = contourne p (x+1,y) in cons_chemin p3
   |(a,b) when a=0 && b>0 -> let p3 = contourne p (x,y-1) in cons_chemin p3
   |(a,b) when a=0 && b<0 -> let p3 = contourne p (x,y+1) in cons_chemin p3
   |_ (*a=0 et b=0*) -> () in 
  cons_chemin p1;
  (List.rev !l,List.length !l)

(********** modifie un chemin pour passer par les ltermes qui trainent pas trop loin (=situés à moins de largeur_couloir) **********)
and ameliore_chemin_stupide monde chemin largeur_couloir=chemin

and ameliore_chemin_intelligent monde chemin largeur_couloir=
  match chemin with
    |[] -> [] (* si le chemin est initialement vide, on renvoit la liste vide*)
    |t::q -> begin
  let deb_chemin = monde.moi.suis_la in
  let fin_chemin = List.fold_left (fun anc_pos new_pos-> new_pos) t q in
  let w,h = Jouer.taille_carte monde.desert_hostile in
  let t=Array.make_matrix (w+1) (h+1) 0 in
  let rec marque cases c= 
    if c=0 then ()
    else
    begin
      let nouv_cases=ref [] in
        let ajoute x y=          
          try
          match Jouer.case_carte monde.desert_hostile (x,y) with
            |Omega |Palmier -> ()
            |Sable |Base ->
            if t.(x).(y)=1 then ()
            else
            begin
              t.(x).(y)<- 1;
              nouv_cases:= (x,y):: !nouv_cases
            end
          with |_ -> ()
        in
          List.iter (fun (x,y) ->
                      ajoute (x-1) y;
                      ajoute (x+1) y;
                      ajoute x (y-1);
                      ajoute x (y+1);
                    ) cases;
          marque !nouv_cases (c-1)
    end
  in
    marque chemin largeur_couloir;    
    let lterm_proches_all=ref[] in
    Hashtbl.iter (fun _ term -> let (x,y)=term.la in if t.(x).(y)=1 then lterm_proches_all:= term::!lterm_proches_all) monde.victimes; 
    let lterm_proches = List.filter (fun x -> lterm_interessant monde x && not (x.la=deb_chemin || x.la=fin_chemin)) !lterm_proches_all
    and bases_proches = List.filter (fun (x,y) -> (t.(x).(y)=1 && not ((x,y)=deb_chemin || (x,y)=fin_chemin))) monde.bases
    in
    let lterm = tri_fusion (fun (x,_) (y,_) -> x<y) 
                    (List.map 
                       (fun term -> (monde.moi.strategie.distance monde monde.moi.suis_la term.la,term.la))
                       lterm_proches
                    ) 
    and bases = tri_fusion (fun (x,_) (y,_) -> x<y) 
                    (List.map 
                       (fun pos -> (monde.moi.strategie.distance monde monde.moi.suis_la pos,pos))
                       bases_proches
                    )
    in
      let positions_clefs = fusion (fun (x,_) (y,_) -> x<y) lterm bases in
        let rec nouveau_chemin pos_clefs pos=
          match pos_clefs with
            |[] -> (*on raccorde la dernière position clef à la destination initiale du chemin*)
              let (chemin,_)=monde.moi.strategie.trouve_chemin monde pos fin_chemin
              in chemin
            |(_,next_pos)::q -> (let (chemin,_)=monde.moi.strategie.trouve_chemin monde pos next_pos in chemin)@(nouveau_chemin q next_pos)
        in
  if positions_clefs=[] then chemin
                        else nouveau_chemin positions_clefs monde.moi.suis_la     
  end
(********** renvoit vrai si c'est la peine de faire un détour pour inspecter ce lterme **********)
and lterm_interessant monde lterme=
  match lterme.taille with
    |None -> true
    |Some taille -> taille<=monde.moi.cerveau && taille>=monde.moi.capacite*monde.moi.strategie.pourcent_taille_min/100

(********** TRI FUSION DE DEUX LISTES **********)
(*éclate une liste l en deux listes*)
and eclate l=
  let rec eclate_rec l l1 l2=
    match l with
      |[] -> (l1,l2)
      |t::q -> eclate_rec q l2 (t::l1)
  in
    eclate_rec l [] []
(*fusion de deux listes triées en une liste triée, supprime les éléments identiques*)
and fusion strictement_inf l1 l2=
  match l1,l2 with
    |[],_ -> l2
    |_,[] -> l1
    |t1::q1,t2::q2 -> if strictement_inf t1 t2 then t1::(fusion strictement_inf q1 l2)
                 else if strictement_inf t2 t1 then t2::(fusion strictement_inf l1 q2)
                 else fusion strictement_inf q1 q2
(*tri fusion avec un ordre < passé en argument*) 
and tri_fusion strictement_inf l=
  match l with
    | [] | [_] -> l
    | _ ->
      let (l1,l2) = eclate l in
      let lg=tri_fusion strictement_inf l1
      and ld=tri_fusion strictement_inf l2 in
        fusion strictement_inf lg ld
