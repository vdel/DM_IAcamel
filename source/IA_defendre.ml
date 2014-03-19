open Types
open Common

type terrain_danger = |S |O |C (* sans danger, oméga, chameau *)
type danger = DANGER | LONGUE_VIE (* évite des calculs inutiles *)

(* méchants dans un certain périmètre modifiable *)
let mechants_du_coin monde =
 let res = ref [] in 
 Hashtbl.iter (fun id mcht -> let (x1,y1)= mcht.est_la and (x,y)= monde.moi.suis_la in 
                               if (abs(x-x1)<3)&&(abs(y-y1)<3)&&(id<>monde.moi.id_moi) then 
                               res := mcht::(!res) ) 
               monde.pas_moi;
 !res
 
let contact monde = List.filter (fun mcht ->  let (x1,y1)= mcht.est_la and (x,y)= monde.moi.suis_la in (abs(x-x1)+abs(y-y1)=1)  )
                          (mechants_du_coin monde)

let commande_sure monde liste = 
 let rec sure_sans_chameaux monde liste = 
  match liste with 
  |[] -> Prends []
  |dir::t -> if depl_possib monde dir then Deplace dir else sure_sans_chameaux monde t 
 in
  
 let rec sure_avec_chameaux monde liste =  
 match liste with 
  |[] -> Prends []
  |dir::t -> if depl_possib monde dir then 
             begin
             let mcht_list = (mechants_du_coin monde) in 
             match mcht_list with 
              |[] -> Deplace dir
              |l ->
              let a,b = get_vector dir in 
              let i,j = 3+a,3+b in
              let alentours,_,_ =(calcule_alentours monde l) in 
              match alentours.(i).(j) with
               |FRANCHISSABLE (DANGEREUX ,_,_) -> sure_avec_chameaux monde t
               |_  -> Deplace dir
             end
            else sure_avec_chameaux monde t
             
  in
  (*sure_sans_chameaux monde liste*)
  match (sure_avec_chameaux monde liste) with 
   |Prends [] -> sure_sans_chameaux monde liste
   |l -> l
 
 
let mise_adaptee_mort monde liste =
  let max_mise = 10*Common.max_mise monde in
  min (monde.moi.bosse*80/100) (max max_mise (List.fold_left (fun max mechant -> if mechant.comportement.mise_optimale>max then mechant.comportement.mise_ultime else max) min_int liste))
                                                       
let mise_adaptee monde liste = 
  List.fold_left (fun max mechant -> if mechant.comportement.mise_optimale>max then mechant.comportement.mise_optimale else max) min_int liste 

let danger_de_mort monde sauve =
 let drap = ref LONGUE_VIE in
 let env = Array.create_matrix 3 3 S in (* mon environnement *)
 for i= 0 to 2 do
  for j=0 to 2 do
  let x,y = monde.moi.suis_la in
  let terre = try
              match Jouer.case_carte monde.desert_hostile (x+i-1, y+j-1) with 
               |Sable |Base |Palmier -> S
               |Omega -> (drap:= DANGER; O) 
              with _ -> S in
  let res = List.fold_left (fun _ mcht -> if mcht.est_la = (x+i-1, y+j-1) then (drap:= DANGER; C) else terre) S (mechants_du_coin monde) in
  env.(i).(j) <- res
  done
 done;
 if !drap = LONGUE_VIE then 0, Prends [] else
  let mechants = contact monde in
  let (mise,action) = 
  match env.(0).(1), env.(1).(0), env.(2).(1), env.(1).(2) with 
  |O,O,O,C -> mise_adaptee_mort monde mechants, commande_sure monde [Sud]
  |C,O,O,O -> mise_adaptee_mort monde mechants, commande_sure monde [Ouest]
  |O,C,O,O -> mise_adaptee_mort monde mechants, commande_sure monde [Nord]
  |O,O,C,O -> mise_adaptee_mort monde mechants, commande_sure monde [Est]
  |O,O,C,_ -> mise_adaptee_mort monde mechants, commande_sure monde [Sud; Est] 
  |O,O,_,C -> mise_adaptee_mort monde mechants, commande_sure monde [Est; Sud] 
  |_,O,O,C -> mise_adaptee_mort monde mechants, commande_sure monde [Ouest; Sud]
  |C,O,O,_ -> mise_adaptee_mort monde mechants, commande_sure monde [Sud; Ouest]
  |C,_,O,O -> mise_adaptee_mort monde mechants, commande_sure monde [Nord; Ouest]
  |_,C,O,O -> mise_adaptee_mort monde mechants, commande_sure monde [Ouest; Nord]
  |O,C,_,O -> mise_adaptee_mort monde mechants, commande_sure monde [Est; Nord]
  |O,_,C,O -> mise_adaptee_mort monde mechants, commande_sure monde [Nord; Est]
  |O,_,C,_ -> if sauve= Deplace Sud then mise_adaptee_mort monde mechants, commande_sure monde [Sud; Nord; Est]
              else mise_adaptee_mort monde mechants, commande_sure monde [Nord; Sud; Est]
  |_,O,_,C -> if sauve= Deplace Est then mise_adaptee_mort monde mechants, commande_sure monde [Est; Ouest; Sud]
              else mise_adaptee_mort monde mechants, commande_sure monde [Ouest; Est; Sud]  
  |C,_,O,_ -> if sauve= Deplace Nord then mise_adaptee_mort monde mechants, commande_sure monde [Sud; Nord; Ouest]
              else mise_adaptee_mort monde mechants, commande_sure monde [Nord; Sud; Ouest]  
  |_,C,_,O -> if sauve= Deplace Ouest then mise_adaptee_mort monde mechants, commande_sure monde [Est; Ouest; Nord]
              else mise_adaptee_mort monde mechants, commande_sure monde [Ouest; Est; Nord] 
  |_  -> 0, Prends []
  in
    if mise<>0 then
      Common.joue monde mise action mechants true
    else (mise,action)


let danger_prochain_tour monde sauve = 
 let drap = ref LONGUE_VIE in
 let env = Array.create_matrix 3 3 S in (* mon environnement *)
 for i= 0 to 2 do
  for j=0 to 2 do
  let x,y = monde.moi.suis_la in
  let terre = try
              match Jouer.case_carte monde.desert_hostile (x+i-1, y+j-1) with 
               |Sable |Base |Palmier -> S
               |Omega -> (drap:= DANGER; O) 
              with _-> S in
  let res = List.fold_left (fun _ mcht -> if mcht.est_la = (x+i-1, y+j-1) then (drap:= DANGER; C) else terre) S (mechants_du_coin monde) in
  env.(i).(j) <- res
  done
 done;
 if !drap = LONGUE_VIE then 0, Prends [] else
 let (mise,action)=
 match env.(0).(0), env.(2).(0), env.(2).(2), env.(0).(2) with 
          |C,O,C,O |O,C,O,C -> begin match sauve with 
                       |Prends _ |Pose _ -> 0, Prends []
                       |Deplace dir -> 1, Prends [] end
          |C,O,_,O |O,C,_,C -> begin match sauve with 
                       |Prends _ |Pose _ -> 0, Prends []
                       |Deplace Est -> 1, commande_sure monde [Est; Sud]
                       |_ -> 1, commande_sure monde [Sud; Est] end
          |O,C,O,_ |C,O,C,_ -> begin match sauve with 
                       |Prends _ |Pose _ -> 0, Prends []
                       |Deplace Sud -> 1, commande_sure monde [Sud; Ouest]
                       |_ -> 1, commande_sure monde [Ouest; Sud] end
          |_,O,C,O |_,C,O,C -> begin match sauve with 
                       |Prends _ |Pose _ -> 0, Prends []
                       |Deplace Ouest -> 1, commande_sure monde [Ouest; Nord]
                       |_ -> 1, commande_sure monde [Nord; Ouest] end
          |O,_,O,C |C,_,C,O -> begin match sauve with 
                       |Prends _ |Pose _  -> 0, Prends []
                       |Deplace Nord -> 1, commande_sure monde [Nord; Est]
                       |_ -> 1, commande_sure monde [Est; Nord] end
          |O,C,_,_ ->  begin match sauve with 
                       |Prends _ |Pose _  -> 0, Prends []
                       |Deplace Nord -> 1, commande_sure monde [Est; Sud; Ouest]
                       |Deplace dir -> 1, commande_sure monde (dir::[Est; Sud; Ouest]) end
          |_,O,C,_ -> begin match sauve with 
                       |Prends _ |Pose _  -> 0, Prends []
                       |Deplace Est -> 1, commande_sure monde [Nord; Ouest; Sud]
                       |Deplace dir -> 1, commande_sure monde (dir::[Nord; Ouest; Sud]) end
          |_,_,O,C -> begin match sauve with 
                       |Prends _ |Pose _  -> 0, Prends []
                       |Deplace Sud -> 1, commande_sure monde [Est; Nord; Ouest]
                       |Deplace dir -> 1, commande_sure monde (dir::[Est; Nord; Ouest]) end
          |C,_,_,O -> begin match sauve with 
                       |Prends _ |Pose _  -> 0, Prends []
                       |Deplace Ouest -> 1, commande_sure monde [Nord; Est; Sud] 
                       |Deplace dir -> 1, commande_sure monde (dir::[Nord; Est; Sud]) end
          |_ -> (* pas d'oméga mais je fuis les chameaux *)
                match sauve with 
                 |Pose _ -> begin
                   match env.(0).(1), env.(1).(0), env.(2).(1), env.(1).(2) with (* opération importante, absolument la faire *)
                     |C,_,_,_ |_,C,_,_ |_,_,C,_ |_,_,_,C -> mise_adaptee monde (mechants_du_coin monde), sauve
                     |_ -> 0, Prends []
                              end
                 |Prends _ -> 0, Prends []
                 |Deplace dir -> 1, commande_sure monde (dir::[Est; Nord; Sud; Ouest])
  in
    if mise=0 || mise=1 then
      (mise,action)
    else
      Common.joue monde mise action (mechants_du_coin monde) false                              
                             


(*
(* gros #*&! d'avant, on sait jamais... *)
let partition_danger_potentiel monde cmde liste = 
 (* on renvoit en plus la direction la plus sure *)
 let eosn = [|0;0;0;0|] in
 let new_liste = 
 List.filter (fun mcht -> let x1,y1 = mcht.est_la and x,y = monde.moi.suis_la in match cmde with
                |Deplace Est   -> if (x1 <= x)&&(0<=(y1-y))&&((y1-y)<3) then begin eosn.(3)<- 1+eosn.(3); true end else 
                                  if (x1 <= x)&&((-3)<(y1-y))&&((y1-y)<0) then begin eosn.(2)<- 1+eosn.(2); true end else
                                  false
                |Deplace Ouest -> if (x1 >= x)&&(0<=(y1-y))&&((y1-y)<3) then begin eosn.(3)<- 1+eosn.(3); true end else 
                                  if (x1 >= x)&&((-3)<(y1-y))&&((y1-y)<0) then begin eosn.(2)<- 1+eosn.(2); true end else
                                  false
                |Deplace Sud   -> if (y1 >= y)&&(0<=(x1-x))&&((x1-x)<3) then begin eosn.(0)<- 1+eosn.(0); true end else 
                                  if (y1 >= y)&&((-3)<(x1-x))&&((x1-x)<0) then begin eosn.(1)<- 1+eosn.(1); true end else
                                  false
                |Deplace Nord  -> if (y1 <= y)&&(0<=(x1-x))&&((x1-x)<3) then begin eosn.(0)<- 1+eosn.(0); true end else 
                                  if (y1 <= y)&&((-3)<(x1-x))&&((x1-x)<0) then begin eosn.(1)<- 1+eosn.(1); true end else
                                  false
                |Prends _ |Pose _ -> true )
               liste
 (* la direction sure est la ou il y a les moins de chameaux A MODIFIER *)           
 in let dir_sure_nb = ref 4 in
 let _ = Array.fold_left (fun m i -> let a = eosn.(i) in if a > m then begin dir_sure_nb:= i; a end else 
                                                         if a=m then begin dir_sure_nb:= 4; m end else m) min_int eosn in
 let dir_sure = match !dir_sure_nb with 
  |0 -> Some Est
  |1 -> Some Ouest
  |2 -> Some Sud
  |3 -> Some Nord 
  |_ -> None in
 new_liste,dir_sure
 
 
*)
