(************ Tas de Fibonacci ************)
(* structure utile pour Dijkstra.. on n'a pas besoin de marquer les sommets *)
type 'a arb = |F of ('a * int) |N of ('a * int * int * 'a arb list)  (* le 1er int est le poids du chemin, le second la taille de la liste*)
type 'a t = {mutable tas : ('a arb list)} (* le minimum est toujours en tête de liste *)

let poids a = match a with
  |F (_,lg) -> lg
  |N (_,lg,_,_) -> lg 
	
let new_t v n = {tas = [F (v,n)]}

let is_empty l = if l.tas = [] then true else false

let union l1 l2 = match l1.tas, l2.tas with
  |[],_ -> l2
  |_,[] -> l1
  |a1::q1, a2::q2 -> let (y,x) = (poids a1, poids a2) in 
    if x<=y then {tas= l2.tas@l1.tas} else {tas = l1.tas@l2.tas}

let add (x,pds) l = union {tas = [F (x,pds)]} l

let extract_min l =
  match l.tas with
    |[] -> failwith "Tas vide"
    |a_min::tl ->
      begin  
        l.tas <- tl;  (* enlève le min et ajoute ses fils comme nouveaux arbres *)
        let minimum = 
          match a_min with 
	    |F (x,poids) -> (x,poids)
	    |N (x,poids,_,fils) -> l.tas <- fils@l.tas; (x,poids) 
        in	   
          let tn = 1000 in (* devrait être O(log n) a la place du 1000, n est la taille de l.tas *)
          let tab = Array.make tn None in
	  let rec regroup l = (* regroupe les racines de même nb de fils entre elles *) 
	    match l with
	      |[] -> ()
              |(F (x,poids_a) as a)::q -> begin
                match tab.(0) with
	          |None -> tab.(0) <- Some a; regroup q 
	          |Some b -> begin
                    let poids_b = poids b in
                    let c = 
                      if poids_b>poids_a then
                        N(x,poids_a,1,[b])
                      else
                        match b with
                          |F(y,poids_b) -> N(y,poids_b,1,[a])
                          |_ -> failwith "pas possible"
                    in
                      tab.(0) <- None;
                      regroup (c::q)
                  end
              end	      
	      |(N (x,poids_a,nbr_fils,fils) as a)::q -> begin
                match tab.(nbr_fils) with
	          |None -> tab.(nbr_fils) <- Some a; regroup q 
	          |Some b -> begin
                    let poids_b = poids b in
                    let c = 
                      if poids_b>poids_a then
                        N(x,poids_a,nbr_fils+1,b::fils)
                      else
                        match b with
                          |F(y,poids_b) -> N(y,poids_b,1,[a])
                          |N(y,poids_b,nbr_fils_b,fils_b) -> N(y,poids_b,nbr_fils_b+1,a::fils_b)
                    in
                      tab.(nbr_fils) <- None;
                      regroup (c::q)
                  end
              end
	  in
	    regroup l.tas; (* on le fait sur le tas et on le remet à jour à l'aide du tableau *)
	    let l_tas = ref [] in
	    for i=0 to (tn-1) do
	      match tab.(i) with
	        |None   -> ()
	        |Some a -> begin
                  match !l_tas with
                    |[] -> l_tas := [a]
                    |min::q -> if (poids min)>(poids a) then l_tas:= a::(!l_tas) 
	                                                else l_tas:= min::a::q
                end
	    done;
	    l.tas <- !l_tas;
	    minimum, l  
          end              
