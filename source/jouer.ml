open Types

let client = new Protocole.client

exception En_dehors

let taille_carte carte = Array.length carte.(0), Array.length carte

let case_carte carte (x,y) =
  let maxx,maxy = taille_carte carte in
  if (x < 0 || x > maxx ||
      y < 0 || y > maxy)
  then raise En_dehors
  else
(*    carte.(maxy - y).(x-1)*)
    carte.(y-1).(x-1)
    (* Ne pas utiliser de try carte.(y).(x) with Invalid_argument "index out of bounds" 
     * car ça ne marcherait pas avec l'option '-unsafe' de ocamlopt
     *)

let change_case_carte carte (x,y) terrain=
  let maxx,maxy = taille_carte carte in
  if (x < 0 || x > maxx ||
      y < 0 || y > maxy)
  then raise En_dehors
  else
    carte.(y-1).(x-1)<-terrain

let init hote port =
  client#connect hote port;

  (* on prévient le serveur *)
  client#send "Joueur";
  let carte = client#lit_carte () in 
  let maxx,maxy = taille_carte carte in
  Printf.printf "La carte du monde est de taille %dx%d\n" maxx maxy ;

  (* initialisation *)
  let (id_client,capacite,eau) = client#lit_config_initiale () in
  Printf.printf 
    "Je suis le chameau numero %d, ma place pour les λ-termes est %d et chacune de mes bosses contient %d litres d'eau super (sans plomb).\n"
    id_client capacite (eau/2);

  (* Première lecture qui va permettre de savoir où est le chameau *)
  let apparition_des_chameaux = client#lit_actions () in 
  let _ = client#lit_termes () in (* aucun terme au premier tour *)

  let positions =
    List.map
      (function
        | id, [Apparait pos] -> id, pos
        | _ -> failwith "Seulement Apparait pour l'initialisation"
      )
      apparition_des_chameaux
  in
  (
    carte,
    (id_client,capacite,eau),
    positions
  )


let joue_un_tour mise commande =
  client#envoie_commande mise commande ;
  (* Et voilà ce qu'il s'est passé pendant le tour *)
  let actions = client#lit_actions () in

  (* Y a-t'il des λ-termes à mes pieds ? *)
  let liste_de_termes = client#lit_termes () in 

  actions, liste_de_termes


let stop () =
  print_endline "Bande de lâches, vous partez déjà ?" ;
  client#disconnect ()



  (** Affichage *)

let afficher_les_ordres liste =
    let aux = function
        |Nord -> "Nord"
        |Sud -> "Sud"
        |Est -> "Est"
        |Ouest -> "Ouest" in
    let aux2 = function
        | Apparait _ -> failwith "Pas d'apparition en cours de jeu !"
        | Apris _ -> print_string " prend un λ-terme"
        | Apose _ -> print_string " pose un λ-terme"
        | Abouge a -> print_string (" se deplace au " ^ (aux a)) in
  List.iter
  (fun (id,ordres) -> 
      print_string "Chameau numéro "; print_int id ; List.iter aux2 ordres ;
      print_newline ()) liste


let afficher_les_termes p =
  if p = [] then
    print_endline "Mmmh, rien ici..."
  else begin
    print_string "Des termes :";
    List.iter
      (fun (i,_,_) -> print_string " "; print_int i)
      p ;
    print_endline " !"
  end


    (** Dessin **)

let dess car l =
    let tbl = Hashtbl.create 1 
    and k = ref 96 in
    List.iter (fun (i,(x,y)) -> incr k ; Hashtbl.add tbl (x,y) (i,char_of_int !k)) l;
    print_newline ();
    print_string "  ";
    let maxx,maxy = taille_carte car in 
    for i=1 to maxx do
        print_int (i mod 10)
    done;
    print_newline ();
    print_newline ();
    for y = 1 to maxy do
        print_int (y mod 10); print_char ' ';
        for  x = 1 to maxx do
            try print_char (snd (Hashtbl.find tbl (x,y)))
            with Not_found ->
                begin
                    match case_carte car (x,y) with
                    | Sable   -> print_char ' '
                    | Palmier -> print_char 'T'
                    | Omega   -> print_char '$'
                    | Base    -> print_char '#'
                end
        done;
        print_newline ()
    done;
    print_newline ();
    Hashtbl.iter (fun _ -> fun (i,k) -> print_int i ; print_string " -> ";
    print_char k ; print_newline ()) tbl



