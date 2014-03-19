(* Fichier de protocole pour les élèves.
 *)

open Unix
open Types


let dir_to_string = function
  | Nord -> "N"
  | Sud -> "S"
  | Est -> "E"
  | Ouest -> "O"



(* cree une carte faite de Terre *)
let creer_carte (largeur, hauteur) = Array.make_matrix hauteur largeur Sable

(* Découpe une chaine en liste des sous-chaines séparées par des espaces *)
let decoupe_string s = Str.split (Str.regexp " ") s

(* Récupère le numéro du chameau 18 dans "#18" *)
let id_of_string s =
  assert (s.[0] = '#') ;
  s.[0] <- '0' ;
  int_of_string s


(** Mon objet client *)

class client = object(self)
  val mutable in_channel = Pervasives.stdin
  val mutable out_channel = Pervasives.stdout
  val mutable is_connected = false

  method connect host port = 
    assert (not is_connected) ;
    let h =
      try gethostbyname host with
      Not_found -> 
        prerr_endline "Hôte inconnu. Vérifiez le nom de l'hôte ou utilisez son adresse IP.";
        exit 1
    in
    let addr = h.h_addr_list.(0) in
    let i, o = open_connection (ADDR_INET(addr, port))
    in
    in_channel <- i ; out_channel <- o ;
    is_connected <- true
  
  method disconnect () =
    assert is_connected;
    shutdown_connection in_channel ;
    close_out out_channel;
    close_in in_channel;
    is_connected <- false

  method send s = 
    output_string out_channel (s^"\n");
    flush out_channel;

  method private read_line () = 
    try input_line in_channel
    with
    End_of_file ->
      print_endline "Fin de la partie, le serveur a fermé la connection.";
      exit 0

  method lit_carte () =
    let l = self#read_line () in
    let (x,y) = Scanf.sscanf l "%d %d" (fun x y -> x,y) in
    let carte = creer_carte (x,y) in
    for j = 1 to y do
      let l = self#read_line () in
      for i = 1 to x do
        let e = 
          match l.[i-1] with
          | '.' -> Sable
          | '#' -> Palmier
          | '~' -> Omega
          | '@' -> Base
          | c -> failwith ("Charactère inconnu: "^String.make 1 c)
        in 
        carte.(j-1).(i-1) <- e;
      done;
    done;
    carte
  
  method lit_config_initiale () = 
    let ligne = self#read_line () in
    let string_liste = decoupe_string ligne in
    match List.map int_of_string string_liste with
    | x::y::z::[] -> x,y,z
    | _ -> failwith "Configuration initiale attendue."


  method lit_termes () =
    let ligne = self#read_line () in
    let string_liste = decoupe_string ligne in
    let rec decoupe_en_termes = function
      | id::pos_x::pos_y::poids::suite ->
          (id,(pos_x,pos_y),poids)::decoupe_en_termes suite
      | _ -> []
    in decoupe_en_termes (List.map int_of_string string_liste)


  method lit_actions () = 
    let ligne = self#read_line () in
    let string_liste = decoupe_string ligne in

    let chameau_id = ref 0 in
    let chameau_actions = ref [] in
    let act a = chameau_actions := a :: !chameau_actions
    in
    let rec decoupe_en_actions = function
      | [] -> [!chameau_id, List.rev !chameau_actions]
      | "X"::x::"Y"::y::suite ->
          act (Apparait (int_of_string x,int_of_string y)) ; decoupe_en_actions suite
      | "N"::suite -> act (Abouge Nord) ; decoupe_en_actions suite
      | "S"::suite -> act (Abouge Sud) ; decoupe_en_actions suite
      | "O"::suite -> act (Abouge Ouest) ; decoupe_en_actions suite
      | "E"::suite -> act (Abouge Est) ; decoupe_en_actions suite
      | "P"::terme::suite -> act (Apris [int_of_string terme]) ; decoupe_en_actions suite
      | "D"::terme::suite -> act (Apose [int_of_string terme]) ; decoupe_en_actions suite
      | ord ::suite ->
          let id = !chameau_id in
          chameau_id := id_of_string ord ;

          if id = 0 then begin (* premier # rencontré *)
            assert (!chameau_actions = []) ;
            decoupe_en_actions suite
          end
          else begin (* on associe les actions au chameau *)
            let a = (id, List.rev !chameau_actions) in
            (* on a empilé les actions dans l'ordre inverse *)
            chameau_actions := [] ;
            a :: decoupe_en_actions suite
          end ;
    in
      decoupe_en_actions string_liste


  method envoie_commande bid a =
    self#send (string_of_int bid ^ " " ^
    (match a with
    | Deplace dir -> "Deplace " ^ dir_to_string dir

    | Prends ol -> "Prends" ^ (
      List.fold_left (fun s o -> s^" "^string_of_int o) "" ol)

    | Pose ol -> "Pose" ^ (
      List.fold_left (fun s o -> s^" "^string_of_int o) "" ol)
    ))
  
end (* class client *)


let terminal_char_by_char () =
  let terminal_io = Unix.tcgetattr Unix.stdin in
  terminal_io.Unix.c_icanon <- false ;
  Unix.tcsetattr Unix.stdin Unix.TCSANOW terminal_io ;
  Unix.in_channel_of_descr Unix.stdin
