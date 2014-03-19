type terrain= SABLE | OMEGA | BASE | PALMIER;;
type parametres = {
  mutable name : string;
  mutable largeur : int;
  mutable hauteur : int;
  mutable capacity : int;
  mutable nb_chameaux : int;
  mutable nb_lterms : int;
  mutable nb_bases : int;
  mutable freq_omega : int;
  mutable freq_palmier : int;
  mutable freq_sable : int;
  mutable magic_number : int;
};;

(* trouve une position sur la carte qui satisfait f*)
let find_pos f w h=
  let i=ref (Random.int w)
  and j=ref (Random.int h)
  in 
    while not (f !i !j) do
     i:=Random.int w;
     j:=Random.int h;      
    done;
    ( !i, !j)
;;

(* génère le fichier .lterm*)
let rec gen_bases out_channel map width height capacity nb_lterms_tot nb_lterms_restant total_lterm bases_list=
  let total = ref 0 in
  match bases_list with
  | [] -> total_lterm
  | (i,j)::q -> 
  begin
    output_string out_channel "  ((";
    output_string out_channel (string_of_int (i+1));
    output_string out_channel ",";
    output_string out_channel (string_of_int (j+1));
    output_string out_channel "),\n";
    output_string out_channel "    [\n";
    let bases_restantes=List.length bases_list in
    let n_term = (if q=[] then nb_lterms_restant else Random.int (2*nb_lterms_restant/bases_restantes)) in
    let n_term = if n_term = 0 then 1 else if nb_lterms_restant-n_term<(bases_restantes-1) then nb_lterms_restant-(bases_restantes-1) else n_term
    in
      for k=1 to n_term do
        let (i2,j2)=find_pos (fun x y -> (map.(x).(y)=BASE || map.(x).(y)=SABLE) && not ((x,y)=(i,j))) width height in
          output_string out_channel "    lterm{uid=";
          output_string out_channel (string_of_int (nb_lterms_tot-nb_lterms_restant+k));
          output_string out_channel ", dest=(";
          output_string out_channel (string_of_int (i2+1));
          output_string out_channel ",";
          output_string out_channel (string_of_int (j2+1));
          output_string out_channel "), place=";
          let place=Random.int capacity+1 in
          total:= !total+place;
          output_string out_channel (string_of_int place);
          if k=n_term then 
            output_string out_channel "}\n"
          else
            output_string out_channel "},\n";
      done;
      output_string out_channel "    ]\n";
      if q=[] then
        output_string out_channel "  )\n"
      else
        output_string out_channel "  ),\n";
      gen_bases out_channel map width height capacity nb_lterms_tot (nb_lterms_restant-n_term) (total_lterm+ !total) q
  end  
;;

let gen p=
  let map = Array.make_matrix p.largeur p.hauteur SABLE in
  (* génération de la carte *)
  let out_channel = open_out (p.name^".map") in
    (*on écrit la taille de la carte *)
    output_string out_channel (string_of_int p.largeur); 
    output_char out_channel ' ';
    output_string out_channel (string_of_int p.hauteur);
    output_char out_channel '\n';
    (* on décide où démarrent les chameaux *)
    let pos_chameaux = ref [] in
    for k=1 to p.nb_chameaux do
      pos_chameaux:= (find_pos (fun i j -> not (List.mem (i,j) !pos_chameaux)) p.largeur p.hauteur):: !pos_chameaux
    done;
    (* on décide où on place les bases *)
    let pos_bases = ref [] in
    for k=1 to p.nb_bases do
      pos_bases:= (find_pos (fun i j -> not (List.mem (i,j) !pos_bases || List.mem (i,j) !pos_chameaux)) p.largeur p.hauteur):: !pos_bases
    done;
    (*on génère la carte*)
    let tot_freq=p.freq_omega+p.freq_palmier+p.freq_sable in
    for j=0 to p.hauteur-1 do
      for i=0 to p.largeur-1 do
        if List.mem (i,j) !pos_chameaux then
        begin
          map.(i).(j)<- SABLE;
          output_char out_channel 'C'
        end
        else
          if List.mem (i,j) !pos_bases then
          begin
            map.(i).(j)<- BASE;
            output_char out_channel '@'
          end
          else
          let k=Random.int tot_freq in
            if 0<=k && k<p.freq_omega then
            begin
              map.(i).(j)<- OMEGA; output_char out_channel '~';
            end
            else if p.freq_omega<=k && k<(p.freq_omega+p.freq_palmier) then
            begin
              map.(i).(j)<- PALMIER; output_char out_channel '#';
            end
            else
            begin
              map.(i).(j)<- SABLE; output_char out_channel '.';
            end
      done;
      output_char out_channel '\n';
    done;
    (*on génère les lambda termes*)
    let out_channel = open_out (p.name^".lterm") in
    output_string out_channel "[\n";
    let total = gen_bases out_channel map p.largeur p.hauteur p.capacity p.nb_lterms p.nb_lterms 0 !pos_bases in
    output_string out_channel "]\n";
    output_string out_channel ("Total des lambda termes: "^(string_of_int total))
;;

let main ()=
  if (Array.length Sys.argv)<>2 then
    print_string "Arguments:\n mapgen nom_carte\n"
  else
  begin
    try
      let in_channel = open_in (Sys.argv.(1)^".par") in
      let params = {name=Sys.argv.(1); largeur=100; hauteur=100; capacity=50; nb_chameaux=1; nb_lterms=100; nb_bases=12; freq_omega=1; freq_palmier=2; freq_sable=15; magic_number=0} in
      try
        while true do
          let line=input_line in_channel in
            let param_name=ref ""
            and param_value=ref ""
            and reading_value=ref false in
              for i=0 to String.length line-1 do
                if line.[i]='=' then reading_value:=true
                else
                  if !reading_value then
                    param_value:= !param_value^(Char.escaped line.[i])
                  else
                    param_name:= !param_name^(Char.escaped line.[i])
              done;
              match !param_name with
                |"LARGEUR" -> params.largeur<-int_of_string !param_value
                |"HAUTEUR" -> params.hauteur<-int_of_string !param_value
                |"CAPACITE_MAX" -> params.capacity<-int_of_string !param_value
                |"NB_CHAMEAUX" -> params.nb_chameaux<-int_of_string !param_value
                |"NB_LTERMS" -> params.nb_lterms<-int_of_string !param_value
                |"NB_BASES" -> params.nb_bases<-int_of_string !param_value
                |"FREQ_OMEGA" -> params.freq_omega<-int_of_string !param_value
                |"FREQ_PALMIER" -> params.freq_palmier<-int_of_string !param_value
                |"FREQ_SABLE" -> params.freq_sable<-int_of_string !param_value
                |"MAGIC_NUMBER" -> params.magic_number<-int_of_string !param_value
                |name -> print_string ("Paramètre inconnu: "^name^"\n"); exit(-1)
        done;
      with
        |_->
        if params.largeur<2 || params.hauteur<2 || params.hauteur*params.largeur<params.nb_chameaux+params.nb_bases then
          print_string "Dimensions trop faibles\n"
        else if params.nb_lterms<params.nb_bases then print_string "Il doit y avoir plus de lambda termes que de bases\n"
        else
          begin
            Random.init params.magic_number;
            gen params
          end
    with
    |Sys_error(_)-> print_string ("Impossible d'ouvrir le fichier "^Sys.argv.(1)^".par\n");
  end
;;

main ();;
