(*Calcul un chemin entre deux points*)
(*Classé par ordre de stupidité décroissante*)
val chemin_pas_si_naif : Types.map_monde -> Types.position -> Types.position -> Types.chemin*int
val chemin_dijkstra : Types.map_monde -> Types.position -> Types.position -> Types.chemin*int  (*chemin le plus court, mais trop lent*)
val chemin_A_star : Types.map_monde -> Types.position -> Types.position -> Types.chemin*int    (*plus rapide mais pas le chemin le plus court*)

(*Distance entre deux points*)
val distance_sable : Types.map_monde -> Types.position -> Types.position -> int     (*Distance à vol d'oiseau, mais très rapide*)
val distance_dijkstra : Types.map_monde -> Types.position -> Types.position -> int
val distance_A_star : Types.map_monde -> Types.position -> Types.position -> int

(*Fait passer un chemin par des bases et des ltermes pas trop loin*)
(*Bonne idée à la base, mais temps de calul trop long, ou pas assez développé*)  
val ameliore_chemin_stupide : Types.map_monde -> Types.chemin -> int -> Types.chemin     (*améliore rien*)
val ameliore_chemin_intelligent : Types.map_monde -> Types.chemin -> int -> Types.chemin (*améliore*)

