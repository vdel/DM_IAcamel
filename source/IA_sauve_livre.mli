(*Calcule, une fois qu'on est chargé, le chemin à faire pour déposer les ltermes*)
(*Classé par ordre de stupidité décroissante*)
val depose_stupide : Types.map_monde -> Types.chemin*int
val depose_ratio : Types.map_monde -> Types.chemin*int
val depose_moins_stupide : Types.map_monde -> Types.chemin*int

(*Calcule le chemin à faire pour aller chercher des ltermes*)
(*Classé par ordre de stupidité décroissante*)
val base_la_plus_proche : Types.map_monde -> Types.chemin*int
val meilleur_ratio : Types.map_monde -> Types.chemin*int

(*Chemin à parcourir pour se rapprocher d'un méchant*)
val traque_mechant : (Types.map_monde -> Types.chemin*int) -> Types.map_monde -> Types.chemin*int
