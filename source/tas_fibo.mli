(************ Tas de Fibonacci ************)
(* structure utile pour Dijkstra.. on n'a pas besoin de marquer les sommets *)
type 'a t (* 'a correspond aux sommets du graphe, ici ce sera un couple (case de la carte, poids mini) *)
val new_t : 'a -> int -> 'a t
val is_empty : 'a t -> bool
val add : 'a * int -> 'a t -> 'a t
val extract_min :'a t -> ('a * int) * 'a t

