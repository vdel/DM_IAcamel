all: client

interfaces= tas_fibo.cmi types.cmi protocole.cmi jouer.cmi common.cmi IA_sauve_chemin.cmi IA_sauve_ramasse.cmi IA_sauve_livre.cmi IA_sauve.cmi IA_attaque.cmi IA_defendre.cmi IA.cmi
objets= tas_fibo.cmo protocole.cmo jouer.cmo common.cmo IA_sauve_chemin.cmo IA_sauve_ramasse.cmo IA_sauve_livre.cmo IA_sauve.cmo IA_attaque.cmo IA_defendre.cmo IA.cmo main.cmo

%.cmi: %.mli
	ocamlc -c $^

%.cmo: %.ml
	ocamlc -c $<

client: $(interfaces) $(objets)
	ocamlc -o $@ unix.cma str.cma graphics.cma $(objets)


clean:
	rm -f *.cmo *.cmi *.cmx *.o *.ml~ *.mli~ client
