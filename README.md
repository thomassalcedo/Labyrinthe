# Labyrinthe
Vous êtes dans un labyrinthe et vous devez vous échapper avant d'être rattrapé par l'ennemi

# Compilation 
```ocamlc -thread graphics.cma unix.cma threads.cma Projet_Final.ml -o Labyrinthe.out```

# Lancement
```./Labyrinthe.out```
ou
```ocamlrun Labyrinthe.out```

# Déplacement
```z q s d```

# Bugs Connus: 
Lorsque que la taille du terrain n'est pas un multitple de 10, le labyrinthe n'est pas centré et une partie peut même être coupée
