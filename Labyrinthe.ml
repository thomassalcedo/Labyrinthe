(*#load "threads.cma";;
#load "graphics.cma";;
#load "unix.cma";;*)

(* ------------------------------------------------------------------------------------------------------------------------------------------- *)


(* 
  Salcedo Thomas 
  Prigodin David 
*)


(* ------------------------------------------------------------------------------------------------------------------------------------------- *)

(* let l = print_string "Entrez largeur: ";
        read_int ();;

let h = print_string "Entrez hauteur: ";
        read_int ();; *)

(* let upleftx = 50;;
let uplefty = 550;;
let rapport = if l/10 = 0 then 1
              else l/10;;
let taille_case = 50/rapport;; 

let rang = Array.make (l*h) 1;; *)

let t = print_string "Entrez taille: ";
        read_int ();;

let upleftx = 50;;
let uplefty = 550;;
let rapport = if t/10 = 0 then 1
              else t/10;;
let taille_case = 50/rapport;; 

let rang = Array.make (t*t) 1;;




Graphics.open_graph " 600x600";;
Graphics.set_window_title "Pacman Labyrinthe";;

module type UF = sig
  type 'a t
  val create : int -> 'a t
  val find : 'a t -> int -> int
  (*val union : 'a t -> int -> int -> unit*)
  val union : 'a t -> int -> int -> 'a t
end


module UF = struct
  type 'a t = 'a array;;

  let create n = 
    let a = Array.make n 0 in
    for i=0 to n-1 do
      a.(i) <- i
    done;
    a;;

  let rec find uf n = 
    let changer_parent uf n parent =
      if uf.(n) <> parent then uf.(n) <- parent;
      parent in
    let parent = uf.(n) in
    if parent = n then 
      n
    else 
      changer_parent uf n (find uf parent);;


  (*uf est présent dans chaque if, pour éviter que ocaml considère que ce n'est pas une fonction *)
  let union uf n m = 
    let nRacine = find uf n in
    let mRacine = find uf m in
    if nRacine <> mRacine then begin
      if rang.(nRacine) < rang.(mRacine) then begin
        uf.(nRacine) <- m;
        rang.(m) <- rang.(m) + rang.(n);
        uf
      end
      else begin
        uf.(mRacine) <- n;  
        rang.(n) <- rang.(n) + rang.(m);
        uf
      end
    end
    else
     uf;;
  
end;;



let mur_au_hasard l h = 
  Random.self_init ();
  let n = Random.int ((l-1) * h + l * (h-1)) in
  if n < (l-1) * h
  then (0,n mod (l-1),n/(l-1))
  else let n2 = n - (l-1) * h in
    (1,n2 mod l,n2/l);;

let cases_adjacentes l h (d,x,y) = 
  match d with
  |0 when x <> (l-1) -> ( (y*h +x) , (y*h +x+1) )
  |0 -> ( (y*h +x) , int_of_float nan )
  |1 when y <> (h-1) -> ( (y*h +x) , ((y+1)*h +x) )
  |1 -> ( (y*h +x) , int_of_float nan )
  |_ -> invalid_arg "cas impossible cases_adjacentes";;

let aff a = 
    for i=0 to Array.length a -1 do 
        for j=0 to Array.length a.(i) -1 do
            print_string " ";
            print_int a.(i).(j);
            print_string " ";
        done;
    done;
    print_int (Array.length a);;


let generate_lab l h = 

  let mur_present = Array.init 2 (fun _ -> Array.init l (fun _ -> Array.make h true)) in

  let uf = UF.create (l*h) in
  let i = ref 0 in

  
  while !i < (l*h -1) do
    let m = mur_au_hasard l h in
    let (d,x,y) = m in
    let (case1,case2) = cases_adjacentes l h m in

    if UF.find uf case1 = UF.find uf case2 then begin
      ()
    end

    else begin
      let uf = UF.union uf case1 case2 in
      (*let (d,x,y) = m in*)
      mur_present.(d).(x).(y) <- false;
      i := !i +1;
    end;

  done;
  mur_present.(0).(l-1).(h-1) <- false;
  mur_present;;



let trace_pourtour upleftx uplefty taille_case l h = 
  Graphics.set_color Graphics.black;

  Graphics.moveto upleftx uplefty;
  Graphics.lineto (upleftx+(taille_case*l)) uplefty;

  Graphics.moveto upleftx uplefty;
  Graphics.lineto upleftx (uplefty-(taille_case*l));
  
  (*On efface l'entrée*)
  Graphics.set_color Graphics.white;
  Graphics.moveto upleftx uplefty;
  Graphics.lineto upleftx (uplefty-taille_case);;


let trace_mur upleftx uplefty taille_case (d,x,y) = 
  Graphics.set_color Graphics.black;
  match d with 
  |0 -> Graphics.moveto (upleftx+(taille_case*(x+1))) (uplefty-(taille_case*y));Graphics.lineto (upleftx+(taille_case*(x+1))) ((uplefty-(taille_case*y))-taille_case)
  |1 -> Graphics.moveto (upleftx+(taille_case*x)) (uplefty-(taille_case*(y+1)));Graphics.lineto ((upleftx+(taille_case*x))+taille_case) (uplefty-(taille_case*(y+1)))
  |_ -> invalid_arg "cas impossible trace_mur";;


let trace_lab upleftx uplefty taille_case l h mur_present = 
  trace_pourtour upleftx uplefty taille_case l h;
  for d=0 to Array.length mur_present-1 do
    for x=0 to Array.length mur_present.(0)-1 do
      for y=0 to Array.length mur_present.(0).(0)-1 do
        if mur_present.(d).(x).(y) then 
          trace_mur upleftx uplefty taille_case (d,x,y)
      done
    done
  done;
  (*On efface la sortie*)
  Graphics.set_color Graphics.white;
  Graphics.moveto (upleftx+(taille_case*l)) (uplefty-(taille_case*l));
  Graphics.lineto (upleftx+(taille_case*l)) ((uplefty-(taille_case*l))+taille_case);;


let gen_voisines mur_present = (*Le tableau voisines est un tableau de taille = nombre de case où chaque case est un tableau de taille 4 representant les voisins
                                  Si le voisin est bloqué par un mur, ça valeur est -1. Sinon c'est le numéro de la case qui est inscrit*)
  let voisines = Array.init (t*t) (fun _ -> Array.make 4 (-1)) in
  
  let j = ref 0 in 
  for d=0 to Array.length mur_present-1 do
    for x=0 to Array.length mur_present.(0)-1 do
      for y=0 to Array.length mur_present.(0).(0)-1 do
        if not (mur_present.(d).(x).(y)) then 
          let (case1,case2) = cases_adjacentes t t (d,x,y) in

          j := 0;
          while !j < 4 do
            if voisines.(case1).(!j) = -1 then begin
              voisines.(case1).(!j) <- case2;
              j := 4;
              end;
          
            j := !j +1;
          done;

          j := 0;
          while !j < 4 do
            if voisines.(case2).(!j) = -1 then begin
              voisines.(case2).(!j) <- case1;
              j := 4;
              end;
          
            j := !j +1;
          done;
      done;
    done;
  done;
  j := 0;
  while !j < 4 do
    begin
    if voisines.(0).(!j) = (t*t) -1 then 
      voisines.(0).(!j) <- -1;
    end;
    begin
    if voisines.((t*t) -1).(!j) = 0 then
      voisines.((t*t) -1).(!j) <- -1;
    end;
    j := !j + 1;
  done;

  voisines;;


(*Fonctions Pacman*)


  let update case_pacman couleur = (*Fonction pour le pacman et pour le fantome, permet de l'afficher*)
    let line,col = (!case_pacman / t), (!case_pacman mod t) in
    let x = upleftx + (col*taille_case) in 
    let y = uplefty - (line*taille_case) in 
    Graphics.set_color couleur;
    Graphics.fill_circle (x + (taille_case)/2) (y - (taille_case)/2) ((taille_case/2)-1);;

let emettre_son () = Graphics.sound 255 10;;

let deplacement case_pacman touche mur_present = 
  let line,col = (!case_pacman / t), (!case_pacman mod t) in
  match touche with
  (*deplacement en haut *)
  |'z' when line <> 0 && not (mur_present.(1).(col).(line -1)) -> update case_pacman Graphics.white; case_pacman := !case_pacman - t;update case_pacman Graphics.blue;
  (*deplacement a gauche*)
  |'q' when col <> 0 && not (mur_present.(0).(col-1).(line)) -> update case_pacman Graphics.white; case_pacman := !case_pacman -1; update case_pacman Graphics.blue;
  (*deplacement en bas*)
  |'s' when line <> (t -1) && not (mur_present.(1).(col).(line)) -> update case_pacman Graphics.white; case_pacman := !case_pacman + t; update case_pacman Graphics.blue;
  (*deplacement a droite*)
  |'d' when (col <> (t -1) && not (mur_present.(0).(col).(line))) || line = t-1 && col = t-1 -> update case_pacman Graphics.white; case_pacman := !case_pacman +1; update case_pacman Graphics.blue;
  (*rencontre d'un mur*)
  |'z'|'q'|'s'|'d' ->  emettre_son ();
  (*mauvaise touche appuyée*)
  |_ -> ();;



(*Fonctions Fantome*)


let rec est_relie src dst evite voisines = 
  if src = dst then true
  else if src = (t*t) -1 then false
      else begin  

        let b = ref false in
        for c = 0 to 3 do
          if voisines.(src).(c) <> evite && voisines.(src).(c) <> -1 then
            if est_relie voisines.(src).(c) dst src voisines then b := true;
        done;
        !b;

      end;;


let deplacement_fantome case_fantome case_pacman voisines= 
  update case_fantome Graphics.white;
  let enRecherche = ref true in
  let i = ref 0 in
  let possibilites = voisines.(!case_fantome) in
  let evite = !case_fantome in
  let dst = !case_pacman in
  while !enRecherche && !i < 4 do

    if possibilites.(!i) <> -1 then begin
      if (est_relie possibilites.(!i) dst evite voisines) then begin
        case_fantome := possibilites.(!i);
        enRecherche := false;
        end;
      end;

    i := !i +1;
  done;
  
  update case_fantome Graphics.red;;


(*Fonctions communes*)

let verif case_pacman case_fantome = 
  let enCour = ref true in

  begin
  if !case_pacman = (t*t) then 
    enCour := false;
  end;

  begin
  if !case_pacman = !case_fantome then
    enCour := false;
  end;

  !enCour
  ;;


let finDePartie result = 
  Graphics.clear_graph ();
  Graphics.set_text_size 45; (*Marche pas mais je sais pas pourquoi...*)
  Graphics.moveto (Graphics.size_x () / 2) (Graphics.size_y () / 2);

  match result with
 |true -> Graphics.set_color Graphics.blue;
          Graphics.draw_string "Gagnee";
          Graphics.set_color Graphics.black;
          Graphics.moveto (Graphics.size_x () / 2) (Graphics.size_y () / 2 - 40);
          Graphics.draw_string "Appuyez sur 'q' pour quitter !";

 |false -> Graphics.set_color Graphics.red;
           Graphics.draw_string "Perdu";
           Graphics.set_color Graphics.black;
           Graphics.moveto (Graphics.size_x () / 2) (Graphics.size_y () /2 - 40);
           Graphics.draw_string "Appuyez sur 'q' pour quitter !";;



(*Lancement jeu*)


let fantome (case_fantome,case_pacman,voisines,enCour) = 
  
  update case_fantome Graphics.red; (*On affiche le fantome*)

  while !enCour do 

    enCour := verif case_pacman case_fantome;

    begin
    if !enCour then
      Unix.sleep 1;
      deplacement_fantome case_fantome case_pacman voisines;
    end;

  done;

  
  begin
  if !case_pacman = t*t then finDePartie true
  else finDePartie false;
  end;;
  

 


let () = 

  let mur_array = generate_lab t t in
  trace_lab upleftx uplefty taille_case t t mur_array;

  let case_fantome = ref (t - 1) in
  let case_pacman = ref 0 in

  let enCour = ref true in


  let voisines = gen_voisines mur_array in 

  ignore @@ Thread.create fantome (case_fantome,case_pacman,voisines,enCour);

  update case_pacman Graphics.blue; (*On affiche pacman*)
  while !enCour do

    enCour := verif case_pacman case_fantome;

    begin
    if !enCour then
    let touche = Graphics.read_key () in 
    deplacement case_pacman touche mur_array;
    end;

  done;

  begin
  if !case_pacman = t*t then finDePartie true
  else finDePartie false;
  end;


  let touche = ref 'a' in
  while !touche <> 'q' do
    touche := Graphics.read_key ();
  done;
  Graphics.close_graph ();;