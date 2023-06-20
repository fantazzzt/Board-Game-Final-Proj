(** Once we figure out players, we'll add info to the territory type of
    which player owns the territory.*)


type player_id = string


type terr_id = string


type cont_id = string

let player_nil = "none"


let player1 = "Player 1"


let player2 = "Player 2"


let player3 = "Player 3"


type troops = int


type territory = {

  tname : terr_id;

  continent : cont_id;

  neighbors : terr_id list;

  mutable owned : player_id;

  mutable troops : troops;

}


type continent = {

  cname : cont_id;

  territories : territory list;

}


type gameboard = {

  continents : continent list;

  players : player_id list;

}


(* South America *)
let venezuela =

  {

    tname = "Venezuela";

    continent = "South America";

    neighbors = [ "Peru"; "Brazil"; "Central America" ];

    owned = "none";

    troops = 35;

  }


let peru =

  {

    tname = "Peru";

    continent = "South America";

    neighbors = [ "Venezuela"; "Brazil"; "Argentina" ];

    owned = "none";

    troops = 35;

  }


let brazil =

  {

    tname = "Brazil";

    continent = "South America";

    neighbors = [ "Venezuela"; "Peru"; "Argentina"; "North Africa" ];

    owned = "none";

    troops = 25;

  }


let argentina =

  {

    tname = "Argentina";

    continent = "South America";

    neighbors = [ "Peru"; "Brazil" ];

    owned = "none";

    troops = 10;

  }


let south_america =

  {

    cname = "South America";

    territories = [ venezuela; peru; brazil; argentina ];

  }


(* Asia *)
let siam =

  {

    tname = "Siam";

    continent = "Asia";

    neighbors = [ "India"; "China"; "Indonesia" ];

    owned = "none";

    troops = 10;

  }


let india =

  {

    tname = "India";

    continent = "Asia";

    neighbors = [ "Siam"; "Middle East"; "Afghanistan"; "China" ];

    owned = "none";

    troops = 10;

  }


let middle_east =

  {

    tname = "Middle East";

    continent = "Asia";

    neighbors =

      [

        "India";

        "Afghanistan";

        "Ukraine";

        "Southern Europe";

        "Egypt";

        "East Africa";

      ];

    owned = "none";

    troops = 10;

  }


let afghanistan =

  {

    tname = "Afghanistan";

    continent = "Asia";

    neighbors = [ "Middle East"; "India"; "China"; "Ural"; "Ukraine" ];

    owned = "none";

    troops = 10;

  }


let china =

  {

    tname = "China";

    continent = "Asia";

    neighbors =

      [ "Siam"; "India"; "Afghanistan"; "Ural"; "Siberia"; "Mongolia" ];

    owned = "none";

    troops = 10;

  }


let mongolia =

  {

    tname = "Mongolia";

    continent = "Asia";

    neighbors = [ "Irkutsk"; "Siberia"; "China"; "Kamchatka"; "Japan" ];

    owned = "none";

    troops = 10;

  }


let ural =

  {

    tname = "Ural";

    continent = "Asia";

    neighbors = [ "Afghanistan"; "Siberia"; "China"; "Ukraine" ];

    owned = "none";

    troops = 10;

  }


let siberia =

  {

    tname = "Siberia";

    continent = "Asia";

    neighbors = [ "Irkutsk"; "Mongolia"; "China"; "Ural"; "Yakutsk" ];

    owned = "none";

    troops = 10;

  }


let yakutsk =

  {

    tname = "Yakutsk";

    continent = "Asia";

    neighbors = [ "Siberia"; "Irkutsk"; "Kamchatka" ];

    owned = "none";

    troops = 10;

  }


let irkutsk =

  {

    tname = "Irkutsk";

    continent = "Asia";

    neighbors = [ "Siberia"; "Yakutsk"; "Mongolia"; "Kamchatka" ];

    owned = "none";

    troops = 10;

  }


let kamchatka =

  {

    tname = "Kamchatka";

    continent = "Asia";

    neighbors = [ "Yakutsk"; "Irkutsk"; "Mongolia"; "Japan" ];

    owned = "none";

    troops = 10;

  }


let japan =

  {

    tname = "Japan";

    continent = "Asia";

    neighbors = [ "Mongolia"; "Kamchatka" ];

    owned = "none";

    troops = 10;

  }


let asia =

  {

    cname = "Asia";

    territories =

      [

        india;

        middle_east;

        afghanistan;

        china;

        mongolia;

        ural;

        siberia;

        yakutsk;

        irkutsk;

        kamchatka;

        japan;

      ];

  }


(* Africa *)
let north_africa =

  {

    tname = "North Africa";

    continent = "Africa";

    neighbors =

      [

        "Egypt";

        "East Africa";

        "Congo";

        "Western Europe";

        "Southern Europe";

        "Brazil";

      ];

    owned = "none";

    troops = 10;

  }


let egypt =

  {

    tname = "Egypt";

    continent = "Africa";

    neighbors =

      [

        "North Africa"; "East Africa"; "Southern Europe"; "Middle East";

      ];

    owned = "none";

    troops = 10;

  }


let east_africa =

  {

    tname = "East Africa";

    continent = "Africa";

    neighbors =

      [

        "Egypt";

        "North Africa";

        "Congo";

        "South Africa";

        "Madagascar";

        "Middle East";

      ];

    owned = "none";

    troops = 10;

  }


let congo =

  {

    tname = "Congo";

    continent = "Africa";

    neighbors = [ "North Africa"; "East Africa"; "South Africa" ];

    owned = "none";

    troops = 10;

  }


let south_africa =

  {

    tname = "South Africa";

    continent = "Africa";

    neighbors = [ "Congo"; "East Africa"; "Madagascar"; "Japan" ];

    owned = "none";

    troops = 10;

  }


let madagascar =

  {

    tname = "Madagascar";

    continent = "Africa";

    neighbors = [ "East Africa"; "South Africa" ];

    owned = "none";

    troops = 10;

  }


let africa =

  {

    cname = "Africa";

    territories =

      [

        north_africa;

        egypt;

        east_africa;

        congo;

        south_africa;

        madagascar;

      ];

  }


(* North America *)
let central_america =

  {

    tname = "Central America";

    continent = "North America";

    neighbors = [ "Venezuela"; "Western US"; "Eastern US"; "Venezuela" ];

    owned = "none";

    troops = 0;

  }


let eastern_us =

  {

    tname = "Eastern US";

    continent = "North America";

    neighbors = [ "Central America"; "Western US"; "Quebec"; "Ontario" ];

    owned = "none";

    troops = 10;

  }


let quebec =

  {

    tname = "Quebec";

    continent = "North America";

    neighbors = [ "Greenland"; "Eastern US"; "Ontario" ];

    owned = "none";

    troops = 10;

  }


let ontario =

  {

    tname = "Ontario";

    continent = "North America";

    neighbors =

      [

        "Quebec";

        "Greenland";

        "Eastern US";

        "Western US";

        "Alberta";

        "Northwest Territory";

      ];

    owned = "none";

    troops = 10;

  }


let western_us =

  {

    tname = "Western US";

    continent = "North America";

    neighbors =

      [ "Eastern US"; "Alberta"; "Ontario"; "Central America" ];

    owned = "none";

    troops = 10;

  }


let alberta =

  {

    tname = "Alberta";

    continent = "North America";

    neighbors =

      [ "Alaska"; "Northwest Territory"; "Ontario"; "Western US" ];

    owned = "none";

    troops = 10;

  }


let alaska =

  {

    tname = "Alaska";

    continent = "North America";

    neighbors = [ "Alberta"; "Northwest Territory"; "Kamchatka" ];

    owned = "none";

    troops = 10;

  }


let greenland =

  {

    tname = "Greenland";

    continent = "North America";

    neighbors =

      [

        "Iceland"; "Northwest Territory"; "Quebec"; "Ontario"; "Iceland";

      ];

    owned = "none";

    troops = 10;

  }


let northwest_territory =

  {

    tname = "Northwest Territory";

    continent = "North America";

    neighbors = [ "Alaska"; "Greenland"; "Ontario"; "Alberta" ];

    owned = "none";

    troops = 10;

  }


let north_america =

  {

    cname = "North America";

    territories =

      [

        central_america;

        eastern_us;

        quebec;

        ontario;

        western_us;

        alberta;

        alaska;

        greenland;

        northwest_territory;

      ];

  }


(* Australia *)
let eastern_australia =

  {

    tname = "Eastern Australia";

    continent = "Australia";

    neighbors = [ "Western Australia"; "New Guinea" ];

    owned = "none";

    troops = 10;

  }


let western_australia =

  {

    tname = "Western Australia";

    continent = "Australia";

    neighbors = [ "Eastern Australia"; "Indonesia"; "New Guinea" ];

    owned = "none";

    troops = 10;

  }


let indonesia =

  {

    tname = "Indonesia";

    continent = "Australia";

    neighbors = [ "Western Australia"; "New Guinea"; "Siam" ];

    owned = "none";

    troops = 10;

  }


let new_guinea =

  {

    tname = "New Guinea";

    continent = "Australia";

    neighbors =

      [ "Eastern Australia"; "Western Australia"; "Indonesia" ];

    owned = "none";

    troops = 10;

  }


let australia =

  {

    cname = "Australia";

    territories =

      [ eastern_australia; western_australia; indonesia; new_guinea ];

  }


(* Europe *)


let ukraine =

  {

    tname = "Ukraine";

    continent = "Europe";

    neighbors =

      [

        "Scandinavia";

        "Northern Europe";

        "Southern Europe";

        "Ural";

        "Afghanistan";

        "Middle East";

      ];

    owned = "none";

    troops = 10;

  }


let scandinavia =

  {

    tname = "Scandinavia";

    continent = "Europe";

    neighbors =

      [ "Ukraine"; "Iceland"; "Northern Europe"; "Great Britain" ];

    owned = "none";

    troops = 10;

  }


let iceland =

  {

    tname = "Iceland";

    continent = "Europe";

    neighbors = [ "Scandinavia"; "Great Britain"; "Greenland" ];

    owned = "none";

    troops = 10;

  }


let great_britain =

  {

    tname = "Great Britain";

    continent = "Europe";

    neighbors = [ "Iceland"; "Northern Europe"; "Western Europe" ];

    owned = "none";

    troops = 10;

  }


let northern_europe =

  {

    tname = "Northern Europe";

    continent = "Europe";

    neighbors =

      [

        "Ukraine";

        "Scandinavia";

        "Great Britain";

        "Western Europe";

        "Southern Europe";

      ];

    owned = "none";

    troops = 10;

  }


let western_europe =

  {

    tname = "Western Europe";

    continent = "Europe";

    neighbors =

      [

        "Great Britain";

        "Northern Europe";

        "Southern Europe";

        "North Africa";

      ];

    owned = "none";

    troops = 10;

  }


let southern_europe =

  {

    tname = "Southern Europe";

    continent = "Europe";

    neighbors =

      [

        "Ukraine";

        "Northern Europe";

        "Western Europe";

        "North Africa";

        "Egypt";

      ];

    owned = "none";

    troops = 10;

  }


let europe =

  {

    cname = "Europe";

    territories =

      [

        iceland;

        scandinavia;

        great_britain;

        northern_europe;

        ukraine;

        western_europe;

        southern_europe;

      ];

  }


let players = [ player1; player2; player3 ]


let gameboard =

  {

    continents =

      [ south_america; africa; north_america; asia; australia; europe ];

    players;

  }


(* Next few functions pretty prints game board *)


let pp_list lst =
  let pp_elts lst =
    let rec loop acc = function
      | [] -> acc
      | [ h ] -> acc ^ h
      | h1 :: (h2 :: t as t') -> loop (acc ^ h1 ^ "; ") t'
    in
    loop "" lst
  in
  "[" ^ pp_elts lst ^ "]"


let string_territory { tname; continent; neighbors; owned; troops } =

  tname ^ ": Owned by " ^ owned ^ " with " ^ string_of_int troops

  ^ " troops\n\t\tNeighbors: " ^ pp_list neighbors ^ "\n\t"


let string_continent { cname; territories } =

  let rec loop acc = function
    | [] -> ""

    | [ h ] -> acc ^ string_territory h

    | h1 :: (h2 :: t1 as t') -> loop (acc ^ string_territory h1) t'

  in

  loop "" territories


let string_gameboard { continents; players } =

  let rec loop acc = function

    | [] -> ""

    | [ h ] -> acc ^ "\n" ^ h.cname ^ "\n\t" ^ string_continent h

    | h1 :: (h2 :: t1 as t') ->

        loop (acc ^ "\n" ^ h1.cname ^ "\n\t" ^ string_continent h1) t'

  in

  loop "" continents


let print_game gameboard = print_string (string_gameboard gameboard)


(* Keeps track of territories owned by a player *)

(* let player_owns_cont player {cname; territories} = let rec loop acc =

   function | [] -> acc | h :: t -> if h.owned = player then loop (h ::

   acc) t else loop acc t in loop [] territories


   let player_owns_board player {continents; players} = let rec loop acc

   = function | [] -> acc | h :: t -> player_owns_cont player h in loop

   [] continents *)


(* Keeps track of territories owned by a player using type - (cont_id,

   territory list) list - where cont_id represents the name of a

   continent and the territory list represents the list of territories

   the player owns within that continent. *)

let player_owns_cont player { cname; territories } =

  let rec loop acc = function

    | [] -> acc

    | h :: t ->

        if h.owned = player then loop (h :: acc) t else loop acc t

  in

  (cname, loop [] territories)


let rec player_owns_board player { continents; players } =

  match continents with
  | [] -> []
  | h :: t ->

      player_owns_cont player h

      :: player_owns_board player { continents = t; players }


let rec cont_terrs ((id : cont_id), (tlist : territory list)) =
  match tlist with
  | [] -> []
  | { tname; continent; neighbors; owned; troops } :: t ->

      tname :: cont_terrs (id, t)


let player_terrs gboard player =
  let map_terrs = player_owns_board player gboard in
  let rec loop = function
    | [] -> []
    | h :: t -> cont_terrs h @ loop t
  in

  loop map_terrs


(* Function takes in a player and returns the number of territories

   owned by that player *)

let rec num_territories player =
  let player_list = player_owns_board player gameboard in
  let rec loop = function
    | [] -> 0

    | (c, tlist) :: t -> List.length tlist + loop t

  in

  loop player_list


(* Next couple functions pretty prints the territories a specific player

   owns in the same format as the gameboard. *)

let string_player_cont lst =

  let rec loop acc = function

    | [] -> ""
    | [ h ] -> acc ^ string_territory h
    | h1 :: (h2 :: t1 as t') -> loop (acc ^ string_territory h1) t'
  in

  loop "" lst


let string_player_board player { continents; players } =

  let player_list = player_owns_board player { continents; players } in

  let rec loop acc = function

    | [] -> ""

    | [ (c, tlist) ] ->

        acc ^ "\n" ^ c ^ "\n\t" ^ string_player_cont tlist

    | h1 :: (h2 :: t1 as t') ->

        loop

          (acc ^ "\n" ^ fst h1 ^ "\n\t" ^ string_player_cont (snd h1))

          t'

  in

  loop "" player_list


let print_player1 = print_string (string_player_board player1 gameboard)


let print_player2 = print_string (string_player_board player2 gameboard)


let print_player3 = print_string (string_player_board player3 gameboard)


(**Army stuff*)


(*This function will tell us how many armies each player should have,

  based on the number of players. This takes in the # of players and

  returns army # For example, 3 players -> 35 infantry each 4 players ->

  30 infantry each etc.. *)

let player_to_army t : int = 20 + (t * 5)


(* This function will create the armies need in the game, dependent on
   player count *)


(* let army_creator t = let g = player_to_army t in Infantry g *)


(** This will be an updating funcion, so that everytime a turn ends,
    call this function to increment armies. Army increment is based on

    the amount of territories. Also the continents controlled should be

    included to see if and extra armies are included*)


(* let helper_inc (f : troops)= match f with |Infantry b -> b *)


(* let inc_armies (terr : int) cont (armies : troops) = We need to

   implement how continents work before I can go further let b =

   helper_inc armies in let g = army_creator ((b + terr) / 3 ) *)


(** This will be an updating function, so that every time a turn ends,
    call this function to increment armies. Army increment is based on

    the amount of territories. Also the continents controlled should be

    included to see if and extra armies are included*)


let rec tests_if_player_control

    (player : player_id)

    (lst : territory list) =

  match lst with

  | [] -> false

  | h :: t -> h.owned = player && tests_if_player_control player t


let cont_players (cont : continent) player =

  let b = cont.territories in
  tests_if_player_control player b


let rec total_cont player = function
  | [] -> []

  | h :: t ->

      if cont_players h player then h :: total_cont player t

      else total_cont player t


let name_to_cont (elt : cont_id) =
  match elt with
  | "Asia" -> 7

  | "Europe" -> 5

  | "North America" -> 5

  | "South America" -> 3

  | "Africa" -> 3

  | "Australia" -> 2

  | _ -> 0

(* raise exception later *)


let rec bonus_counter = function
  | [] -> 0

  | h :: t -> name_to_cont h.cname + bonus_counter t


let inc_armies (terr : int) (cont : continent list) player =

  let s = total_cont player cont in

  (*this is a list of all controlled continents*)

  let bonus = bonus_counter s in
  (terr / 3) + bonus

(* For the future, figure out how to keep track of the territories a

   player owns *)


let rec board_territories { continents; players } =

  match continents with
  | [] -> []
  | h :: t ->

      h.territories @ board_territories { continents = t; players }


let map_territories gboard =
  let rec loop lst =
    match lst with
    | [] -> []
    | h :: t -> (h.tname, h) :: loop t
  in

  loop (board_territories gboard)


let rec lookup k = function
  | [] -> failwith "Not a territory"

  | (k', v) :: t -> if k = k' then v else lookup k t


let add_troops gboard (terr : terr_id) (tr : troops) =
  (* let map_gboard = map_territories gboard in *)
  (* match lookup terr map_gboard with | {tname; continent; neighbors;

     owned; troops} -> {tname; continent; neighbors; owned; troops =

     troops + tr} | terr -> terr.troops := terr.troops + tr *)

  (* for i = 0 to List.length map_gboard do let terr_add = lookup terr

     map_gboard in if snd (List.nth map_gboard i) = terr_add then

     terr_add.troops <- terr_add.troops + tr done; gboard *)

  (* let cont_list = gboard.continents in *)

  for i = 0 to List.length gboard.continents - 1 do

    let terr_list = (List.nth gboard.continents i).territories in

    for j = 0 to List.length terr_list - 1 do

      let curr_terr = List.nth terr_list j in

      if curr_terr.tname = terr then

        (List.nth (List.nth gboard.continents i).territories j).troops <-

          (List.nth (List.nth gboard.continents i).territories j).troops

          + tr

    done

  done;
  gboard


(** TODO: creating all of the territories as variables. Bunch of let
    statements - territories never change. *)


(** TODO: function that takes in a string name and matches it to its
    territory.*)


(** TODO: function that takes in a string name and outputs the

    neighboring territories of that territory.*)


let rec get_territory name lst =
  match lst with
  | [] ->

      {

        tname = "Nowhere";

        continent = "";

        neighbors = [];

        owned = "";

        troops = 0;

      }

  | h :: t -> if h.tname = name then h else get_territory name t


let get_continent terr = terr.continent


let get_neighbors terr = terr.neighbors
(* let rec convert lst = match lst with | [] -> [] | h :: t -> h.tname

   :: convert t in convert terr.neighbors *)


let is_neighbor t1 t2 lst =
  let rec search name lst =

    match lst with
    | [] -> false
    | h :: t -> if h = name then true else search name t
  in

  search t2 (get_territory t1 lst |> get_neighbors)


let p1t = ref 0


let p2t = ref 0


let p3t = ref 0


let incr_p1 () =

  p1t := !p1t + 1;
  ()


let decr_p1 () =

  p1t := !p1t - 1;
  ()


let incr_p2 () =

  p2t := !p2t + 1;
  ()


let decr_p2 () =

  p2t := !p2t - 1;
  ()


let incr_p3 () =

  p3t := !p3t + 1;
  ()


let decr_p3 () =

  p3t := !p3t - 1;
  ()

let balance =

  while !p1t - 18 > 0 do

    while !p2t - 18 < 0 do

      incr_p2 ();

      decr_p1 ()

    done;

    while !p3t - 18 > 0 do

      incr_p3 ();

      decr_p1 ()

    done

  done;
  while !p2t - 18 > 0 do

    while !p1t - 18 < 0 do

      incr_p1 ();

      decr_p2 ()

    done;

    while !p3t - 18 > 0 do

      incr_p3 ();

      decr_p2 ()

    done

  done;

  while !p3t - 18 > 0 do

    while !p2t - 18 < 0 do

      incr_p2 ();

      decr_p3 ()

    done;

    while !p1t - 18 > 0 do

      incr_p1 ();

      decr_p3 ()

    done

  done


let player_maker (num : int) =

  match num with

  | 1 -> [ player1 ]

  | 2 -> [ player1; player2 ]

  | 3 -> [ player1; player2; player3 ]

  | _ -> []


let random_terr_gen (lst : player_id list) =

  let b = Random.int (List.length lst) in
  List.nth lst b

let rec terr_list (cont_list : continent list) =

  match cont_list with

  | [] -> []

  | h :: t -> h.territories :: terr_list t


let rec rand_terr terr_list =

  let b = Random.int (List.length terr_list) in

  if (List.nth terr_list b).owned = "none" then List.nth terr_list b

  else rand_terr terr_list


let rec checker = function

  | [] -> true

  | h :: t when h.owned = "none" -> false && checker t

  | h :: t when h.owned <> "none" -> true && checker t

  | _ -> false

let rec f g = function

  | [] -> []

  | h :: t when h = g -> f g t

  | h :: t when h <> g -> h :: f g t

  | _ -> []

let match_player_to_inc (player : player_id) =

  match player with

  | player1 -> incr_p1 ()
  | player2 -> incr_p2 ()
  | player3 -> incr_p3 ()
  | _ -> ()


let rec assign_terrs terrs_list lst t =
  let g = random_terr_gen lst in

  let f = rand_terr terrs_list in
  f.owned <- g;
  match_player_to_inc g;
  if checker terrs_list then () else assign_terrs (t terrs_list) lst t


let rec split_terr (num_player : int) =

  let b = player_maker num_player in
  let j =

    List.flatten

      (terr_list

         [

           africa; asia; europe; north_america; south_america; australia;

         ])

  in

  assign_terrs j b

let alpha =

  let j = split_terr 3 in
  balance

let random_player_assign =
  let lst = [ player1; player2; player3 ] in

  let f = Random.int (List.length lst) in

  List.nth lst f


let initialization gboard =
  for i = 0 to List.length gboard.continents - 1 do
    let terr_list = (List.nth gboard.continents i).territories in

    for j = 0 to List.length terr_list - 1 do
      (List.nth (List.nth gboard.continents i).territories j).owned <-

        List.nth players (Random.int (List.length players))

    done

  done;
  balance;

  print_string (string_gameboard gboard);

  gboard


let rec set_playerorder nameslst acc = 
  if List.length nameslst = List.length acc then 
    let rec loop i = function
    | [] -> "\n\nPlayer 1's turn will now begin... \n\n\n"
    | h :: [] -> "Player " ^ string_of_int i ^ ": " ^ h ^ loop (i + 1) []
    | h :: t -> "Player " ^ string_of_int i ^ ": " ^ h ^ "; "
      ^ loop (i + 1) t
    in "\nHere is your turn order - \n" ^ (loop 1 acc) else
    let ran = Random.self_init (); Random.int (List.length nameslst) in
    let elt = List.nth nameslst ran in
    if List.mem elt acc then set_playerorder nameslst acc else
        set_playerorder nameslst (acc @ [elt])

let get_troops terr = terr.troops
 
let rec rolls terr_from terr_to =
  let dice_attack = if terr_from.troops = 2 then 1
  else if terr_from.troops = 3 then 2 else 3 in
  let dice_defend = if terr_to.troops = 1 then 1 else 2 in
  let attack_rolls = init_rolls dice_attack |> List.sort Int.compare |> List.rev in
  let defend_rolls = init_rolls dice_defend |> List.sort Int.compare
    |> List.rev in
  ((dice_attack, attack_rolls), (dice_defend, defend_rolls))
 
and init_rolls = function
  | 0 -> []
  | i -> Random.self_init (); (Random.int (6) + 1) :: init_rolls (i-1)
 
let rec attack gboard terr_from terr_to =
  let turn_rolls = rolls terr_from terr_to in
  let attack_rolls = snd (fst turn_rolls) in
  let score_rolls = if List.length attack_rolls = 2 then List.rev attack_rolls
  else List.tl attack_rolls |> List.rev in
  let rec loop at def =
    match at, def with
    | [], [] -> gboard
    | h1 :: t1, h2 :: t2 -> if h1 > h2 then add_troops gboard terr_to.tname ~-1
      else add_troops gboard terr_from.tname ~-1; loop t1 t2
    | _ -> failwith "shouldn't happen"
  in loop score_rolls (snd (snd turn_rolls))



  (* Write specifications for functions that we are keeping
  Remember to write AF and RI for gameboard
  
  Json_to_board function
  Takes in a json structure and turns it into a gameboard
  See a2 for help
  
  Initialization
  Complete initialization for a gameboard
  This means returning a gameboard where player assignments to territories are random. We can assume that this is a three-player game for now. Players’ total number of troops must be the same, but the number on each territory can be different
  This should not need to use refs or any mutable programming. The function doesn’t mutate a gameboard, it just returns a randomized gameboard
  
  Troops_earned
  Takes in a player and a gameboard and Returns an int representing how many troops the player gets for his deploy phase
  1 troop for every three territories plus any continent bonuses
  
  At end: remove functions
  This is important because clarkson said if you leave unused code in your program he might consider it an AI violation of trying to inflate LOC
   *)