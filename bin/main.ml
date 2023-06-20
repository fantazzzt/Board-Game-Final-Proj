open Game
(** [play_game f] starts the adventure in file [f]. *)
 
open Board
open Command
open State

let ansired_plus_func f plr brdst str = 
  ANSITerminal.print_string [ ANSITerminal.red ] str;
  f plr brdst

let bug_statement f = "There is a exception that is not one of the expected \
 exceptions in" ^ f ^ ". There is most likely a bug\n"
 
let rec play_deploy plr brdst =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("\n" ^ plr ^ ": Deploy Stage: Choose a territory to deploy troops. \
    You have " ^ string_of_int (troops_2play brdst) ^ " troops left to deploy.\
    To view the gameboard, type view board. To place \
    troops on a territory, write deploy, then the name of the territory and \
    then the number of troops that you wish to place. For example: \
    deploy North Africa 1.\n");
  print_string "\n> ";
  let rec statements brdst =
    (*Note: made fix to match part, want to check with others to see if it works*)
    match (try Command.parse_deploy (read_line ()) with
    | Empty ->
       ANSITerminal.print_string [ ANSITerminal.red ] "The input was empty, please type a valid input\n";
       TryAgain
    | Malformed ->
       ANSITerminal.print_string [ ANSITerminal.red ] "Did not type (View board) or (Deploy __), please type one of these\n";
       TryAgain
    | Failure e -> ANSITerminal.print_string [ ANSITerminal.red ]
       e (*(bug_statement "Command.parse")*);
       TryAgain)
  with
    | Quit ->
      ANSITerminal.print_string [ ANSITerminal.red ]
      "\nAdios, hope you had a good time playing!\n\n";
      exit 0
    | TryAgain -> play_deploy plr brdst
    | ViewBoard ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
      (Board.string_gameboard (State.get_gameboard brdst));
      play_deploy plr brdst
    | View p ->
      let player = String.trim (List.fold_left (fun acc s -> acc ^ " " ^ (String.capitalize_ascii s)) "" p) in
      ANSITerminal.print_string [ ANSITerminal.yellow ]
      (Board.string_player_board player (State.get_gameboard brdst));
      play_deploy plr brdst
    | Deploy parsedlst -> 
      if List.length parsedlst < 2 then
         ansired_plus_func play_deploy plr brdst 
         "Either did not specify a territory or did not specify how many troops to deploy, please enter an integer.\n"
      else
        let
          parsedterr, trps_str = 
          List.(fold_left (fun acc s -> acc ^ " " ^ (String.capitalize_ascii s))
          (String.capitalize_ascii (hd parsedlst)) 
          (parsedlst |> rev |> tl |> rev |> tl)), 
          List.(parsedlst |> rev |> hd)
        in
      match
          State.deploy brdst (parsedterr) (trps_str)
      with
      | NoTroops ->
          ANSITerminal.print_string [ ANSITerminal.red ]
          "Did not specify how many troops to deploy, please enter an integer.\n";
          play_deploy plr brdst
      | TooMany ->
          ANSITerminal.print_string [ ANSITerminal.red ]
          "You tried to place more troops than you have left to deploy, please enter a smaller integer.\n";
          play_deploy plr brdst
      | NotTerr ->
          ANSITerminal.print_string [ ANSITerminal.red ]
          "The territory you specified either does not exist or is not owned by you, please enter 1 of your territories.\n";
          play_deploy plr brdst
      | Legal st -> 
        ANSITerminal.print_string [ ANSITerminal.blue ]
        ("Deployed " ^ trps_str ^ " troop(s) to " ^ parsedterr ^ ".\n");
          (*For MS3: too many nested matches here, make below an if...else statement*)
          match State.troops_2play st with
          | 0 ->
            ANSITerminal.print_string [ ANSITerminal.blue ]
            "You have no more troops left to deploy. It is now time for your Attack phase.\n";
            exit 0
          | _ -> play_deploy plr st
  in statements brdst
 
  let rec play_attack plr brdst =   raise (Failure "Unimplemented")
   (* ANSITerminal.print_string [ ANSITerminal.green ]
    ("\n" ^ plr ^ ": Attack Stage: Do you want to attack? Please type 'Yes'
    or 'No' \n");
  print_string "\n> ";
  match (try Command.parse_beforeattack (read_line ()) with
    |Empty ->
    |Malformed ->
    |
  with
   |ViewBoard -> (*same as deploy*)
   |View p -> (* same as deploy *)
   |Fortify -> (* go to next player and go to deploy for that player *)
   |Yes ->
    ANSITerminal.print_string [ ANSITerminal.green ]
    ("\n" ^ plr ^ ": What territory would you like to attack from? \n");
    print_string "\n> ";
    let terr_from = Command.parse(read_line ()) in
    match terr_from with
    | Empty
    | NotOwned -> (* Enter another territory*)
    | NotEnoughTroops -> (* Choose a different territory or pass*)
    | Territory t -> (* Get territory to bind to variable*)
    | _ -> (* Incorrect input - can add something else*)
    ANSITerminal.print_string [ ANSITerminal.green ]
    ("\n" ^ plr ^ ": What territory would you like to attack? \n");
    print_string "\n> ";
    let terr_attack = Command.parse(read_line()) in
    match terr_attack with
    |
    (*recursive loop here*)
    ANSITerminal.print_string [ ANSITerminal.green ]
    ("\n" ^ plr ^ ": You are currently attacking [concatenate the territory].
    If you don't want to attack this territory type 'Back', and if you do, 
    type 'Go'? \n");
    print_string "\n> ";
    let go = Command.parseTerr(read_line()) in
    match go with
    | Back -> (* call play_attack *)
    | Go -> (* called Board.attack and then recurse through inner loop*)
                  (* If the number of troops in territory attacking is 0, then
                    change the boardstate*)
                  match State.attack t_from t_attack num_dice with
                  | OutOfTroops st -> (* have 1 troop left*)
                  | Attack st -> ANSITerminal.print_string [ ANSITerminal.green ]
                  ("\n The attacker rolled " ^ string_of_int (fst (fst State.get_rolls st)) ^ 
                  " dice with the rolls: " ^ Board.pp_list snd (fst (State.get_rolls st)) ^ 
                  ". The defender rolled " ^ string_of_int (fst (snd State.get_rolls)) ^ 
                  " dice with rolls: " ^ Board.pp_list snd (fst (State.get_rolls st)) ^ "\n");
                  | TookOver st -> territory attacking has 0 troops  *)

 
 
let rec play_fortify plr brdst = raise (Failure "Unimplemented")
 
(** [play_turn brdst plr] starts player [plr]'s turn on board [gbrd] at state
 [brdst] and terminates when the game ends. *)
let rec play_turn plr brdst =
  play_turn (plr (*needs to change in MS2*)) (brdst
  |> play_deploy plr
  |> play_attack plr
  |> play_fortify plr)
 
 
let data_dir_prefix = "data" ^ Filename.dir_sep

let ansired_main f str =
  ANSITerminal.print_string [ ANSITerminal.red ] str; f
 
(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
  "\n\nWhat's up Ze-Wen, welcome to our first Risk™ (partial) game demo.\n";
  let rec mainloop () =
    ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWrite the names of the (three) players playing.
    Each player’s name must only be one word long, and player’s names are separated
    by a space. There must be exactly 3 players (for this stage in the game).\n";
    print_string "\n> ";
    try (let nameslst = Command.parse_names (read_line ())
      in
        (* For now: the print stamement with   prints: "Here is your turn order:
      Player 1: <name1> ; Player 2: <name2> Player 3: <name3>.\n
      Player 1's turn will now begin... \n\n\n" *)
        ANSITerminal.print_string [ ANSITerminal.green ]
        (Board.set_playerorder (nameslst) []);
        play_turn (State.whose_turn State.demo_st) State.demo_st) with
    | Malformed -> ANSITerminal.print_string [ ANSITerminal.red ]
    "\nMake sure you write exaclty THREE players' names\n"; mainloop ()
    | Empty -> ANSITerminal.print_string [ ANSITerminal.red ]
    "\nThe input was empty, please type a valid input\n"; mainloop ()
    | _ -> ANSITerminal.print_string [ ANSITerminal.red ]
      (bug_statement "parse_names"); mainloop ()
  in
    mainloop ()
 









(* let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
"\n\nWhat's up Ze-Wen, welcome to our first Risk™ (partial) game demo.\n";
ANSITerminal.print_string [ ANSITerminal.green ]
"\nWrite the names of the (three) players playing. 
Each player’s name must only be one word long, and player’s names are separated 
by a space. There must be exactly 3 players (for this stage in the game).\n";

  play_turn (State.whose_turn (State.init_st)) (State.init_st) *)
(* Initializing a game:
  Idea 1: Printing: “Write the names of the (three) players playing. 
  Each player’s name must only be one word long, and player’s names are separated by a space. 
  There must be between 2 and n players.”
  We then randomize the order of the players, and print:
  “Here is your turn order: Player 1: <name1> ; Player 2: <name2> … . 
  To initialize the board and begin Player 1’s turn, type start.”
  We then randomly assign territories to players
  The total number of troops on the map at the start is the same for each player,
  Just have to decide how to distribute troops across territories *)
  
  (*print_endline
"Please enter the name of the game file you want to load.\n";
print_string "> ";
match read_line () with
| exception End_of_file -> ()
| file_name -> play_game (data_dir_prefix ^ file_name)*)
 
(* Execute the game engine. *)
let () = main ()
