open Board

type gen_state = {
  players : Board.player_id list;
  current_turn : Board.player_id;
  gameboard : Board.gameboard;
}

module type Queryable = sig
  type t

  val whose_turn : t -> Board.player_id

  val terrs_owned : t -> Board.terr_id list

  val terr_troops : t -> Board.terr_id -> Board.troops

  val get_gameboard : t -> Board.gameboard

  val return_gen_st : t -> gen_state
end

module type DeploySig = sig
  type t

  type deploy_result =
    | Legal of t
    | NoTroops
    | TooMany
    | NotTerr

  val init_start : t

  val init_deploy : gen_state -> t

  val troops_2_play : t -> Board.troops

  val deploy : t -> Board.terr_id -> string -> deploy_result

  include Queryable with type t := t
end

module DeploySt : DeploySig = struct
  type t = {
    gen_st : gen_state;
    troops_available : Board.troops;
  }
  (** The abstract type of values representing the game state druing the
      deploy phase. TODO: write AF and RI*)

  (** The type representing the result of an attempted deployment. *)
  type deploy_result =
    | Legal of t
    | NoTroops
    | TooMany
    | NotTerr

  let init_start =
    {
      gen_st =
        {
          players = Board.players;
          current_turn = Board.player1;
          gameboard = Board.initialization Board.gameboard;
        };
      troops_available =
        10
        (*Note: want to replace this with function calculating number of
          troops available*);
    }

  let init_deploy gst =
    {
      gen_st = gst;
      troops_available =
        10
        (*Note: want to replace this with function calculating number of
          troops available*);
    }

  (*The player whose turn it is*)
  let whose_turn st = st.gen_st.current_turn

  (** A terr_id list of the territories owned by the current player*)
  let terrs_owned st =
    Board.player_terrs st.gen_st.gameboard st.gen_st.current_turn

  let get_gameboard brdst = brdst.gen_st.gameboard

  let return_gen_st st = st.gen_st

  (*Number of troops a player has left to deploy*)
  let troops_2_play st = st.troops_available

  (*Number of troops on a certain territory*)
  let terr_troops terr = raise (Failure "Unimplemented")

  (**Deploys quantity [trps] of troops to territory [terr] owned by the
    current player*)
  let deploy (st : t) terr (trps_str : string) =
    try
      if
        (*[trps] is greater than troops available to deploy*)
        troops_2_play st < int_of_string trps_str
      then TooMany
      else if
        (*[terr] is the terr_id of a territory owned by [plr]*)
        List.exists (fun terr_id -> terr_id = terr) (terrs_owned st)
        (*Would List.find be better?*)
      then
        (*deploy the troops, aka update state*)
        Legal
          {
            gen_st =
              {
                players = Board.players;
                current_turn = st.gen_st.current_turn;
                gameboard =
                  Board.add_troops st.gen_st.gameboard terr
                    (int_of_string trps_str)
                  (*Do we have this function or do we need to implement
                    it?*);
              };
            troops_available =
              st.troops_available - int_of_string trps_str;
          }
      else NotTerr
    with
    | Failure s -> NoTroops
  (*Number of troops not specified *)
end

module type AttackSig = sig
  type t
  (** The abstract type of values representing the game state. *)
  val init_attack : gen_state -> t

  (** The type representing the result of an attempted deployment. *)
  type attack_result = Legal of t

  include Queryable with type t := t
end

module AttackSt : AttackSig = struct
  type t = {
    gen_st : gen_state
  }
  (** The abstract type of values representing the game state. *)

  let init_attack gst = {
    gen_st = gst
  }

  (** The type representing the result of an attempted deployment. *)
  type attack_result = Legal of t

  (*The player whose turn it is*)
  let whose_turn st = st.gen_st.current_turn

  (** A terr_id list of the territories owned by the current player*)
  let terrs_owned st =
    Board.player_terrs st.gen_st.gameboard st.gen_st.current_turn

  let get_gameboard brdst = brdst.gen_st.gameboard

  let return_gen_st st = st.gen_st

  (*Number of troops on a certain territory*)
  let terr_troops terr = raise (Failure "Unimplemented")
end
