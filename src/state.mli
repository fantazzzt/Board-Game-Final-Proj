(** Representation of dynamic gameboard state (in the deploy phase).

    This module represents the state of the gameboard as the risk game
    is being played, including whose turn it is, the territories he owns
    and the number of troops on them, the amount of troops a player has
    to deploy, and functions that cause the state to change. *)

type gen_state

module type Queryable = sig
  type t
  (** The abstract type of values representing the game state. *)

  (*The player whose turn it is*)
  val whose_turn : t -> Board.player_id

  val terrs_owned : t -> Board.terr_id list
  (** The territories owned by the player whose turn it is*)

  val terr_troops : t -> Board.terr_id -> Board.troops
  (**Number of troops on a certain territory*)

  val get_gameboard : t -> Board.gameboard
  (**TODO: write description*)

  val return_gen_st : t -> gen_state
  (**TODO: write description*)
end

module type DeploySig = sig
  type t
  (** The abstract type of values representing the game state. *)

  (** The type representing the result of an attempted deployment. *)
  type deploy_result =
    | Legal of t
    | NoTroops
    | TooMany
    | NotTerr

  val init_start : t
  (**TODO: describe init_start*)

  val init_deploy : gen_state -> t
  (**TODO: describe init_deploy*)

  val troops_2_play : t -> Board.troops
  (**Number of troops a player has left to deploy*)

  val deploy : t -> Board.terr_id -> string -> deploy_result
  (**Deploys quantity [trps] of troops to territory [terr] owned by
     player [plr]*)

  include Queryable with type t := t
end

module DeploySt : DeploySig

module type AttackSig = sig
  type t
  (** The abstract type of values representing the game state. *)

  val init_attack : gen_state -> t
  (**TODO*)

  (** The type representing the result of an attempted deployment. *)
  type attack_result = Legal of t

  include Queryable with type t := t
end

module AttackSt : AttackSig