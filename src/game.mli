(** The game file implements the round functionality of the poker game. It 
    creates the state representing the poker game(including the players, 
    pool money, community cards, etc..) and the function needed to modify it 
    throughout the game. This includes function that implements flipping the 
    community cards, the AI for the computer and more. All the functions from 
    this file are combined in bin/main.ml allowing for a fully functioning poker
    game.  *)

open Card

type player
(** The type [player] represents a player in the poker game. Each player has 
a name represented by a string, a card list made up of two cards, money represented
by an int, and a last bet, which is the most recent amount of money that player 
has bet. Representation invariant: money and last_bet must be a natural number.

For example: 
- If a player p is defined as [{name = "Steven"; cards = [Spades (3); Hearts (10)]; 
money = 500; last_bet = 50}], p represents a person named Steven playing in the poker game
who has a Three of Spades and a Ten of Hearts, 500 dollars left, and whose last bet 
was $50. *)

type game_state
(** The type [game_state] represents the state of a specific poker game. A gamestate
contains a list of active players, the person to last raise a bet, the pool of money,
the current bet, and the dealer. *)

type state
(** The type [state] represents the state of many poker games. A state
contains a list of the community cards, the main_player (user), the computers playing,
the game_state, and the difficulty of the game. *)

type action
(** The type [action] represents an action that a player can make during their turn. 
    Actions can be Raise (n) where n is an integer, Call, or Fold. *)

exception No_Input
(** Raised when an empty input is parsed. *)

exception Invalid_Input
(** Raised when an invalid command is parsed. *)

exception No_Players
(** Raised when there are no active players left. *)

val initiate : int -> int -> int -> state
(**[initiate n deck money] initiate a state object with n players who have cards
   in deck [deck] and have starting money [money] *)

val get_cards : player -> card list
(**[get_cards player] returns the card list of the player*)

val get_money : player -> int
(**[get_money player] returns the amount of money the player has*)

val get_last_bet : player -> int
(**[get_last_bet p] returns the last_bet field of player p *)

val get_main_player : state -> player
(** [get_main_player s] returns the main_player field of the state s*)

val get_actives : state -> player list
(** [get_actives s] returns the active players of the state s*)

val get_community_cards : state -> community_card list
(** [get_community_cards s] returns the community_cards field of the state s*)

val get_name : player -> string
(** [get_name p] returns the string name of the player *)

val change_actives : state -> player list -> game_state
(** [change_gs gs actives'] returns a new game_state with the new actives
   actives'*)

val change_gs : state -> game_state -> state
(** [change_gs st gs'] returns a new state with the new game_state gs'*)

val update_list : player list -> player -> player -> player list
(** [update_list lst p n] takes in a list of players, and returns an identical
   list of players, only with player p having n less money *)

val if_mp : state -> player -> bool
(** [if_mp st p] returns a boolean: true if p is the main player of the state st,
   false if not *)

val parse_action : string -> state -> player -> action
(**[parse_action str] takes the string and returns an action(call, raise or
   fold) or an excpetion if the input string is an invalid input*)

val get_action : unit -> state -> player -> bool -> action
(**[get_action ()] returns a valid action no matter what input*)

val first_round : bool -> state -> state
(**[get_action b s] returns a state with the first_round done. Bool b is true
    if the blinds have been done or not. *)

val next_round : bool -> state -> state
(**[next_round b s] returns a state with an additional round having been completed. Bool b is 
    whether or not the first bet has been completed*)

val reset_bet : state -> state
(**[reset_bet s] returns a new state that is adapted from state s. The changes made are 
    that the last_bets of all of the players are reset to zero and the current bet 
    is reset to zero as well.*)

val flop_done : state -> bool
(**[flop_done s] returns a boolean which depends on whether or not the flop cards have
    been flipped or not. If the first three community cards are flipped, flop_done s
    returns true, otherwise false*)

val turn_done : state -> bool
(**[turn_done s] returns a boolean which depends on whether or not the turn card has
    been flipped or not. If the fourth community cards is flipped, flop_done s
    returns true, otherwise false*)

val river_done : state -> bool
(**[river_done s] returns a boolean which depends on whether or not the river card has
    been flipped or not. If the fifth community cards is flipped, flop_done s
    returns true, otherwise false*)

val show_flop : state -> state
(**[show_flop s] returns an altered state of s where the first three community cards
    are now flipped over, enabling the user to view them. *)

val show_turn : state -> state
(**[show_turn s] returns an altered state of s where the fourth community card
    is now flipped over, enabling the user to view it. *)

val show_river : state -> state
(**[show_river s] returns an altered state of s where the fifth community card
    is now flipped over, enabling the user to view it. *)

val get_dealer : state -> player
(**[get_dealer s] returns the dealer of the state of s *)

val get_players : state -> player list
(**[get_players s] returns a list of active players of the state of s *)

val get_card_list : state -> card list list
(**[get_card_list s] returns a list of a list of cards of all of the active
    players left in state s. *)

val find_name : player list -> card list -> string
(**[find_name plst clist] returns the name of the person in plist
    who's cards are equal to clist. *)

val get_pool : state -> int
(**[get_pool st] returns the value of the pool of state s *)

val empty_pool : state -> player list -> state
val find_player : player list -> card list -> player