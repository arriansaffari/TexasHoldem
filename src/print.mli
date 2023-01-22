(** The print module includes all the important printing functions. This 
    includes a function to pretty print a player's info, the community cards, 
    and the current state. *)

open Card
open Game

val print_player_info : player -> state -> unit
(**[print_player_info player mc] prints the info of the player. If the player is
   a computer AI, it will only print the money(input [mc=false]). If the player
   is the main player, it prints the money and the cards(input [input=true])*)

val print_players : player list -> state -> unit
(** [print_ais computers_ai] prints the info of all the ai in the game, which as
    of now is the amount of money they have *)

val print_community_cards : community_card list -> unit
(**[print_community_cards ccards] prints the 5 community cards. If a community
   card is face up then it prints the cards value and suit and if the card is
   face down it prints [face down]*)

val print_bet : player -> int -> unit
(**[print_bet p n] prints that player p made a bet of n dollars*)

val print_state : state -> unit
(** [print_state state] prints the current state of the game [state]. It 
    prints the active players still left in the game, their money and last
    raise, as well as the pool, and the main player's cards. *)

val print_end : state -> unit
(** [print_end state] prints the state of the game [state] once the game has been
    completed and there is a winner. It shows the cards of all of the active 
    players, and prints the winner. *)