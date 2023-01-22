(** The card module includes all the functionality for drawing cards. This goes
    all the way from defining what a card is to functions to pretty print the 
    cards and functions to rank different poker hands. *)

type card = Spades of int | Hearts of int | Clubs of int | Diamonds of int

(** type card definition, where the constructor is the suit and the number is the 
    number of the card  *)

type community_card = { ccard : card; face_up : bool }
(** community_card represents a community card where ccard is the actual card
    and face_up is whether the card is flipped or not(ie. whether we can see it
    or not) *)

val is_face_up : community_card -> bool
(** [is_face_up community_card] returns whether the community card is face up or
    face down(true and false respectively)*)

val get_ccard : community_card -> card
(** [get_ccard community_card] returns the [ccard] field of the [community_card]
    type*)

val flip_card : community_card -> community_card
(** [flip_card community_card] changes the [face_up] field of the [community_card]
    type to true*)

val create_com_card : card -> bool -> community_card
(** [create_com_card cc fu] creates a community card with fields ccard=cc and
    face_up=fu *)

val get_card_value_string : card -> string
(**[get_card_value_string c] returns the string value of the card (ie. for input
   [Spades 3], it returns returns ["Three"])*)

val get_card_value : card -> int
(**[get_card_value single_card] returns a numerical value for the value of the
   card. It returns a higher value for cards that are higher ranked *)

val get_card_suit : card -> string
(**[get_card_suit c] returns the string suit of the card (ie. for input
  [Spades 3], it returns returns ["Spade"])*)

val get_string_card : card -> string
(**[get_string_card c] returns the string value of the card (ie. for input
  [Spades 3], it returns returns ["Three Spade"])*)

val get_icon_card : card -> string
(**[get_icon_card c] returns the "card-like image" of the card (ie. for input
    [Spades 3], it returns returns ["┌─────────┐\n│3        │\n│         │\n│    
    ♠    │\n│         │\n│        3│\n└─────────┘"] *)

val get_2_player_cards : card list -> string
(** precondition: the card list is of length 2. 
        [get_2_player_cards clist] will return the string representing the icons 
        of the 2 player cards in a list. *)

val get_icon_face_down : string
(** precondition: this value is used when the face_up value is false.
[get_icon_face_down] returns the "card-like image" of the flipped card 
(ie. for any card input, it returns ["┌─────────┐\n│░░░░░░░░░│\n│░░░░░░░░░│\n
│░░░░░░░░░│\n│░░░░░░░░░│\n│░░░░░░░░░│\n└─────────┘"] *)

val get_5_ccards_icon : community_card list -> string
(** precondition: the community_card list is of length 5. 
    [get_5_ccards_icon cclist] will return the string representing the icones 
    of all the community cards in cclist. If a card is face down then it will 
    show a face down card and if it is face it will show that card. The string 
    is confusing due to '\n' but should make sense if printed *)

val remove : int -> 'a list -> 'a list
(** [remove n l] returns the nth element from the list l*)

val rand_card : card list -> card * card list
(** [rand_card deck] returns a random card from the deck and returns the deck
    without that card*)

val draw_cards : int -> card list list
(** [draw_hands_community n] draw the hands for the n players and the 5
    community cards at random. It returns a list
    [\[\[5 community cards\]; \[2 player cards\];...;\[2 player cards\]\]],
    where you have n [\[2 player cards\]] elements*)

val rank :
  card list list -> community_card list -> (card list * card list * string) list
(** [rank plcl ccs], where plcl are the list of player cards and ccs are the five 
    community cards, outputs a list of the winner based on texas hold'em rules. 
    If there is only one winner, most likely scenario, the list will be of size 
    one but if there is a tie the output would include all the winners. The 
    output is list of the winner(s)' 2 cards, 5 winning cards and a string 
    representing the type of the winning hand (ie. flush, pairs, etc)*)