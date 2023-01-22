Random.self_init ()

open Yojson.Basic.Util

type card = Spades of int | Hearts of int | Clubs of int | Diamonds of int
type community_card = { ccard : card; face_up : bool }

let deck =
  let rec helper n =
    if n = 1 then []
    else Spades n :: Hearts n :: Clubs n :: Diamonds n :: helper (n - 1)
  in
  helper 14

let is_face_up community_card = community_card.face_up
let get_ccard community_card = community_card.ccard
let create_com_card cc fu = { ccard = cc; face_up = fu }
let flip_card c = { ccard = c.ccard; face_up = true }

let get_card_value c =
  match c with Hearts n -> n | Spades n -> n | Clubs n -> n | Diamonds n -> n

let get_card_value_string c =
  match get_card_value c with
  | 2 -> "Two"
  | 3 -> "Three"
  | 4 -> "Four"
  | 5 -> "Five"
  | 6 -> "Six"
  | 7 -> "Seven"
  | 8 -> "Eight"
  | 9 -> "Nine"
  | 10 -> "Ten"
  | 11 -> "Jack"
  | 12 -> "Queen"
  | 13 -> "King"
  | 14 -> "Ace"
  | _ -> failwith "The value of the card is not that of a 52 card deck"

let get_card_suit c =
  match c with
  | Spades _ -> "Spades"
  | Hearts _ -> "Hearts"
  | Clubs _ -> "Clubs"
  | Diamonds _ -> "Diamonds"

let get_string_card c = get_card_value_string c ^ " " ^ get_card_suit c

let get_icon_card (c : card) =
  let json = Yojson.Basic.from_file "json/cards.json" in
  json
  |> member (get_card_suit c)
  |> member (get_card_value_string c)
  |> to_string

let get_2_player_cards (clist : card list) : string =
  if List.length clist != 2 then failwith "Invalid number of cards"
  else
    let clist_format = List.map (fun c -> get_icon_card c) clist in
    let clist_adjust =
      List.map (fun c -> String.split_on_char '\n' c) clist_format
    in
    let helper l n1 n2 = List.nth (List.nth l n1) n2 in
    let list_to_string clist =
      let o = ref "" in
      for i = 0 to 6 do
        o := !o ^ helper clist 0 i ^ " " ^ helper clist 1 i ^ " " ^ "\n"
      done;
      !o
    in
    list_to_string clist_adjust

let get_icon_face_down =
  let json = Yojson.Basic.from_file "json/cards.json" in
  json |> member "Flipped" |> to_string

let get_5_ccards_icon cclist =
  if List.length cclist != 5 then failwith "Invalid number of ccards"
  else
    let ccdraw =
      List.map
        (fun cc ->
          if cc.face_up then get_icon_card cc.ccard else get_icon_face_down)
        cclist
    in
    let ccreformat = List.map (fun cc -> String.split_on_char '\n' cc) ccdraw in
    let helper l n1 n2 = List.nth (List.nth l n1) n2 in
    let list_to_string clist =
      let o = ref "" in
      for i = 0 to 6 do
        o :=
          !o ^ helper clist 0 i ^ " " ^ helper clist 1 i ^ " "
          ^ helper clist 2 i ^ " " ^ helper clist 3 i ^ " " ^ helper clist 4 i
          ^ "\n"
      done;
      !o
    in
    list_to_string ccreformat

let remove n l =
  let rec first n l =
    if n > 0 then List.hd l :: first (n - 1) (List.tl l) else []
  in
  let rec second n l = if n >= 0 then second (n - 1) (List.tl l) else l in
  first n l @ second n l

let rand_card (deck : card list) =
  let x = Random.int (List.length deck) in
  (List.nth deck x, remove x deck)

let draw_cards n =
  let rec helper n sub_deck =
    let c1 = rand_card sub_deck in
    let c2 = rand_card (snd c1) in
    if n > 0 then [ fst c1; fst c2 ] :: helper (n - 1) (snd c2) else []
  in
  let com1 = rand_card deck in
  let com2 = rand_card (snd com1) in
  let com3 = rand_card (snd com2) in
  let com4 = rand_card (snd com3) in
  let com5 = rand_card (snd com4) in
  [ fst com1; fst com2; fst com3; fst com4; fst com5 ] :: helper n (snd com5)

let flush cards =
  let rec helper acc st = function
    | [] -> acc
    | h :: t ->
        if get_card_suit h = st then
          (float_of_int (get_card_value h) *. (51. ** acc))
          +. helper (acc +. 1.) st t
        else -100000000000.
  in
  let temp = helper 0. (get_card_suit (List.nth cards 0)) cards in
  if temp < 0. then 0. else temp /. 32000.

let straight = function
  | [ c1; c2; c3; c4; c5 ] ->
      if
        get_card_value c1 + 1 = get_card_value c2
        && get_card_value c2 + 1 = get_card_value c3
        && get_card_value c3 + 1 = get_card_value c4
        && get_card_value c4 + 1 = get_card_value c5
      then 85. *. float_of_int (get_card_value c5)
      else if
        (get_card_value c5 = 14 && get_card_value c1 = 2)
        && get_card_value c1 + 1 = get_card_value c2
        && get_card_value c2 + 1 = get_card_value c3
        && get_card_value c3 + 1 = get_card_value c4
      then 85. *. float_of_int (get_card_value c4)
      else 0.
  | _ -> failwith "Invalid Input"

let rec to_assoc l =
  let rec helper1 l acc =
    match l with
    | [] -> (acc, [])
    | h :: t ->
        if get_card_value h = get_card_value (fst acc) then
          helper1 t (h, snd acc + 1)
        else (acc, h :: t)
  in
  match l with
  | [] -> []
  | h :: t ->
      let t = helper1 t (h, 1) in
      fst t :: to_assoc (snd t)

let royal_or_straigh_flush = function
  | h :: t -> if 10 = get_card_value h then "Royal Flush" else "Straight flush"
  | _ -> failwith "Impossible"

let score_5 cards =
  let sorted_cards =
    List.sort (fun c1 c2 -> get_card_value c1 - get_card_value c2) cards
  in
  let assoc_hand = to_assoc sorted_cards in
  if List.length assoc_hand = 5 then
    let flush_points = flush sorted_cards in
    let straight_points = straight sorted_cards in
    if flush_points > 0. then
      if straight_points > 0. then
        (straight_points *. 375., royal_or_straigh_flush sorted_cards)
        (* Straight/Royal Flush *)
      else (flush_points, "Flush") (* Flush *)
    else if straight_points > 0. then (straight_points, "Straight")
      (* Straight *)
    else
      (* High Card *)
      let rec sum1 n = function
        | [] -> 0
        | h :: t ->
            (get_card_value h * int_of_float (51. ** n)) + sum1 (n +. 1.) t
      in
      (float_of_int (sum1 0. cards) /. 100000000., "High card")
  else if List.length assoc_hand = 4 then
    (* One pair *)
    let rec sum2 acc = function
      | [] -> 0
      | (c, n) :: t ->
          if n = 2 then
            (get_card_value c * int_of_float (51. ** 4.)) + sum2 acc t
          else
            (get_card_value c * int_of_float (51. ** acc)) + sum2 (acc +. 1.) t
    in
    (float_of_int (sum2 0. assoc_hand) /. 10000000., "Pair")
  else if List.length assoc_hand = 3 then
    let n1, n2, n3 =
      ( snd (List.nth assoc_hand 0),
        snd (List.nth assoc_hand 1),
        snd (List.nth assoc_hand 2) )
    in
    if n1 = 3 || n2 = 3 || n3 = 3 then
      (* 3 of a kind *)
      let rec sum3 acc = function
        | [] -> 0
        | (c, n) :: t ->
            if n = 3 then
              (get_card_value c * int_of_float (51. ** 3.)) + sum3 (acc +. 1.) t
            else
              (get_card_value c * int_of_float (51. ** acc))
              + sum3 (acc +. 1.) t
      in
      (float_of_int (sum3 0. assoc_hand) /. 4400., "Three of a kind")
    else
      (* 2 pairs *)
      let rec sum4 acc = function
        | [] -> 0
        | (c, n) :: t ->
            if n = 2 then
              (get_card_value c * int_of_float (20. ** acc))
              + sum4 (acc +. 1.) t
            else get_card_value c + sum4 acc t
      in
      (float_of_int (sum4 1. assoc_hand) /. 100., "Two Pairs")
  else
    let n1, v1, n2, v2 =
      ( snd (List.nth assoc_hand 0),
        get_card_value (fst (List.nth assoc_hand 0)),
        snd (List.nth assoc_hand 1),
        get_card_value (fst (List.nth assoc_hand 1)) )
    in
    if n1 = 3 || n2 = 3 then
      (* FULL HOUSE*)
      if n1 = 3 then (100. *. float_of_int ((14 * v1) + v2), "Full House")
      else (100. *. float_of_int ((14 * v2) + v1), "Full House")
    else if n1 = 4 (* FOUR OF A KIND *) then
      (680. *. float_of_int ((14 * v1) + v2), "Four of a Kind")
    else (686. *. float_of_int ((14 * v2) + v1), "Four of a Kind")

let remove_2 skip l =
  let rec helper acc skip = function
    | [] -> []
    | h :: t ->
        if acc = fst skip then helper (acc + 1) skip t
        else if acc = snd skip then helper (acc + 1) skip t
        else h :: helper (acc + 1) skip t
  in
  helper 0 skip l

let get_pairs l =
  let length = List.length l - 1 in
  let rec find_subset min max acc =
    if acc = max then [ (min, acc) ]
    else (min, acc) :: find_subset min max (acc + 1)
  in
  let rec find_set min max =
    if max = min then []
    else find_subset min max (min + 1) :: find_set (min + 1) max
  in
  List.flatten (find_set 0 length)

let score_player (pl_cards : card list) (cc : community_card list) =
  let seven_cards = List.map (fun { ccard; face_up } -> ccard) cc @ pl_cards in
  let pairs_to_remove = get_pairs seven_cards in
  let rec find_best best_score pairs_to_remove seven_cards =
    match pairs_to_remove with
    | [] -> best_score
    | h :: t ->
        let temp1 = remove_2 h seven_cards in
        let temp2 = score_5 temp1 in
        if fst temp2 > fst (fst best_score) then
          find_best (temp2, remove_2 h seven_cards) t seven_cards
        else find_best best_score t seven_cards
  in
  find_best ((0., ""), []) pairs_to_remove seven_cards

let rank (plst : card list list) (cc : community_card list) =
  let output = ref [] in
  let rec helper plst cc best =
    match plst with
    | [] -> best
    | h :: t ->
        let temp = score_player h cc in
        if fst (fst temp) > fst (fst (snd best)) then helper t cc (h, temp)
        else if fst (fst temp) < fst (fst (snd best)) then helper t cc best
        else (
          output := (h, snd temp, snd (fst temp)) :: !output;
          helper t cc best)
  in
  let temp = helper plst cc ([], ((0., ""), [])) in
  (fst temp, snd (snd temp), snd (fst (snd temp))) :: !output