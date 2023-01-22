open Card
open CircleList

exception No_Input
exception Invalid_Input
exception No_Players
exception Wrong_Amount_of_Cards
exception Not_found

type player = { name : string; cards : card list; money : int; last_bet : int }

type game_state = {
  actives : player list;
  last_raise : player;
  pool : int;
  curr_bet : int;
  dealer : player;
}

type state = {
  community_cards : community_card list;
  main_player : player;
  computers : player list;
  game_state : game_state;
  difficulty : int;
}

type action = Raise of int | Call | Fold

let names =
  [|
    "Mike";
    "Sarah";
    "Joey";
    "Samantha";
    "Noah";
    "Emma";
    "James";
    "Sophia";
    "Ben";
    "Mia";
    "Henry";
    "Charlotte";
    "Joe";
  |]

let initiate n money d =
  let x = draw_cards n in
  let count = ref (-1) in
  let ps =
    List.map
      (fun x ->
        count := !count + 1;
        { name = names.(!count); cards = x; money; last_bet = 0 })
      (List.tl x)
  in
  let mp =
    {
      name = "You";
      cards = (List.nth ps (Random.int (List.length ps))).cards;
      money;
      last_bet = 0;
    }
  in
  let actives = List.map (fun x -> if x.cards = mp.cards then mp else x) ps in
  let first_deal = List.hd (List.rev actives) in
  {
    community_cards = List.map (fun x -> create_com_card x false) (List.hd x);
    main_player = mp;
    computers = List.filter (fun x -> x.cards <> mp.cards) ps;
    game_state =
      {
        actives;
        last_raise = first_deal;
        pool = 0;
        curr_bet = 0;
        dealer = first_deal;
      };
    difficulty = d;
  }

let show_flop st =
  Spectrum.Simple.printf "@{<purple, bold>%s@}" "****Flop Flipped Over****\n";
  match st.community_cards with
  | [ c1; c2; c3; c4; c5 ] ->
      let flipped_cards =
        [ Card.flip_card c1; Card.flip_card c2; Card.flip_card c3; c4; c5 ]
      in
      {
        community_cards = flipped_cards;
        main_player = st.main_player;
        computers = st.computers;
        game_state = st.game_state;
        difficulty = st.difficulty;
      }
  | _ -> raise Wrong_Amount_of_Cards

let flop_done (st : state) =
  match st.community_cards with
  | [ c1; c2; c3; c4; c5 ] ->
      if c1.face_up && c2.face_up && c3.face_up then true else false
  | _ -> false

let turn_done (st : state) =
  match st.community_cards with
  | [ c1; c2; c3; c4; c5 ] -> if c4.face_up && flop_done st then true else false
  | _ -> false

let river_done (st : state) =
  match st.community_cards with
  | [ c1; c2; c3; c4; c5 ] -> if c5.face_up && turn_done st then true else false
  | _ -> false

let show_turn st =
  Spectrum.Simple.printf "@{<purple, bold>%s@}" "****Turn Flipped Over****\n";
  match st.community_cards with
  | [ c1; c2; c3; c4; c5 ] ->
      let flipped_cards = [ c1; c2; c3; Card.flip_card c4; c5 ] in
      {
        community_cards = flipped_cards;
        main_player = st.main_player;
        computers = st.computers;
        game_state = st.game_state;
        difficulty = st.difficulty;
      }
  | _ -> raise Wrong_Amount_of_Cards

let show_river st =
  Spectrum.Simple.printf "@{<purple, bold>%s@}" "****River Flipped Over****\n";
  match st.community_cards with
  | [ c1; c2; c3; c4; c5 ] ->
      let flipped_cards = [ c1; c2; c3; c4; Card.flip_card c5 ] in
      {
        community_cards = flipped_cards;
        main_player = st.main_player;
        computers = st.computers;
        game_state = st.game_state;
        difficulty = st.difficulty;
      }
  | _ -> raise Wrong_Amount_of_Cards

let get_cards player = player.cards
let get_money player = player.money
let get_last_bet player = player.last_bet
let get_main_player s = s.main_player
let get_actives s = s.game_state.actives
let get_players s = s.main_player :: s.computers
let get_community_cards s = s.community_cards
let get_name player = player.name
let get_dealer st = st.game_state.dealer
let get_pool st = st.game_state.pool

let change_actives (st : state) (actives' : player list) =
  let gs = st.game_state in
  {
    dealer = gs.dealer;
    actives = actives';
    last_raise = gs.last_raise;
    pool = gs.pool;
    curr_bet = gs.curr_bet;
  }

let change_gs (st : state) (gs' : game_state) =
  {
    community_cards = st.community_cards;
    main_player = st.main_player;
    computers = st.computers;
    game_state = gs';
    difficulty = st.difficulty;
  }

let next_player (st : state) =
  let gs = st.game_state in
  let gs' =
    {
      actives = CircleList.next gs.actives;
      last_raise = gs.last_raise;
      pool = gs.pool;
      curr_bet = gs.curr_bet;
      dealer = gs.dealer;
    }
  in
  change_gs st gs'

let reset_last_bet p =
  { name = p.name; money = p.money; cards = p.cards; last_bet = 0 }

let reset_bet (st : state) =
  let gs' =
    {
      dealer = st.game_state.dealer;
      actives = List.map (fun x -> reset_last_bet x) st.game_state.actives;
      last_raise = st.game_state.last_raise;
      pool = st.game_state.pool;
      curr_bet = 0;
    }
  in
  {
    community_cards = st.community_cards;
    main_player = reset_last_bet st.main_player;
    computers = List.map (fun x -> reset_last_bet x) st.computers;
    game_state = gs';
    difficulty = st.difficulty;
  }

let subtract_money (p : player) (n : int) =
  { name = p.name; cards = p.cards; money = p.money - n; last_bet = n }

let if_mp (st : state) (p : player) = p.cards == st.main_player.cards

let parse_action str st p =
  let lst = String.split_on_char ' ' str |> List.filter (fun x -> x <> "") in
  if List.length lst == 0 then raise No_Input
  else
    match List.hd lst with
    | "raise" -> (
        if List.length lst != 2 then raise Invalid_Input
        else
          match int_of_string (List.nth lst 1) with
          | exception Failure _ -> raise Invalid_Input
          | n ->
              if n < st.game_state.curr_bet || n - p.last_bet > p.money then
                raise Invalid_Input
              else Raise n)
    | "call" -> Call
    | "fold" -> Fold
    | _ -> raise Invalid_Input

let specific_parse_action str st p =
  let lst = String.split_on_char ' ' str |> List.filter (fun x -> x <> "") in
  if List.length lst == 0 then raise No_Input
  else
    match List.hd lst with
    | "call" -> Call
    | "fold" -> Fold
    | _ -> raise Invalid_Input

let rec get_action () st p b =
  if b then (
    try parse_action (read_line ()) st p
    with _ ->
      print_string
        "\n\
         Invalid input. Enter \"call\", \"fold\", or \"raise n\" (where n is \
         an integer)\n";
      print_string "> ";
      get_action () st p b)
  else
    try specific_parse_action (read_line ()) st p
    with _ ->
      print_string "\nInvalid input. Enter \"call\" or \"fold\"\n";
      print_string "> ";
      get_action () st p b

let update_list (lst : player list) (p : player) (p' : player) =
  List.map (fun x -> if x = p then p' else x) lst

let bet_print (p : player) (n : int) (st : state) =
  if n = st.game_state.curr_bet then print_endline (p.name ^ " called\n")
  else print_endline (p.name ^ " raised " ^ string_of_int n ^ " dollars\n");
  st

let raise_print (p : player) (n : int) (st : state) =
  if p = st.main_player then (
    print_endline ("You raised the bet to " ^ string_of_int n ^ " dollars\n");
    st)
  else (
    print_endline
      (p.name ^ " raised the bet to " ^ string_of_int n ^ " dollars\n");
    st)

let fold_print (p : player) (st : state) =
  print_endline (p.name ^ " folded\n");
  st

let print_blinds p1 p2 (st : state) =
  Printf.printf "Small blind: %s\n" (get_name p1);
  Printf.printf "Big blind: %s\n" (get_name p2);
  st

(* bet takes in a players bet, and completely updates the state, regardless of raise, call, etc*)
let bet (p : player) (n : int) (st : state) =
  let extra = if n - p.last_bet > 0 then n - p.last_bet else 0 in
  let p' =
    { name = p.name; cards = p.cards; money = p.money - extra; last_bet = n }
  in
  let gs' =
    {
      dealer = st.game_state.dealer;
      actives = CircleList.next (update_list st.game_state.actives p p');
      last_raise =
        (if st.game_state.curr_bet = n then st.game_state.last_raise else p');
      pool = st.game_state.pool + extra;
      curr_bet = p'.last_bet;
    }
  in
  {
    community_cards = st.community_cards;
    main_player = (if p = st.main_player then p' else st.main_player);
    computers = update_list st.computers p p';
    game_state = gs';
    difficulty = st.difficulty;
  }

let mp_fold st =
  let actives' =
    List.filter (fun x -> x <> st.main_player) st.game_state.actives
  in
  if st.main_player.name == st.game_state.last_raise.name then
    let last' = CircleList.last actives' in
    let gs' =
      {
        dealer = st.game_state.dealer;
        actives = actives';
        last_raise = last';
        pool = st.game_state.pool;
        curr_bet = st.game_state.curr_bet;
      }
    in
    change_gs st gs'
  else
    let gs' =
      {
        dealer = st.game_state.dealer;
        actives = actives';
        last_raise = st.game_state.last_raise;
        pool = st.game_state.pool;
        curr_bet = st.game_state.curr_bet;
      }
    in
    change_gs st gs' |> fold_print st.main_player

let user_action (st : state) () =
  let money = st.main_player.money in
  if money = 0 then (
    print_endline "You called because you have no money left\n";
    bet st.main_player 0 st)
  else if money < st.game_state.curr_bet - st.main_player.last_bet then (
    print_endline
      ("You have " ^ string_of_int money
     ^ " dollars, so you would use all your money matching the bet.");
    print_endline "Do you want to \"call\" or \"fold\"?\n";
    print_string "> ";
    let input = get_action () st st.main_player false in
    match input with
    | Fold -> mp_fold st
    | Call -> bet st.main_player money st
    | Raise n -> raise Invalid_Input)
  else (
    print_endline ("You have " ^ string_of_int money ^ " dollars.");
    print_endline
      "Do you want to \"call\", \"fold\", or \"raise n\" (where n is an \
       integer)?\n";
    print_string "> ";
    let input = get_action () st st.main_player true in
    match input with
    | Fold -> mp_fold st
    | Call -> bet st.main_player st.game_state.curr_bet st
    | Raise n -> bet st.main_player n st)

let player_action (p : player) (a : action) (st : state) =
  match a with
  | Fold ->
      let actives' = List.filter (fun x -> x <> p) st.game_state.actives in
      change_gs st (change_actives st actives') |> fold_print p
  | Call ->
      if p.money = 0 || st.game_state.curr_bet - p.last_bet > p.money then
        bet p p.money st |> bet_print p p.money
      else bet p st.game_state.curr_bet st |> bet_print p st.game_state.curr_bet
  | Raise n -> bet p n st |> raise_print p n

let poker_ai p st =
  let n = Random.int 5 in
  match n with
  | 0 ->
      if st.game_state.curr_bet > 0 then player_action p Fold st
      else player_action p Call st
  | 1 | 2 -> player_action p Call st
  | 3 | 4 ->
      let amt =
        st.game_state.curr_bet + ((1 + Random.int 4) * 10) - p.last_bet
      in
      if amt > p.money then player_action p Call st
      else player_action p (Raise (amt + p.last_bet)) st
  | _ -> player_action p Fold st

let computer_decision (p : player) (st : state) : state =
  if p.money = 0 || st.game_state.curr_bet - p.last_bet > p.money then
    let n = Random.int 3 in
    match n with
    (* | 0 | 1 -> player_action p Fold st *)
    | _ -> player_action p Call st
  else if st.difficulty = 1 then
    let n = Random.int 5 in
    match n with
    | 0 | 1 ->
        if st.game_state.curr_bet > 0 then player_action p Fold st
        else player_action p Call st
    | 2 | 3 -> player_action p Call st
    | 4 ->
        let amt =
          st.game_state.curr_bet + ((1 + Random.int 4) * 10) - p.last_bet
        in
        if amt > p.money then player_action p Call st
        else player_action p (Raise (amt + p.last_bet)) st
    | _ -> player_action p Fold st
  else poker_ai p st

let rec blinds (st : state) =
  let players = st.game_state.actives in
  if CircleList.peek players = st.game_state.dealer then
    next_player st
    |> bet (CircleList.nth players 1) 10
    |> bet (CircleList.nth players 2) 20
    |> print_blinds (CircleList.nth players 1) (CircleList.nth players 2)
  else blinds (next_player st)

let rec first_round (blinds_done : bool) (st : state) =
  if not blinds_done then first_round true (blinds st)
  else
    let players = st.game_state.actives in
    if
      st.game_state.last_raise.name != (CircleList.peek players).name
      || CircleList.is_empty players
    then
      match players with
      | h :: _ ->
          if h = st.main_player then user_action st () |> first_round true
          else st |> computer_decision h |> first_round true
      | _ -> raise No_Players
    else st

let rec next_round (first_bet : bool) (st : state) =
  let players = st.game_state.actives in
  if (not (CircleList.length players = 1)) && first_bet then
    let p = CircleList.peek st.game_state.actives in
    if CircleList.peek players = st.main_player then
      user_action st () |> next_round false
    else computer_decision p st |> next_round false
  else if
    st.game_state.last_raise.name != (CircleList.peek players).name
    && not (CircleList.length players = 1)
  then
    match players with
    | h :: _ ->
        if h = st.main_player then user_action st () |> next_round false
        else st |> computer_decision h |> next_round false
    | _ -> raise No_Players
  else st

let get_card_list (st : state) =
  st.game_state.actives
  |> List.map (fun x -> x.cards)
  |> List.fold_left (fun lst x -> x :: lst) []

let rec find_name (actives : player list) (cards : card list) =
  match actives with
  | [] -> ""
  | h :: t -> if h.cards == cards then h.name else find_name t cards

let rec find_player (actives : player list) (cards : card list) =
  match actives with
  | [] -> raise Not_found
  | h :: t -> if h.cards == cards then h else find_player t cards

let add_money (p : player) (n : int) (st : state) =
  let p' =
    { name = p.name; cards = p.cards; money = p.money + n; last_bet = n }
  in
  let gs' =
    {
      dealer = st.game_state.dealer;
      actives = update_list st.game_state.actives p p';
      last_raise = st.game_state.last_raise;
      pool = st.game_state.pool;
      curr_bet = 0;
    }
  in
  {
    community_cards = st.community_cards;
    main_player = (if p = st.main_player then p' else st.main_player);
    computers = update_list st.computers p p';
    game_state = gs';
    difficulty = st.difficulty;
  }

let empty_pool st plist =
  let num_winners = List.length plist in
  let reward = st.game_state.pool / num_winners in
  let rec update_state plist st =
    match plist with
    | [] -> st
    | h :: t -> update_state t (add_money h reward st)
  in
  update_state plist st