open Card
open Game
open Spectrum

let reset_ppf = Spectrum.prepare_ppf Format.std_formatter
let print_money m = Spectrum.Simple.printf "@{<green>$%s\n@}" (string_of_int m)
let print_name = Spectrum.Simple.printf "@{<blue>%s@}"
let print_yellow = Spectrum.Simple.printf "@{<yellow>%s@}"
let sec_sep = "-----------------------------------------------------------"
let big_sec_sep = "==========================================================="

let print_winner (st : state) =
  let active_cards = get_card_list st in
  let winner = rank active_cards (get_community_cards st) in
  if List.length winner = 1 then
    match List.hd winner with
    | cards, _, hand ->
        let name = find_name (get_actives st) cards in
        print_yellow (name ^ " won: " ^ hand ^ "\n")
  else
    for i = 0 to List.length winner - 1 do
      match List.nth winner i with
      | cards, _, hand ->
          let name = find_name (get_actives st) cards in
          print_yellow (name ^ " won: " ^ hand ^ "\n")
    done

let print_player_info (p : player) (st : state) =
  if p = get_dealer st then print_name "***Dealer***\n" else ();
  if p = get_main_player st then (
    print_name (get_name p ^ "\n");
    print_string (get_2_player_cards (get_cards p));
    print_string "Your money: ";
    print_money (get_money p);
    print_string "Last bet: ";
    print_money (p |> get_last_bet))
  else (
    print_name (get_name p ^ "\n");
    print_string "Money: ";
    print_money (get_money p);
    print_string "Last bet:";
    print_money (get_last_bet p));
  print_endline sec_sep

let rec print_cards (p : player) (st : state) =
  print_name (get_name p ^ "\n");
  print_string (get_2_player_cards (get_cards p));
  print_string "Money: ";
  print_money (get_money p);
  print_endline sec_sep

let rec show_hands (lst : player list) (st : state) =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      print_cards h st;
      show_hands t st

let rec print_community_cards ccards = print_string (get_5_ccards_icon ccards)

let rec print_final_money plist =
  match plist with
  | [] -> print_endline ""
  | h :: t ->
      let name = get_name h in
      let money = get_money h in
      print_endline (name ^ ": $" ^ string_of_int money)

let print_end (st : state) =
  print_endline big_sec_sep;
  Spectrum.Simple.printf "@{<bold>%s\n" "Card Reveal";
  Spectrum.Simple.printf "@{<bold>%s\n" sec_sep;
  print_endline ("Pool: " ^ string_of_int (get_pool st));
  show_hands (get_actives st) st;
  print_endline "Community Cards";
  print_community_cards (get_community_cards st);
  print_winner st;
  print_endline big_sec_sep

let rec print_folded (st : state) =
  let rec get_players_name plst acc =
    match plst with
    | [] -> acc
    | h :: t ->
        if t = [] then acc ^ get_name h
        else
          let s = get_name h ^ ", " in
          get_players_name t (acc ^ s)
  in
  let rec helper (st : state) (lst : player list) (acc : player list) =
    match lst with
    | [] -> acc
    | h :: t ->
        if List.length (List.filter (fun x -> x = h) (get_actives st)) = 0 then
          helper st t (h :: acc)
        else helper st t acc
  in
  let folded = helper st (get_players st) [] in
  print_endline ("Folded: " ^ get_players_name folded "")

let rec print_players players s =
  match players with
  | [] -> print_string ""
  | h :: t ->
      print_player_info h s;
      print_players t s

let print_state (s : state) =
  print_endline big_sec_sep;
  Spectrum.Simple.printf "@{<bold>%s\n" "Active Players";
  Spectrum.Simple.printf "@{<bold>%s\n" sec_sep;
  print_players (get_actives s) s;
  print_folded s;
  print_endline ("Pool: " ^ string_of_int (get_pool s));
  print_endline "Community Cards";
  print_community_cards (get_community_cards s);
  print_endline big_sec_sep

let print_bet (p : player) (n : int) =
  print_name (get_name p ^ "\n");
  print_endline (" just bet " ^ string_of_int n ^ " dollars\n")
