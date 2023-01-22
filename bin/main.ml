(* Main loop of the poker game *)
open Pocker
open Game
open Print
open Spectrum
open Card

let start_money = 1000

let start_message1 =
  "\n\
  \  ╭╮╭╮╭╮  ╭╮              ╭╮    ╭━━━╮        ╭╮ ╭╮ ╭╮  ╭╮  ╭╮\n\
  \  ┃┃┃┃┃┃  ┃┃             ╭╯╰╮   ┃╭━╮┃        ┃┃ ┃┃ ┃┃  ┃┃  ┃┃     ╭╮\n\
  \  ┃┃┃┃┃┣━━┫┃╭━━┳━━┳╮╭┳━━╮╰╮╭╋━━╮┃┃ ┃┣━━┳━━┳╮╭┫┃ ┃╰━╯┣━━┫┃╭━╯┣━━┳╮╭┫┃\n\
  \  ┃╰╯╰╯┃┃━┫┃┃╭━┫╭╮┃╰╯┃┃━┫ ┃┃┃╭╮┃┃┃ ┃┃╭━┫╭╮┃╰╯┃┃ ┃╭━╮┃╭╮┃┃┃╭╮┃┃━┫╰╯┣╯\n\
  \  ╰╮╭╮╭┫┃━┫╰┫╰━┫╰╯┃┃┃┃┃━┫ ┃╰┫╰╯┃┃╰━╯┃╰━┫╭╮┃┃┃┃╰╮┃┃ ┃┃╰╯┃╰┫╰╯┃┃━┫┃┃┃\n\
  \   ╰╯╰╯╰━━┻━┻━━┻━━┻┻┻┻━━╯ ╰━┻━━╯╰━━━┻━━┻╯╰┻┻┻┻━╯╰╯ ╰┻━━┻━┻━━┻━━┻┻┻╯"

let start_message2 =
  "\n\
  \  ╭━━┳╮            ╭╮  ╭━┳╮     ╭╮                       ╭╮\n\
  \  ╰╮╭┫╰┳━╮╭━┳┳┳━┳━╮┃╰┳━┫━┫╰╮╭━┳━┫┣┳━┳┳╮╭━┳━╮╭━━┳━╮╭━━┳━╮╭╯┣━╮\n\
  \   ┃┃┃┃┃┻┫┃╋┃╭┫┻┫╋╰┫╭┫┻╋━┃╭┫┃╋┃╋┃━┫┻┫╭╯┃╋┃╋╰┫┃┃┃┻┫┃┃┃┃╋╰┫╋┃┻┫\n\
  \   ╰╯╰┻┻━╯┣╮┣╯╰━┻━━┻━┻━┻━┻━╯┃╭┻━┻┻┻━┻╯ ┣╮┣━━┻┻┻┻━╯╰┻┻┻━━┻━┻━╯\n\
  \          ╰━╯               ╰╯         ╰━╯"

let rec get_input low high =
  print_string "> ";
  let x = read_line () in
  let y =
    try int_of_string x
    with exn ->
      Spectrum.Simple.printf "@{<red>%s@}\n" "Invalid input";
      get_input low high
  in
  if y >= low && y <= high then y
  else (
    Spectrum.Simple.printf "@{<yellow>%s@}\n"
      "Input is either too high or too low ";
    get_input low high)

let quit input = if input = "quit" then false else true

(* let play_game state = let s = ref state in while quit (print_string "Click
   enter to continue. Type \"quit\" to leave. \n > "; read_line ()) do s :=
   round_1 !s; print_state !s done *)

let get_winners st =
  let active_cards = get_card_list st in
  let winners = rank active_cards (get_community_cards st) in
  let rec helper plist acc =
    match plist with
    | [] -> acc
    | (cards, _, _) :: t ->
        let p = find_player (get_actives st) cards in
        helper t (p :: acc)
  in
  helper winners []

let reset_state st = get_winners st |> empty_pool st

let print_helper st =
  print_state st;
  st

let rec game (r1 : bool) (first_bet : bool) (st : state) =
  if r1 then first_round false st |> game false true
  else if not (flop_done st) then
    st |> reset_bet |> show_flop |> print_helper |> next_round true
    |> game false true
  else if not (turn_done st) then
    st |> reset_bet |> show_turn |> print_helper |> next_round true
    |> game false true
  else if not (river_done st) then
    st |> reset_bet |> show_river |> print_helper |> next_round true
    |> game false true
  else st

let main () =
  print_endline start_message1;
  print_endline start_message2;
  print_endline
    "Please enter the number of opponents you want to play with(between 3 and \
     12).\n";
  let num_players = get_input 3 12 + 1 in
  print_endline ("Number of players: " ^ string_of_int num_players ^ "\n");
  print_endline "Please enter the difficulty (1 for easy or 2 for hard)";
  let difficulty = get_input 1 2 in
  print_endline ("Difficulty: " ^ if difficulty = 1 then "Easy" else "Hard");
  let x = initiate num_players start_money difficulty in
  print_state x;
  let finished_game = game true false x in
  print_end (reset_state finished_game)

let () = main ()