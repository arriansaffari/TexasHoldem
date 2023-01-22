(** 
    This testing approach demonstrates an overall correctness of thte system 
    because I chose to automate both glass and black box testing for the 
    fundamental functions that enable me to manipulate the deck, while manually 
    simulating the game enviornment several times to ensure a smooth and correct
    progression of my rounds. 
*)

open OUnit2
open Pocker
open Card
open CircleList
open Print

let test name func input expected =
  name >:: fun _ -> assert_equal expected (func input)

let first_test (name : string) (tuple : int * int) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (fst tuple)

let second_test (name : string) (tuple : int * int) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (snd tuple)

let get_string_card_test (name : string) (c : card) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_string_card c)

let is_face_up_test (name : string) (cc : community_card)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_face_up cc)

let get_ccard_test (name : string) (c : community_card) (expected_output : card)
    : test =
  name >:: fun _ -> assert_equal expected_output (get_ccard c)

let create_com_card_test (name : string) (card : card) (face_up : bool)
    (expected_output : community_card) : test =
  name >:: fun _ -> assert_equal expected_output (create_com_card card face_up)

let flip_card_test (name : string) (cc : community_card)
    (expected_output : community_card) : test =
  name >:: fun _ -> assert_equal expected_output (flip_card cc)

let get_card_value_test (name : string) (c : card) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_card_value c)

let get_card_suit_test (name : string) (c : card) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_card_suit c)

let testing_rank (name : string) (ccs : card list) (pcs : card list list)
    (expected_output : (card list * card list * string) list) =
  let ccards = List.map (fun card -> create_com_card card false) ccs in
  name >:: fun _ -> assert_equal expected_output (rank pcs ccards)

let testing_rank (name : string) (ccs : card list) (pcs : card list list)
    (expected_output : (card list * card list * string) list) =
  let ccards = List.map (fun card -> create_com_card card false) ccs in
  name >:: fun _ -> assert_equal expected_output (rank pcs ccards)

let plc1 = [ Hearts 14; Diamonds 12 ]
let plc2 = [ Hearts 4; Diamonds 5 ]
let plc3 = [ Hearts 3; Diamonds 10 ]

let card_tests =
  [
    second_test "testing [second] tuple function" (1, 2) 2;
    first_test "testing [first] tuple function" (1, 2) 1;
    get_string_card_test "testing [get_string_card] on 2 of hearts" (Hearts 2)
      "Two Hearts";
    get_string_card_test "testing [get_string_card] on 3 of hearts" (Hearts 3)
      "Three Hearts";
    get_string_card_test "testing [get_string_card] on 4 of hearts" (Hearts 4)
      "Four Hearts";
    get_string_card_test "testing [get_string_card] on 5 of hearts" (Hearts 5)
      "Five Hearts";
    get_string_card_test "testing [get_string_card] on 6 of hearts" (Hearts 6)
      "Six Hearts";
    get_string_card_test "testing [get_string_card] on 7 of hearts" (Hearts 7)
      "Seven Hearts";
    get_string_card_test "testing [get_string_card] on 8 of hearts" (Hearts 8)
      "Eight Hearts";
    get_string_card_test "testing [get_string_card] on 9 of hearts" (Hearts 9)
      "Nine Hearts";
    get_string_card_test "testing [get_string_card] on 10 of hearts" (Hearts 10)
      "Ten Hearts";
    get_string_card_test "testing [get_string_card] on 11 of hearts" (Hearts 11)
      "Jack Hearts";
    get_string_card_test "testing [get_string_card] on 12 of hearts" (Hearts 12)
      "Queen Hearts";
    get_string_card_test "testing [get_string_card] on 13 of hearts" (Hearts 13)
      "King Hearts";
    get_string_card_test "testing [get_string_card] on 14 of hearts" (Hearts 14)
      "Ace Hearts";
    get_string_card_test "testing [get_string_card] on 2 of spades" (Spades 2)
      "Two Spades";
    get_string_card_test "testing [get_string_card] on 3 of Spades" (Spades 3)
      "Three Spades";
    get_string_card_test "testing [get_string_card] on 4 of Spades" (Spades 4)
      "Four Spades";
    get_string_card_test "testing [get_string_card] on 5 of Spades" (Spades 5)
      "Five Spades";
    get_string_card_test "testing [get_string_card] on 6 of Spades" (Spades 6)
      "Six Spades";
    get_string_card_test "testing [get_string_card] on 7 of Spades" (Spades 7)
      "Seven Spades";
    get_string_card_test "testing [get_string_card] on 8 of Spades" (Spades 8)
      "Eight Spades";
    get_string_card_test "testing [get_string_card] on 9 of Spades" (Spades 9)
      "Nine Spades";
    get_string_card_test "testing [get_string_card] on 10 of Spades" (Spades 10)
      "Ten Spades";
    get_string_card_test "testing [get_string_card] on 11 of Spades" (Spades 11)
      "Jack Spades";
    get_string_card_test "testing [get_string_card] on 12 of Spades" (Spades 12)
      "Queen Spades";
    get_string_card_test "testing [get_string_card] on 13 of Spades" (Spades 13)
      "King Spades";
    get_string_card_test "testing [get_string_card] on 14 of Spades" (Spades 14)
      "Ace Spades";
    get_string_card_test "testing [get_string_card] on 2 of Clubs" (Clubs 2)
      "Two Clubs";
    get_string_card_test "testing [get_string_card] on 3 of Clubs" (Clubs 3)
      "Three Clubs";
    get_string_card_test "testing [get_string_card] on 4 of Clubs" (Clubs 4)
      "Four Clubs";
    get_string_card_test "testing [get_string_card] on 5 of Clubs" (Clubs 5)
      "Five Clubs";
    get_string_card_test "testing [get_string_card] on 6 of Clubs" (Clubs 6)
      "Six Clubs";
    get_string_card_test "testing [get_string_card] on 7 of Clubs" (Clubs 7)
      "Seven Clubs";
    get_string_card_test "testing [get_string_card] on 8 of Clubs" (Clubs 8)
      "Eight Clubs";
    get_string_card_test "testing [get_string_card] on 9 of Clubs" (Clubs 9)
      "Nine Clubs";
    get_string_card_test "testing [get_string_card] on 10 of Clubs" (Clubs 10)
      "Ten Clubs";
    get_string_card_test "testing [get_string_card] on 11 of Clubs" (Clubs 11)
      "Jack Clubs";
    get_string_card_test "testing [get_string_card] on 12 of Clubs" (Clubs 12)
      "Queen Clubs";
    get_string_card_test "testing [get_string_card] on 13 of Clubs" (Clubs 13)
      "King Clubs";
    get_string_card_test "testing [get_string_card] on 14 of Clubs" (Clubs 14)
      "Ace Clubs";
    get_string_card_test "testing [get_string_card] on 2 of Diamonds"
      (Diamonds 2) "Two Diamonds";
    get_string_card_test "testing [get_string_card] on 3 of Diamonds"
      (Diamonds 3) "Three Diamonds";
    get_string_card_test "testing [get_string_card] on 4 of Diamonds"
      (Diamonds 4) "Four Diamonds";
    get_string_card_test "testing [get_string_card] on 5 of DiamondsDiamonds"
      (Diamonds 5) "Five Diamonds";
    get_string_card_test "testing [get_string_card] on 6 of Diamonds"
      (Diamonds 6) "Six Diamonds";
    get_string_card_test "testing [get_string_card] on 7 of Diamonds"
      (Diamonds 7) "Seven Diamonds";
    get_string_card_test "testing [get_string_card] on 8 of Diamonds"
      (Diamonds 8) "Eight Diamonds";
    get_string_card_test "testing [get_string_card] on 9 of Diamonds"
      (Diamonds 9) "Nine Diamonds";
    get_string_card_test "testing [get_string_card] on 10 of Diamonds"
      (Diamonds 10) "Ten Diamonds";
    get_string_card_test "testing [get_string_card] on 11 of Diamonds"
      (Diamonds 11) "Jack Diamonds";
    get_string_card_test "testing [get_string_card] on 12 of Diamonds"
      (Diamonds 12) "Queen Diamonds";
    get_string_card_test "testing [get_string_card] on 13 of Diamonds"
      (Diamonds 13) "King Diamonds";
    get_string_card_test "testing [get_string_card] on 14 of Diamonds"
      (Diamonds 14) "Ace Diamonds";
    is_face_up_test "testing [is_face_up] on community card with face up true"
      { ccard = Spades 5; face_up = true }
      true;
    is_face_up_test "testing [is_face_up] on community card with face up true"
      { ccard = Clubs 6; face_up = false }
      false;
    get_ccard_test "testing [get_ccard] on community card with 3 of hearts"
      { ccard = Hearts 3; face_up = true }
      (Hearts 3);
    create_com_card_test "testing [create_com_card]" (Spades 1) true
      { ccard = Spades 1; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 2) true
      { ccard = Spades 2; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 3) true
      { ccard = Spades 3; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 4) true
      { ccard = Spades 4; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 5) true
      { ccard = Spades 5; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 6) true
      { ccard = Spades 6; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 7) true
      { ccard = Spades 7; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 8) true
      { ccard = Spades 8; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 9) true
      { ccard = Spades 9; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 10) true
      { ccard = Spades 10; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 11) true
      { ccard = Spades 11; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 12) true
      { ccard = Spades 12; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 13) true
      { ccard = Spades 13; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 14) true
      { ccard = Spades 14; face_up = true };
    create_com_card_test "testing [create_com_card]" (Spades 1) false
      { ccard = Spades 1; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 2) false
      { ccard = Spades 2; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 3) false
      { ccard = Spades 3; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 4) false
      { ccard = Spades 4; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 5) false
      { ccard = Spades 5; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 6) false
      { ccard = Spades 6; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 7) false
      { ccard = Spades 7; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 8) false
      { ccard = Spades 8; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 9) false
      { ccard = Spades 9; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 10) false
      { ccard = Spades 10; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 11) false
      { ccard = Spades 11; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 12) false
      { ccard = Spades 12; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 13) false
      { ccard = Spades 13; face_up = false };
    create_com_card_test "testing [create_com_card]" (Spades 14) false
      { ccard = Spades 14; face_up = false };
    flip_card_test "testing [flip_card]"
      { ccard = Hearts 9; face_up = false }
      { ccard = Hearts 9; face_up = true };
    flip_card_test "testing [flip_card]"
      { ccard = Hearts 9; face_up = true }
      { ccard = Hearts 9; face_up = true };
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 1) 1;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 14) 14;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 5) 5;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 10)
      10;
    get_card_value_test "testing [get_card_value] for Hearts 2" (Hearts 2) 2;
    get_card_value_test "testing [get_card_value] for Spades 13" (Spades 1) 1;
    get_card_value_test "testing [get_card_value] for Clubs 6" (Clubs 6) 6;
    get_card_value_test "testing [get_card_value] for Diamonds 11" (Diamonds 11)
      11;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 3) 3;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 2) 2;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 7) 7;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 12)
      12;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 4) 4;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 3) 3;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 8) 8;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 13)
      13;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 5) 5;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 4) 4;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 9) 9;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 14)
      14;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 6) 6;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 5) 5;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 10) 10;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 1)
      1;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 7) 7;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 8) 8;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 11) 11;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 2)
      2;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 8) 8;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 9) 9;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 12) 12;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 3)
      3;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 9) 9;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 10) 10;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 13) 13;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 4)
      4;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 10) 10;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 11) 11;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 14) 14;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 5)
      5;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 11) 11;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 12) 12;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 1) 1;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 6)
      6;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 12) 12;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 13) 13;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 2) 2;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 7)
      7;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 13) 13;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 14) 14;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 3) 3;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 8)
      8;
    get_card_value_test "testing [get_card_value] for Hearts 1" (Hearts 14) 14;
    get_card_value_test "testing [get_card_value] for Spades 14" (Spades 13) 13;
    get_card_value_test "testing [get_card_value] for Clubs 5" (Clubs 4) 4;
    get_card_value_test "testing [get_card_value] for Diamonds 10" (Diamonds 9)
      9;
    get_card_suit_test "tesing [get_card_suit] for Hearts 1" (Hearts 1) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 14" (Spades 14)
      "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 5" (Clubs 5) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 10" (Diamonds 10)
      "Diamonds";
    get_string_card_test "testing [get_string_card] for Clubs 8" (Clubs 8)
      "Eight Clubs";
    get_card_suit_test "tesing [get_card_suit] for Hearts 2" (Hearts 2) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 1" (Spades 1) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 6" (Clubs 6) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 11" (Diamonds 11)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 3" (Hearts 3) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 2" (Spades 2) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 7" (Clubs 7) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 12" (Diamonds 12)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 4" (Hearts 4) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 3" (Spades 3) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 8" (Clubs 8) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 13" (Diamonds 13)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 5" (Hearts 5) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 4" (Spades 4) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 9" (Clubs 9) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 14" (Diamonds 14)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 6" (Hearts 6) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 5" (Spades 5) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 10" (Clubs 10) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 1" (Diamonds 1)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 7" (Hearts 7) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 6" (Spades 6) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 11" (Clubs 11) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 2" (Diamonds 2)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 8" (Hearts 8) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 7" (Spades 7) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 12" (Clubs 12) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 3" (Diamonds 3)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 9" (Hearts 9) "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 8" (Spades 8) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 13" (Clubs 13) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 4" (Diamonds 4)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 10" (Hearts 10)
      "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 9" (Spades 9) "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 14" (Clubs 14) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 5" (Diamonds 5)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 10" (Hearts 11)
      "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 9" (Spades 10)
      "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 1" (Clubs 1) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 6" (Diamonds 6)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 10" (Hearts 12)
      "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 9" (Spades 11)
      "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 2" (Clubs 2) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 7" (Diamonds 7)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 13" (Hearts 13)
      "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 12" (Spades 12)
      "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 3" (Clubs 3) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 8" (Diamonds 8)
      "Diamonds";
    get_card_suit_test "tesing [get_card_suit] for Hearts 14" (Hearts 14)
      "Hearts";
    get_card_suit_test "tesing [get_card_suit] for Spades 13" (Spades 31)
      "Spades";
    get_card_suit_test "tesing [get_card_suit] for Clubs 4" (Clubs 4) "Clubs";
    get_card_suit_test "tesing [get_card_suit] for Diamonds 9" (Diamonds 9)
      "Diamonds";
    testing_rank "rank test #1: four of a kind"
      [ Spades 4; Diamonds 4; Clubs 4; Spades 10; Spades 8 ]
      [ plc1; plc2; plc3 ]
      [
        ( [ Hearts 4; Diamonds 5 ],
          [ Spades 4; Diamonds 4; Clubs 4; Spades 10; Hearts 4 ],
          "Four of a Kind" );
      ];
    testing_rank "rank test #2: full house"
      [ Spades 4; Diamonds 4; Clubs 5; Spades 10; Spades 8 ]
      [ plc1; plc2; plc3 ]
      [
        ( [ Hearts 4; Diamonds 5 ],
          [ Spades 4; Diamonds 4; Clubs 5; Hearts 4; Diamonds 5 ],
          "Full House" );
      ];
    testing_rank "rank test #3: straight"
      [ Spades 13; Diamonds 12; Clubs 11; Spades 2; Spades 9 ]
      [ plc1; plc2; plc3 ]
      [
        ( [ Hearts 3; Diamonds 10 ],
          [ Spades 13; Diamonds 12; Clubs 11; Spades 9; Diamonds 10 ],
          "Straight" );
      ];
    testing_rank "rank test #4: two pairs"
      [ Clubs 14; Diamonds 2; Clubs 7; Spades 2; Spades 9 ]
      [ plc1; plc2; plc3 ]
      [
        ( [ Hearts 14; Diamonds 12 ],
          [ Clubs 14; Diamonds 2; Spades 2; Hearts 14; Diamonds 12 ],
          "Two Pairs" );
      ];
    testing_rank "rank test #5: high card"
      [ Clubs 2; Diamonds 13; Clubs 11; Spades 6; Spades 9 ]
      [ plc1; plc2 ]
      [
        ( [ Hearts 14; Diamonds 12 ],
          [ Clubs 2; Diamonds 13; Clubs 11; Spades 9; Hearts 14 ],
          "High card" );
      ];
    testing_rank "rank test #6: flush"
      [ Diamonds 2; Diamonds 13; Diamonds 11; Diamonds 6; Spades 9 ]
      [ plc1; plc2; plc3 ]
      [
        ( [ Hearts 14; Diamonds 12 ],
          [ Diamonds 2; Diamonds 13; Diamonds 11; Diamonds 6; Diamonds 12 ],
          "Flush" );
      ];
    testing_rank "rank test #7: royal flush and tie"
      [ Clubs 10; Clubs 11; Clubs 12; Clubs 13; Clubs 14 ]
      [ plc1; plc2 ]
      [
        ( [ Hearts 14; Diamonds 12 ],
          [ Clubs 10; Clubs 11; Clubs 12; Clubs 13; Clubs 14 ],
          "Royal Flush" );
        ( [ Hearts 4; Diamonds 5 ],
          [ Clubs 10; Clubs 11; Clubs 12; Clubs 13; Clubs 14 ],
          "Royal Flush" );
      ];
    testing_rank "rank test #8: pair"
      [ Clubs 3; Spades 7; Hearts 4; Clubs 8; Diamonds 14 ]
      [ plc1; plc2; plc3 ]
      [
        ( [ Hearts 14; Diamonds 12 ],
          [ Spades 7; Clubs 8; Diamonds 14; Hearts 14; Diamonds 12 ],
          "Pair" );
      ];
    testing_rank "rank test #7: 3 of a kind"
      [ Clubs 10; Spades 7; Hearts 4; Clubs 10; Diamonds 14 ]
      [ plc1; plc2; plc3 ]
      [
        ( [ Hearts 3; Diamonds 10 ],
          [ Clubs 10; Spades 7; Clubs 10; Diamonds 14; Diamonds 10 ],
          "Three of a kind" );
      ];
    testing_rank "rank test #7: straight flush"
      [ Spades 5; Clubs 11; Spades 7; Clubs 13; Spades 8 ]
      [ plc1; plc3; [ Spades 6; Spades 9 ] ]
      [
        ( [ Spades 6; Spades 9 ],
          [ Spades 5; Spades 7; Spades 8; Spades 6; Spades 9 ],
          "Straight flush" );
      ];
  ]

let test_suite = "interval test suite" >::: List.flatten [ card_tests ]
let _ = run_test_tt_main test_suite