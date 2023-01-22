module CircleList = struct
  exception Empty
  exception Invalid_argument

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let add person lst = person :: lst
  let next = function [] -> raise Empty | h :: t -> t @ [ h ]
  let pop = function [] -> raise Empty | _ :: t -> t

  let rec nth lst n =
    match lst with
    | h :: t -> if n = 0 then h else nth t (n - 1)
    | [] -> raise Invalid_argument

  let length lst =
    let rec length_helper acc lst =
      match lst with [] -> acc | h :: t -> length_helper (1 + acc) t
    in
    length_helper 0 lst

  let peek = function [] -> raise Empty | h :: _ -> h
  let rec last = function [] -> raise Empty | [ h ] -> h | _ :: t -> last t

  let rec mem lst p =
    match lst with [] -> false | h :: t -> if h = p then true else mem t p
end
