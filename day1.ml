#use "common.ml";;

let count_increments lines =
  let rec is_incr acc prev lst = match lst with
    | [] -> acc - 1
    | l :: ls -> if prev < l then is_incr (acc + 1) l ls
      else is_incr acc l ls in

  lines
  |> is_incr 0 0

let rec sliding_window acc lines = match lines with
  | fst :: snd :: thd :: tail -> sliding_window ((fst + snd + thd) :: acc) (snd :: thd :: tail)
  | _ -> List.rev acc

let () =
  read_lines "inputs/1.txt"
  |> List.map int_of_string
  |> count_increments
  |> Printf.printf "Day 1.1 - Increments: %d\n";

  read_lines "inputs/1.txt"
  |> List.map int_of_string
  |> sliding_window []
  |> count_increments
  |> Printf.printf "Day 1.2 - Increments: %d"
