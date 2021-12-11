#use "common.ml";;

let split_directions line =
  match String.split_on_char ' ' line with
  | "forward" :: num :: [] -> Either.Left (int_of_string num)
  | "up":: num :: [] -> Either.Right (int_of_string ("-" ^ num))
  | _ :: num :: [] -> Either.Right (int_of_string num)
  | _ -> Either.Left 0

let sum_directions (forward, updown) =
  let f = List.fold_left (+) 0 forward in
  let u = List.fold_left (+) 0 updown in
  (f, u)

(* Part 2 *)
let calculate_dir (forward, aim, depth) line =
  match String.split_on_char ' ' line with
  | "forward" :: num :: _ -> let n = int_of_string num in (forward + n, aim, depth + (aim * n))
  | "up" :: num :: _ -> let n = int_of_string num in (forward, aim - n, depth)
  | "down" :: num :: _ -> let n = int_of_string num in (forward, aim + n, depth)
  | _ -> (forward, aim , depth)


let () =
  read_lines "inputs/2.txt"
  |> List.partition_map split_directions
  |> sum_directions
  |> fun (a, b) -> a * b;
  |> Printf.printf "Day 2.1 - Result: %d\n";

  read_lines "inputs/2.txt"
  |> List.fold_left calculate_dir (0, 0, 0)
  |> fun (a, b, c) -> a * c;
  |> Printf.printf "Day 2.2 - Result: %d"
