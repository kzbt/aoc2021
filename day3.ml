#use "common.ml";;

let most_common_bits state line =
  let s = String.to_seq line in
  let s = Seq.map (fun c -> if c == '0' then (1, 0) else (0, 1)) s in
  let s = List.of_seq s in
  List.map2 (fun (s0, s1) (t0, t1) -> (s0 + t0, s1 + t1)) state s

let build_gamma_epsilon state =
  let choose_bit str (a, b) = if a > b then str ^ "0" else str ^ "1" in
  let gamma = List.fold_left choose_bit "" state in
  let epsilon = String.map (fun c -> if c == '1' then '0' else '1') gamma in
  (int_of_string ("0b" ^ gamma), int_of_string ("0b" ^ epsilon))

let () =
  read_lines "inputs/3.txt"
  |> List.fold_left most_common_bits (List.init 12 (Fun.const (0, 0)))
  |> build_gamma_epsilon
  |> fun (g, e) -> g * e;
  |> Printf.printf "Day 3.1 - Result: %d\n"
