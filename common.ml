let read_lines path =
  let ic = open_in path in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop lines = match try_read () with
    | Some line -> loop (line :: lines)
    | None -> close_in ic; List.rev lines in
  loop []
