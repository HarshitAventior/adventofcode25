let dial_start = 50

let _raw_input = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n"

let parse_instr line =
  let value = int_of_string (String.sub line 1 (String.length line - 1)) in
  match line.[0] with
  | 'L' ->
      Int.neg value
  | 'R' ->
      value
  | _ ->
      failwith ("Invalid instruction received : " ^ line)

(* Jane street base has a String.split_lines.. useful *)
(* List operations are not lazy by default use seq module for it *)
let parse input : int list =
  String.split_on_char '\n' input
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map parse_instr

let step start move = (start + move) mod 100

let p1 instructions =
  let results =
    List.fold_left
      (fun dial inst -> step (List.hd dial) inst :: dial)
      [dial_start] instructions
  in
  let zeroes = List.filter (fun x -> x = 0) results in
  List.length zeroes

let flatten_instructions inst =
  List.map
    (fun s ->
      match s = Int.abs s with
      | true ->
          List.init s (fun _ -> 1)
      | false ->
          List.init (Int.abs s) (fun _ -> -1) )
    inst
  |> List.flatten

let p2 instructions =
  let results =
    List.fold_left
      (fun dial inst -> step (List.hd dial) inst :: dial)
      [dial_start] instructions
  in
  let zeroes = List.filter (fun x -> x = 0) results in
  List.length zeroes

let () =
  let input = Utils.read_file "./inputs/day1.input" in
  let instructions = parse input in
  Printf.printf "Part 1: %d\n" (p1 instructions) ;
  Printf.printf "Part 2: %d\n" (p2 (flatten_instructions instructions))
