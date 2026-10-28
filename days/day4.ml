let input_filename = ref "./inputs/day4.demo"

let set_input_file_name filename = input_filename := filename

module RollSet = Set.Make (Int)

let cc = ref 0

let rc = ref 0

let build_map_set map =
  List.mapi (fun idx v -> (idx, v)) map
  |> List.filter_map (fun (idx, v) -> if v = '@' then Some idx else None)
  |> RollSet.of_list

let idx_to_coor idx = (idx mod !cc, idx / !cc)

let coord_to_idx (x, y) = (y * !cc) + x

let neighbours idx =
  let x, y = idx_to_coor idx in
  [ (x + 1, y - 1)
  ; (x + 1, y)
  ; (x + 1, y + 1)
  ; (x, y + 1)
  ; (x, y - 1)
  ; (x - 1, y - 1)
  ; (x - 1, y)
  ; (x - 1, y + 1) ]
  |> List.filter (fun (x, y) -> x >= 0 && x < !cc && y >= 0 && y < !rc)
  |> List.map coord_to_idx

(* Accessible = fewer than 4 neighbouring rolls *)
let is_valid_roll map_set idx =
  List.length (List.filter (fun n -> RollSet.mem n map_set) (neighbours idx))
  < 4

let print_roll_set roll_set =
  Fmt.pr "@%a@\n"
    (Fmt.list ~sep:Fmt.semi (Fmt.pair ~sep:Fmt.comma Fmt.int Fmt.int))
    (List.map idx_to_coor (RollSet.to_list roll_set))

let rec part2 roll_set total_removed =
  let valid_rolls = RollSet.filter (is_valid_roll roll_set) roll_set in
  let valid_count = RollSet.cardinal valid_rolls in
  if valid_count = 0 then total_removed
  else part2 (RollSet.diff roll_set valid_rolls) (total_removed + valid_count)

let () =
  Arg.parse [] set_input_file_name ("Usage: " ^ Sys.argv.(0) ^ " [filename]") ;
  let contents = Utils.read_file !input_filename in
  let map =
    String.split_on_char '\n' contents
    |> List.filter (fun line -> String.trim line <> "")
    |> List.map (fun line -> String.to_seq line |> List.of_seq)
  in
  rc := List.length map ;
  cc := List.length (List.hd map) ;
  Fmt.pr "cc=%d\n" !cc ;
  Fmt.pr "rc=%d\n" !rc ;
  let map_set = build_map_set (List.flatten map) in
  (* print_roll_set map_set ; *)
  let valid_rolls = RollSet.filter (is_valid_roll map_set) map_set in
  (* print_roll_set valid_rolls ; *)
  Fmt.pr "Part 1 = %d\n" (RollSet.cardinal valid_rolls) ;
  Fmt.pr "Part 2 = %d\n" (part2 map_set 0)
