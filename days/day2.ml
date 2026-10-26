open Printf

let input_filename = ref "./inputs/day2.input"

let set_input_file_name filename = input_filename := filename

let parse_entry (entry : string) =
  let range = String.split_on_char '-' entry in
  (List.nth range 0, List.nth range 1)

let parse line = String.split_on_char ',' line |> List.map parse_entry

let rec range s e = if s > e then [] else e :: range s (e - 1)

let expand_range (start, ending) =
  Printf.printf "start = [%s], ending = [%s]\n" start ending ;
  let s = int_of_string start in
  let e = int_of_string ending in
  List.map string_of_int (range s e)

let is_repeating str =
  if String.length str mod 2 <> 0 then false
  else
    let middle = String.length str / 2 in
    let p1 = String.sub str 0 middle in
    let p2 = String.sub str middle middle in
    p1 = p2

(* In part 2 we have to bruteforce lmao *)
(* I am disappointed by the lack of back referencing capabilities in ocaml regexp engines *)
(* ^(\d+)\1+$ *)

let chunks_of n s =
  if n <= 0 then invalid_arg "chunks_of" ;
  List.init
    ((String.length s + n - 1) / n)
    (fun i -> String.sub s (i * n) (min n (String.length s - (i * n))))

let is_perfect_substring str ss =
  let ssl = String.length ss in
  let str_len = String.length str in
  if ssl > str_len then raise (Failure "substring bigger than string")
  else if str_len mod ssl <> 0 then
    (* Doesn't perfectly divide the string *)
    false
  else chunks_of ssl str |> List.for_all (fun c -> c = ss)
(* lmoa I accidentally wrote c = str ) *)

let is_invalid num =
  let mid = String.length num / 2 in
  let repeating_chunks =
    List.init mid (fun i -> i + 1)
    |> List.map (fun l ->
        let name = String.sub num 0 l in
        name )
    |> List.filter (fun ss -> is_perfect_substring num ss)
  in
  List.length repeating_chunks > 0

let () =
  Arg.parse [] set_input_file_name "Enter input filename" ;
  let input = Utils.read_file !input_filename |> String.trim in
  let ranges = parse input in
  (* ranges has type (string * string) list *)
  (* Fmt.pr "%a\n" *)
  (*   (Fmt.list ~sep:Fmt.comma (Fmt.pair Fmt.string Fmt.string)) *)
  (*   ranges ; *)
  let expanded_ranges = List.map expand_range ranges |> List.flatten in
  let repeating_values = List.filter is_repeating expanded_ranges in
  let answer_p1 =
    List.map int_of_string repeating_values |> List.fold_left Int.add 0
  in
  printf "Part 1 = %d\n" answer_p1 ;
  let invalid_values = List.filter is_invalid expanded_ranges in
  let answer_p2 =
    List.map int_of_string invalid_values |> List.fold_left Int.add 0
  in
  printf "Part 2 = %d\n" answer_p2
