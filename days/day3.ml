open Printf

let input_filename = ref "./inputs/day3.demo"

let set_input_file_name filename = input_filename := filename

type joltage = int

let list_max l = List.fold_left Int.max 0 l

let max_joltage (bank : joltage list) =
  let l = List.length bank in
  (* Not searching till extreme end to leave atleast 1 digit for ones place *)
  let left_bank = List.take (l - 1) bank in
  let tens_digit = list_max left_bank in
  (* Searching towards the right side after the max *)
  let right_bank =
    List.drop_while (fun v -> v <> tens_digit) bank |> List.drop 1
  in
  let ones_digit = list_max right_bank in
  (tens_digit * 10) + ones_digit

let rec max_joltage2 bank count =
  let l = List.length bank in
  if l < count then failwith "offset > bank length" ;
  (* Not searching till extreme end to leave atleast 1 digit for ones place *)
  let offset = count - 1 in
  let left_bank = List.take (l - offset) bank in
  let digit = list_max left_bank in
  (* Searching towards the right side after the max *)
  if count - 1 = 0 then digit
  else
    let right_bank =
      List.drop_while (fun v -> v <> digit) bank |> List.drop 1
    in
    (* Not tail recursive.. shameful *)
    (digit * Base.Int.pow 10 offset) + max_joltage2 right_bank (count - 1)

let () =
  Arg.parse [] set_input_file_name ("Usage: " ^ Sys.argv.(0) ^ " [filename]") ;
  let contents = Utils.read_file !input_filename in
  let banks : joltage list list =
    String.split_on_char '\n' contents
    |> List.filter (fun l -> String.trim l <> "")
    |> List.map (fun line ->
        String.to_seq line |> List.of_seq
        |> List.map (fun c -> Char.code c - Char.code '0') )
  in
  let two_joltages = List.map (fun b -> max_joltage2 b 2) banks in
  (* Fmt.pr "@[%a@]\n" (Fmt.list ~sep:Fmt.comma Fmt.int) two_joltages ; *)
  Fmt.pr "Part 1 = %d\n" (List.fold_left Int.add 0 two_joltages) ;
  let twelve_joltages = List.map (fun b -> max_joltage2 b 12) banks in
  (* Fmt.pr "@[%a@]\n" (Fmt.list ~sep:Fmt.comma Fmt.int) twelve_joltages ; *)
  Fmt.pr "Part 2 = %d\n" (List.fold_left Int.add 0 twelve_joltages)
