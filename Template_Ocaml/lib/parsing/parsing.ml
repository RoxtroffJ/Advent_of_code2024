exception Parsing_Failure
type 'a parser = string -> ('a * string)

let load file_name = 
  let file = open_in file_name in

  let rec read res = 
    try
      input_line file |> Dynarray.add_last res;
      read res
    with _ -> close_in file; res
  in

  Dynarray.create () |> read |> Dynarray.to_array


let remove_fst str = String.sub str 1 (String.length str - 1)

let rec parse_word_res ret word text =
  if String.length word == 0 then (ret, text)
  else if String.length text == 0 || word.[0] != text.[0] then raise Parsing_Failure
  else begin
    parse_word_res ret (remove_fst word) (remove_fst text)
  end

let parse_word = parse_word_res ()

let parse_int text = 
  try Scanf.sscanf text "%d%n" (fun a b -> (a, String.sub text b (String.length text - b))) with
  | _ -> raise Parsing_Failure


let combine_parsers parser_a parser_b text = 
  let (a,next) = parser_a text in
  let (b,rest) = parser_b next in
  (a,b),rest

let (+>) = combine_parsers

let try_or parser1 parser2 text = 
  try parser1 text with
  | Parsing_Failure -> parser2 text

let (<|>) = try_or

let find_all parser text = 
  let rec aux text res = 
    if String.length text = 0 then res
    else 
      try
        let (v,next) = parser text in
        v::res |> aux next
      with
      | Parsing_Failure -> aux (remove_fst text) res
  in

  aux text [] |> List.rev

let split_list parser text = 

  let build sep acc = (acc |> List.rev |> List.to_seq |> String.of_seq, sep) in  

  let rec build_one sep acc text = 
    if String.length text == 0 then build sep acc, None
    else try
      let new_sep, next = parser text in
        (acc |> List.rev |> List.to_seq |> String.of_seq, sep),Some(Some new_sep,next)
      with
      | Parsing_Failure -> build_one sep (text.[0]::acc) (remove_fst text)
  in

  let rec build res = function
    | None -> res
    | Some(sep, text) ->
      let a,next = build_one sep [] text in
      build (a::res) next
  in

  build [] (Some(None, text)) |> List.rev
