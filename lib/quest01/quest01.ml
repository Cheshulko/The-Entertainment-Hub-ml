open Common

module Input = struct
  type content = {
    pattern: char array array;
    tokens: char array array;
  }

  let pattern_rows_cnt pattern = Array.length pattern
  let pattern_cols_cnt pattern = Array.length pattern.(0)
  let max_position pattern = 
    let columns = pattern_cols_cnt pattern in
    (columns + 1) / 2

  let parse input = try let pattern, tokens = 
    match Str.split (Str.regexp "\n\n") input with 
    | [pattern; tokens] -> begin 
      let pattern_rows = String.split_on_char '\n' pattern in
      let tokens_rows  = String.split_on_char '\n' tokens  in

      array2d_from_list pattern_rows, 
      array2d_from_list tokens_rows
    end
    | _ -> failwith "Expected correct input" in

    {pattern; tokens}
  with
  | Sys_error message -> 
    Printf.printf "Error: %s\n" message; 
    failwith message
end

let play_token pattern column token = 
  let cur_position = ref (0, column) in
  let cur_nail_ind = ref 0 in
  let iteration = fun position nail_index pattern_row ->
    let row, col = position in
    let rec turn_left position =
      if col = 0 then
        turn_right position
      else 
        row, col - 1
    and turn_right position =
      if col + 1 = Input.pattern_cols_cnt pattern then
        turn_left position
      else 
        row, col + 1
    in
    match pattern_row.(col) with
    | '.' -> (row + 1, col), nail_index;
    | '*' -> begin 
      match token.(nail_index) with 
      | 'L' -> turn_left position, nail_index + 1;
      | 'R' -> turn_right position, nail_index + 1;
      | _ -> failwith "Unreachable!"
    end
    | _ -> failwith "Unreachable!"
  in 
  let condition () = fst !cur_position = Input.pattern_rows_cnt pattern in
  let action () = 
    let next_position, next_nail_ind = iteration 
      !cur_position 
      !cur_nail_ind 
      pattern.(fst !cur_position) 
    in
    cur_position := next_position;
    cur_nail_ind := next_nail_ind
  in
  until condition action;

  let win = 2 * ((snd !cur_position) / 2 + 1) - (column / 2 + 1) in
  max win 0

let play (input: Input.content) = 
  input.tokens
  |> Array.mapi (fun index -> play_token input.pattern (2 * index))
  |> Array.fold_left ((+)) 0

let part1 = string_of_int << play << Input.parse

let part2 file_content = 
  let input = Input.parse file_content in
  let max_position = Input.max_position input.pattern in
  input.tokens
  |> Array.map (
    fun token ->
      (0 -- max_position)
      |> List.map (fun index -> play_token input.pattern (2 * index) token) 
      |> List.fold_left max 0
  ) 
  |> Array.fold_left (+) 0
  |> string_of_int

let part3 file_content = 
  let input = Input.parse file_content in
  let max_position = Input.max_position input.pattern in
  let positions = Array.init max_position (fun _ -> -1) in
  let rec place_rec cur_token_ind = 
    if cur_token_ind = Array.length input.tokens then
      (* All tokens have been placed *)
      let win =
        positions
        |> Array.to_list
        |> List.mapi (fun position token_ind -> token_ind, position)
        |> List.filter_map (fun (token_ind, position) -> 
          if token_ind <> -1 then 
            Some (token_ind, position)
          else
            None 
        )
        |> List.sort Stdlib.compare
        |> List.map (fun (token_index, position) -> 
          play_token input.pattern (2 * position) input.tokens.(token_index)
        )
        |> List.fold_left (+) 0 
      in      
      (win, win)

    else
      (* Place next token *)
      (0 -- Array.length positions)
      |> List.filter_map (fun position -> 
        if positions.(position) = -1 then 
          Some position
        else
          None
      ) 
      |> List.map (fun position -> 
        positions.(position) <- cur_token_ind;
        let mi_ma = place_rec (cur_token_ind + 1) in
        positions.(position) <- -1;
        mi_ma
      ) 
      |> List.fold_left (fun (cur_mi, cur_ma) (mi, ma) -> 
        min cur_mi mi,
        max cur_ma ma
      ) (Stdlib.max_int, Stdlib.min_int)
  in
  let row, col = place_rec 0 in
  Printf.sprintf "%d %d" row col

let rec solvePart part = 
  match part with
  | "tests" -> begin 
    assert (solvePart "part1_sample"   = "26");
    assert (solvePart "part1"          = "42");

    assert (solvePart "part2_sample"   = "115");
    assert (solvePart "part2"          = "1116");

    assert (solvePart "part3_sample_1" = "13 43");
    assert (solvePart "part3_sample_2" = "25 66");
    assert (solvePart "part3_sample_3" = "39 122");
    assert (solvePart "part3"          = "26 104");

    "OK"
  end
  | _ ->  
    part 
    |> Printf.sprintf "lib/quest01/input/%s"
    |> read_file 
    |> match part with 
      | "part1_sample"   -> part1
      | "part1"          -> part1
      | "part2_sample"   -> part2
      | "part2"          -> part2
      | "part3_sample_1" -> part3
      | "part3_sample_2" -> part3
      | "part3_sample_3" -> part3
      | "part3"          -> part3
      | _ -> 
        part 
        |> Printf.sprintf "Wrong part '%s' for quest01" 
        |> failwith

let solve part = fun () -> 
  let result = solvePart part in
  Printf.printf "Quest01. Part: %s. Result: %s\n" part result