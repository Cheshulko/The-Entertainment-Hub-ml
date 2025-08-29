open Common

module Input = struct
  type state = {
    pulse: int;
    spin: int;
    roll_number: int;
    seed: int;
    position: int;
  }

  type die = {
    faces: int array;
    state: state;
  }

  type content = {
    dies: die array;
    bets: int array array;
  }

  let re = Str.regexp {|\([0-9]+\): faces=\[\([-0-9,]+\)\] seed=\([0-9]+\)|}

  let parse_dies input = try
    input
    |> Str.split (Str.regexp "\n") 
    |> List.map (fun line -> 
      assert (Str.string_match re line 0);

      let faces = line 
        |> Str.matched_group 2 
        |> String.split_on_char ',' 
        |> List.map int_of_string 
        |> Array.of_list in
      let seed = line |> Str.matched_group 3 |> int_of_string in

      {
        faces;
        state = {
          seed;
          spin = 0; 
          pulse = seed; 
          roll_number = 1;
          position = 0; 
        };
      }
    )
    |> Array.of_list
  with
  | Sys_error message -> 
    Printf.printf "Error: %s\n" message; 
    failwith message

  let parse_bets input = input
  |> Str.split (Str.regexp "\n") 
  |> List.map (Array.of_list << List.map (fun x -> x - int_of_char '0') << List.map int_of_char << string_to_list)
  |> Array.of_list

  let parse input = 
    match Str.split (Str.regexp "\n\n") input with 
    | [dies]       -> {dies = parse_dies dies; bets = [||]}
    | [dies; bets] -> {dies = parse_dies dies; bets = parse_bets bets}
    | _ -> failwith "Expected correct input"
end

type game = {
  mutable finishes: int list;
  bet_positions: int array
}

module PairOrd = struct
  type t = int * int
  
  let compare (a1, b1) (a2, b2) =
    match compare a1 a2 with
    | 0 -> compare b1 b2
    | c -> c
end

module PairSet = Set.Make(PairOrd)

let dij = [(0, 0); (0, 1); (0, -1); (1, 0); (-1, 0)]

let die_value (die: Input.die) = die.faces.(die.state.position)

let update_state (state: Input.state) = 
  let spin = state.roll_number * state.pulse in 
  let pulse = (state.pulse + spin) mod state.seed + 1 + state.roll_number + state.seed in
  let roll_number = state.roll_number + 1 in 
  {state with spin; pulse; roll_number}

let play_die (die: Input.die) =
  let state = update_state die.state in
  let position = (die.state.position + state.spin) mod (Array.length die.faces) in
  {die with state = {state with position}}

let play_dies = Array.map play_die

let play_points (input: Input.content) =
  let points = ref 0 in 
  let dies = ref input.dies in 
  let condition () = !points >= 10000 in
  let action () = 
    let next_dies = play_dies !dies in
    let win = Array.fold_right (fun die -> (+) (die_value die)) next_dies 0 in
    points := !points + win;
    dies := next_dies;
  in until condition action;

  string_of_int @@ !dies.(0).state.roll_number - 1

let check_bet_results game dies bets =
  Array.iteri (fun i position -> 
    if position <> Array.length bets then
      let has_matched = die_value dies.(i) = bets.(position) in
      if has_matched then begin
        game.bet_positions.(i) <- game.bet_positions.(i) + 1;
        if game.bet_positions.(i) = Array.length bets then
          game.finishes <- i :: game.finishes;
      end
    ) game.bet_positions

let play_line (input: Input.content) =
  let game = {
    finishes = [];
    bet_positions = Array.init (Array.length input.dies) (fun _ -> 0);
  } in 
  let dies = ref input.dies in 
  let condition () = List.length game.finishes = Array.length input.dies in
  let action () = 
    let next_dies = play_dies !dies in
    (* Part 2 *)
    assert (Array.length input.bets = 1);
    check_bet_results game next_dies input.bets.(0);
    dies := next_dies;
  in until condition action;

  game.finishes
  |> List.rev
  |> List.map ((+) 1)
  |> List.map string_of_int
  |> String.concat ","

let play_die_grid bets seen die =
  let max_i, max_j = Array.length seen, Array.length seen.(0) in
  let add_next next_result (i, j) set = 
    let is_in_bounds (i, j) = i >= 0 && i < max_i && j >= 0 && j < max_j in
    dij
    |> List.map (fun (di, dj) -> i + di, j + dj) 
    |> List.filter is_in_bounds
    |> List.filter (fun (i, j) -> bets.(i).(j) = next_result)
    |> List.fold_left (flip PairSet.add) set
  in
  let update_seen points = PairSet.iter (fun (i, j) -> seen.(i).(j) <- 1) points in
  let die = ref @@ play_die die in
  let initial_die_value = die_value !die in
  let iteration_cnt = ref 0 in 
  let initial_points = Array.(
    bets
    |> mapi (fun i -> mapi (fun j value -> value, (i, j)))
    |> to_list
    |> concat
    |> to_list
    |> List.filter (((=) initial_die_value) << fst)
    |> List.map snd
  ) in
  let cur_points = ref @@ List.fold_left (flip PairSet.add) PairSet.empty initial_points in 
  update_seen !cur_points;
  (* `Genius` stop condition cuz i dont want to find a cycle *)
  let condition () = !iteration_cnt = 100000 in
  let action () = 
    let next_die = play_die !die in
    let next_cur = PairSet.fold (add_next @@ die_value next_die) !cur_points PairSet.empty in
    update_seen next_cur;

    die := next_die;
    cur_points := next_cur;
    iteration_cnt := !iteration_cnt + 1;
  in until condition action
  
let play_grid (input: Input.content) =
  let seen = Array.make_matrix (Array.length input.bets) (Array.length input.bets.(0)) 0 in

  Array.iter (play_die_grid input.bets seen) input.dies;
  Array.(
    seen
    |> fold_left (fun acc row -> acc + fold_left (+) 0 row) 0
    |> string_of_int
  )

let part1 = play_points << Input.parse

let part2 = play_line << Input.parse

let part3 = play_grid << Input.parse

let rec solvePart part = 
  match part with
  | "tests" -> begin 
    assert (solvePart "part1_sample"   = "844");
    assert (solvePart "part1"          = "635");

    assert (solvePart "part2_sample"   = "1,3,4,2");
    assert (solvePart "part2"          = "6,2,8,3,9,5,7,4,1");

    assert (solvePart "part3_sample_1" = "33");
    assert (solvePart "part3_sample_2" = "1125");
    assert (solvePart "part3"          = "157642");

    "OK"
  end
  | _ ->  
    part 
    |> Printf.sprintf "lib/quest03/input/%s"
    |> read_file 
    |> match part with 
      | "part1_sample"   -> part1
      | "part1"          -> part1
      | "part2_sample"   -> part2
      | "part2"          -> part2
      | "part3_sample_1" -> part3
      | "part3_sample_2" -> part3
      | "part3"          -> part3
      | _ -> 
        part 
        |> Printf.sprintf "Wrong part '%s' for quest03" 
        |> failwith

let solve part = fun () -> 
  let result = solvePart part in
  Printf.printf "Quest03. Part: %s. Result: %s\n" part result