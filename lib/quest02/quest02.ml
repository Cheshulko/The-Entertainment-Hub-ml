open Common

module Input = struct
  type content = {
    balloons: char array;
  }

  let parse ?(repeat = 1) input = {
    balloons = 
      input
      |> string_to_array 
      |> repeat_array repeat
  }
end

let fluffbolts = [| 'R'; 'G'; 'B' |]

let play_line (input: Input.content) =
  let rec shoot_line fluffbolt ballons =  
    if (not @@ Deque.is_empty ballons) && Deque.pop_front ballons = fluffbolt then 
        shoot_line fluffbolt ballons
  in 
  let cur_fluffbolt = ref 0 in
  let ballons_left = 
    input.balloons
    |> Array.to_seq
    |> Deque.from_seq 
  in
  let condition () = Deque.is_empty ballons_left in
  let action () = 
    let cur_fluffbolt_ind = !cur_fluffbolt mod (Array.length fluffbolts) in
    shoot_line fluffbolts.(cur_fluffbolt_ind) ballons_left;
    cur_fluffbolt := !cur_fluffbolt + 1
  in until condition action;

  !cur_fluffbolt

let shoot_circle (input: Input.content) = 
  let shoot_circle fluffbolt queue1 queue2 = 
    let ballon = Deque.pop_front queue1 in 
    let ballons_cnt = Deque.length queue1 + Deque.length queue2 in
    if ballons_cnt mod 2 = 1 then
      if ballon = fluffbolt then
        let _ = Deque.pop_front queue2 in
        ()
      else 
        queue2
        |> Deque.pop_front
        |> Deque.push_back queue1
  in
  let cur_fluffbolt = ref 0 in
  let first_half = (Array.length input.balloons + 1) / 2 in
  let queue1 = 
    input.balloons
    |> Array.to_seq
    |> Seq.take first_half
    |> Deque.from_seq 
  in
  let queue2 = 
    input.balloons
    |> Array.to_seq
    |> Seq.drop first_half
    |> Deque.from_seq 
  in
  let condition () = Deque.is_empty queue1 && Deque.is_empty queue2 in
  let action () = 
    let cur_fluffbolt_ind = !cur_fluffbolt mod (Array.length fluffbolts) in
    shoot_circle fluffbolts.(cur_fluffbolt_ind) queue1 queue2;
    cur_fluffbolt := !cur_fluffbolt + 1
  in until condition action;

  !cur_fluffbolt

let part1 = string_of_int << play_line << Input.parse

let part2 ~repeat = string_of_int << shoot_circle << Input.parse ~repeat

let part3 = part2

let rec solvePart part = 
  match part with
  | "tests" -> begin 
    assert (solvePart "part1_sample"   = "7");
    assert (solvePart "part1"          = "131");

    assert (solvePart "part2_sample_1" = "14");
    assert (solvePart "part2_sample_2" = "304");
    assert (solvePart "part2_sample_3" = "1464");
    assert (solvePart "part2_sample_4" = "2955");
    assert (solvePart "part2"          = "21184");

    assert (solvePart "part3"          = "21318362");

    "OK"
  end
  | _ ->  
    part 
    |> Printf.sprintf "lib/quest02/input/%s"
    |> read_file 
    |> match part with 
      | "part1_sample"   -> part1
      | "part1"          -> part1
      | "part2_sample_1" -> (part2 ~repeat: 5)
      | "part2_sample_2" -> (part2 ~repeat: 10)
      | "part2_sample_3" -> (part2 ~repeat: 50)
      | "part2_sample_4" -> (part2 ~repeat: 100)
      | "part2"          -> (part2 ~repeat: 100)
      | "part3"          -> (part3 ~repeat: 100000)
      | _ -> 
        part 
        |> Printf.sprintf "Wrong part '%s' for quest02" 
        |> failwith

let solve part = fun () -> 
  let result = solvePart part in
  Printf.printf "Quest01. Part: %s. Result: %s\n" part result