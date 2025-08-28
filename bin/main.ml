let solve quest part = 
  match quest with 
  | "quest01" -> Quest01.solve part ()
  | "quest02" -> Quest02.solve part ()
  | "quest03" -> failwith "unimplemented"
  | _ -> 
    quest 
    |> Printf.sprintf "Wrong quest '%s'. Supported quests: 'quest01', 'quest02', 'quest03'" 
    |> failwith  

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | [_; quest; part] -> begin
    try solve quest part with 
      | Failure message -> Printf.printf "%s\n" message
  end
  | _ -> print_endline "Wrong input. Expected <quest> <part>"