let (<<) f g x = f (g x)
let rec (--) i j = if i >= j then [] else i :: i + 1 -- j

let flip f x y = f y x

let list_to_string char_list = String.of_seq (List.to_seq char_list)
let array_to_string char_array = String.init (Array.length char_array) (fun i -> char_array.(i))
let string_to_list str = List.init (String.length str) (fun i -> str.[i])
let string_to_array str = Array.init (String.length str) (fun i -> str.[i])

let array2d_from_list list = Array.of_list (List.map string_to_array list)
let array2d_to_string arr =
  let buffer = Buffer.create 16 in
  let rows = Array.length arr in
  let cols = Array.length arr.(0) in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      Buffer.add_char buffer arr.(i).(j);
    done;
    Buffer.add_string buffer "\n";
  done;
  Buffer.contents buffer

let until condition action =
  let rec loop () =
    if not (condition ()) then begin
      action ();
      loop ()
    end
  in
  loop ()

let repeat_array times arr =
  List.init times (fun _ -> arr)
  |> Array.concat

let read_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content