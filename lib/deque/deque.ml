type 'a t = { 
  mutable front: 'a list;
  mutable back: 'a list;
  mutable size: int
}

exception Empty

let create () = { 
  front = []; 
  back = []; 
  size = 0 
}

let is_empty d = d.size = 0
let length d = d.size

let push_front d x =
  d.front <- x :: d.front;
  d.size <- d.size + 1

let push_back d x =
  d.back <- x :: d.back;
  d.size <- d.size + 1

let pop_front d =
  match d.front with
  | x :: xs ->
    d.front <- xs;
    d.size <- d.size - 1;
    x
  | [] ->
    match List.rev d.back with
    | [] -> raise Empty
    | x :: xs ->
      d.front <- xs;
      d.back <- [];
      d.size <- d.size - 1;
      x

let pop_back d =
  match d.back with
  | x :: xs ->
    d.back <- xs;
    d.size <- d.size - 1;
    x
  | [] ->
    match List.rev d.front with
    | [] -> raise Empty
    | x :: xs ->
      d.back <- xs;
      d.front <- [];
      d.size <- d.size - 1;
      x

let from_seq seq =
  let d = create () in
  Seq.iter (push_back d) seq;
  d