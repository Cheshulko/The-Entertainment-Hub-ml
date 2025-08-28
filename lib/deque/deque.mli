(* 
| Operation    | Worst-case | Amortized |
| ------------ | ---------- | --------- |
| `push_front` | O(1)       | O(1)      |
| `push_back`  | O(1)       | O(1)      |
| `pop_front`  | O(n)       | O(1)      |
| `pop_back`   | O(n)       | O(1)      |
| `is_empty`   | O(1)       | O(1)      |
| `length`     | O(1)       | O(1)      |
| `from_seq`   | O(n)       | O(n)      | 
*)

type 'a t

exception Empty

val create: unit -> 'a t
val is_empty: 'a t -> bool
val length: 'a t -> int

val push_front: 'a t -> 'a -> unit
val push_back: 'a t -> 'a -> unit

val pop_front: 'a t -> 'a
val pop_back: 'a t -> 'a

val from_seq: 'a Seq.t -> 'a t