(* Define vector (dynamic array) *)
type 'a vector =
  { compacity : int
  ; mutable index : int
  ; buffer : 'a array
  }

(* create a new vector *)
let create com init = { compacity = com; index = 0; buffer = Array.make com init }

(* function to enlarge the vector to 2 times larger *)
let enlarge vec =
  { compacity = 2 * vec.compacity
  ; index = vec.index
  ; buffer =
      (let new_vec = Array.make (2 * vec.compacity) vec.buffer.(0) in
       Array.blit vec.buffer 0 new_vec 0 vec.compacity;
       new_vec)
  }
;;

(* add new value to vector, and return the new vector *)
let rec push x vec =
  if vec.index >= vec.compacity
  then push x (enlarge vec)
  else (
    vec.buffer.(vec.index) <- x;
    vec.index <- vec.index + 1;
    vec)
;;

(* a map for vector, make it acts like a functor *)
let vmap f vec =
  if vec.index = 0
  then { compacity = 0; index = 0; buffer = [||] }
  else (
    let r = create vec.compacity (f vec.buffer.(0)) in
    r.index <- vec.index;
    for i = 0 to vec.index - 1 do
      r.buffer.(i) <- f vec.buffer.(i)
    done;
    r)
;;

(* map inplace version *)
let vmap_inplace f vec =
  for i = 0 to vec.index - 1 do
    Array.set vec.buffer i (f (Array.get vec.buffer i))
  done
;;
