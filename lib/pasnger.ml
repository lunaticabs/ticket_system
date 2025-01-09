type passenger =
  { id_type : string
  ; id_number : string
  ; name : string
  ; phone : string
  ; emergency_contact : string
  ; emergency_phone : string
  }

(* Add a passenger to the list *)
let add_passenger (passenger_list : passenger list) (p : passenger) : passenger list =
  p :: passenger_list
;;

(* Delete a passenger when condition is met *)
let delete_passenger (passenger_list : passenger list) (cond : passenger -> bool)
  : passenger list
  =
  List.filter (fun x -> not (cond x)) passenger_list
;;

(* Find a single passenger by condition *)
let find_passenger (passenger_list : passenger list) (cond : passenger -> bool)
  : passenger option
  =
  try Some (List.find cond passenger_list) with
  | Not_found -> None
;;

(* Modify a passenger in the list if condition is met *)
let modify_passenger
      (passenger_list : passenger list)
      (cond : passenger -> bool)
      (update : passenger -> passenger)
  : passenger list
  =
  List.map (fun p -> if cond p then update p else p) passenger_list
;;

(* Print a passenger *)
let output_passenger (p : passenger) =
  Printf.printf
    "ID Type: %s\n\
     ID Number: %s\n\
     Name: %s\n\
     Phone: %s\n\
     Emergency Contact: %s\n\
     Emergency Phone: %s\n\n"
    p.id_type
    p.id_number
    p.name
    p.phone
    p.emergency_contact
    p.emergency_phone
;;

(* Print all passengers *)
let output_all_passengers (passenger_list : passenger list) =
  List.iter output_passenger passenger_list
;;
