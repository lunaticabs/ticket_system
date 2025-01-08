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

(* Example usage *)
let () =
  Printf.printf "Using normal list to store passengers.\n\n";
  let passengers = [] in
  let passengers =
    add_passenger
      passengers
      { id_type = "身份证"
      ; id_number = "123456789012345678"
      ; name = "张三"
      ; phone = "13800138000"
      ; emergency_contact = "李四"
      ; emergency_phone = "13800138001"
      }
  in
  (* Output all passengers *)
  Printf.printf "All passengers:\n";
  output_all_passengers passengers;
  (* Find a passenger *)
  match find_passenger passengers (fun p -> p.name = "张三") with
  | Some p -> Printf.printf "Found passenger: %s\n\n" p.name
  | None ->
    Printf.printf "Passenger not found\n\n";
    (* Modify a passenger *)
    let passengers =
      modify_passenger
        passengers
        (fun p -> p.name = "张三")
        (fun p -> { p with phone = "13900139000" })
    in
    (* Add another passenger *)
    let passengers =
      add_passenger
        passengers
        { id_type = "护照"
        ; id_number = "AB12345678"
        ; name = "王五"
        ; phone = "13800138002"
        ; emergency_contact = "赵六"
        ; emergency_phone = "13800138003"
        }
    in
    (* Output all passengers after modifications *)
    Printf.printf "All passengers after modifications:\n";
    output_all_passengers passengers;
    (* Delete a passenger *)
    let passengers = delete_passenger passengers (fun p -> p.name = "张三") in
    Printf.printf "All passengers after deleting 张三:\n";
    output_all_passengers passengers
;;
