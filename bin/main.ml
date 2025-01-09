open Ticket_system_lib.Train
open Ticket_system_lib.Pasnger

(** type *)
type menu_option =
  | AddTrain
  | ListTrains
  | StopTrain
  | AddPassenger
  | ListPassengers
  | RemovePassenger
  | Exit

let parse_input input =
  match input with
  | "1" -> Some AddTrain
  | "2" -> Some ListTrains
  | "3" -> Some StopTrain
  | "4" -> Some AddPassenger
  | "5" -> Some ListPassengers
  | "6" -> Some RemovePassenger
  | "7" -> Some Exit
  | _ -> None
;;

let print_menu () =
  print_endline "\nWelcome to the Train & Passenger Management System!";
  print_endline "Choose an option:";
  print_endline "1. Add a new train";
  print_endline "2. List all trains";
  print_endline "3. Stop a train from operation";
  print_endline "4. Add a passenger";
  print_endline "5. List passengers";
  print_endline "6. Remove a passenger";
  print_endline "7. Exit";
  read_line ()
;;

let add_train_interaction system =
  print_endline "Enter train ID:";
  let train_id = read_line () in
  print_endline "Enter start station:";
  let start_station = read_line () in
  print_endline "Enter end station:";
  let end_station = read_line () in
  print_endline "Enter departure time (YYYY-MM-DD HH:MM):";
  let departure_time = read_line () in
  print_endline "Enter ticket price:";
  let ticket_price = float_of_string (read_line ()) in
  print_endline "Is the train operating? (yes/no):";
  let is_operating =
    match read_line () with
    | "yes" -> true
    | _ -> false
  in
  print_endline "Enter route details (type 'done' when finished):";
  let rec collect_stations acc =
    print_endline "Enter station name:";
    match read_line () with
    | "done" -> List.rev acc
    | station_name ->
      print_endline "Enter arrival time:";
      let arrival_time = read_line () in
      print_endline "Enter departure time:";
      let departure_time = read_line () in
      print_endline "Enter distance (in km):";
      let distance = float_of_string (read_line ()) in
      let station = { station_name; arrival_time; departure_time; distance } in
      collect_stations (station :: acc)
  in
  let route = collect_stations [] in
  let train_info =
    { train_id
    ; start_station
    ; end_station
    ; departure_time
    ; ticket_price
    ; is_operating
    ; route
    }
  in
  let updated_system = add_train system train_info in
  print_endline "Train added successfully!";
  updated_system
;;

let list_trains_interaction system =
  if List.length system.train_list = 0
  then print_endline "No trains available."
  else List.iter (fun train -> output_train train) system.train_list
;;

let stop_train_interaction system =
  print_endline "Enter the train ID to stop:";
  let train_id = read_line () in
  let updated_system = stop_train system train_id in
  print_endline "Train operation stopped.";
  updated_system
;;

let add_passenger_interaction system =
  print_endline "Enter passenger ID type:";
  let id_type = read_line () in
  print_endline "Enter passenger ID number:";
  let id_number = read_line () in
  print_endline "Enter passenger name:";
  let name = read_line () in
  print_endline "Enter phone number:";
  let phone = read_line () in
  print_endline "Enter emergency contact:";
  let emergency_contact = read_line () in
  print_endline "Enter emergency contact phone number:";
  let emergency_phone = read_line () in
  let new_passenger =
    { id_type; id_number; name; phone; emergency_contact; emergency_phone }
  in
  let updated_passenger_list = add_passenger system.passenger_list new_passenger in
  print_endline "Passenger added successfully!";
  { system with passenger_list = updated_passenger_list }
;;

let list_passengers_interaction system =
  if List.length system.passenger_list = 0
  then print_endline "No passengers in the system."
  else output_all_passengers system.passenger_list
;;

let remove_passenger_interaction system =
  print_endline "Enter passenger ID number to remove:";
  let id_num = read_line () in
  let updated_list =
    delete_passenger system.passenger_list (fun p -> p.id_number = id_num)
  in
  print_endline "Passenger removed.";
  { system with passenger_list = updated_list }
;;

let rec main_loop system =
  match parse_input (print_menu ()) with
  | Some AddTrain -> main_loop (add_train_interaction system)
  | Some ListTrains ->
    list_trains_interaction system;
    main_loop system
  | Some StopTrain -> main_loop (stop_train_interaction system)
  | Some AddPassenger -> main_loop (add_passenger_interaction system)
  | Some ListPassengers ->
    list_passengers_interaction system;
    main_loop system
  | Some RemovePassenger -> main_loop (remove_passenger_interaction system)
  | Some Exit -> print_endline "Goodbye!"
  | None ->
    print_endline "Invalid choice, please try again.";
    main_loop system
;;

let () =
  let system_with_trains =
    { train_list = read_train_info "data/system.txt"; passenger_list = [] }
  in
  main_loop system_with_trains
;;
