(* 定义车次信息的记录类型 *)
type station_info = {
  station_name : string;
  arrival_time : string;
  departure_time : string;
  distance : float;
}

type train_info = {
  train_id : string;
  start_station : string;
  end_station : string;
  departure_time : string;
  ticket_price : float;
  is_operating : bool;
  route : station_info list; (* 停靠站信息 *)
}

(* 定义高铁车次信息的存储 *)
type system = {
  train_list : train_info list;
  passenger_list : Pasnger.passenger list;
}

let add_train system train_info =
  let updated_train_list = train_info :: system.train_list in
  { system with train_list = updated_train_list }

let stop_train system train_id =
  let updated_train_list =
    List.map
      (fun train ->
        if train.train_id = train_id then { train with is_operating = false }
        else train)
      system.train_list
  in
  { system with train_list = updated_train_list }

let output_train train_info =
  Printf.printf "Train ID: %s\n" train_info.train_id;
  Printf.printf "From: %s to %s\n" train_info.start_station
    train_info.end_station;
  Printf.printf "Departure Time: %s\n" train_info.departure_time;
  Printf.printf "Ticket Price: %.2f\n" train_info.ticket_price;
  Printf.printf "Operating: %b\n" train_info.is_operating;

  Printf.printf "Stops:\n";
  List.iter
    (fun stop ->
      Printf.printf "  Station: %s\n" stop.station_name;
      Printf.printf "  Arrival Time: %s\n" stop.arrival_time;
      Printf.printf "  Departure Time: %s\n" stop.departure_time;
      Printf.printf "  Distance: %.2f km\n\n" stop.distance)
    train_info.route

let output_all_trains system = List.iter output_train system.train_list

let parse_bool str =
  match String.lowercase_ascii str with
  | "yes" -> true
  | "no" -> false
  | _ -> failwith "Invalid boolean value"

let parse_train_info lines =
  let rec aux acc lines =
    match lines with
    | [] -> List.rev acc
    | "train_id:" :: train_id :: "start_station:" :: start_station
      :: "end_station:" :: end_station :: "departure_time:" :: departure_time
      :: "ticket_price:" :: ticket_price :: "is_operating:" :: is_operating
      :: "route:" :: rest ->
        let rec parse_route acc lines =
          match lines with
          | "station_name:" :: station_name :: "arrival_time:" :: arrival_time
            :: "departure_time:" :: departure_time :: "distance:" :: distance
            :: rest ->
              let distance_float = float_of_string distance in
              let station =
                {
                  station_name;
                  arrival_time;
                  departure_time;
                  distance = distance_float;
                }
              in
              parse_route (station :: acc) rest
          | _ -> (List.rev acc, lines)
        in
        let route, remaining_lines = parse_route [] rest in
        let train =
          {
            train_id;
            start_station;
            end_station;
            departure_time;
            ticket_price = float_of_string ticket_price;
            is_operating = parse_bool is_operating;
            route;
          }
        in
        aux (train :: acc) remaining_lines
    | _ -> failwith "Invalid train info format"
  in
  aux [] lines

let read_train_info filename =
  let input_channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line input_channel in
      read_lines (line :: acc)
    with End_of_file ->
      close_in input_channel;
      List.rev acc
  in
  let lines = read_lines [] in
  parse_train_info lines
