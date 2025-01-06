(* 定义车次信息的记录类型 *)
type station_info = {
  station_name: string;
  arrival_time: string;
  departure_time: string;
  distance: float;
}

type train_info = {
  train_id: string;
  start_station: string;
  end_station: string;
  departure_time: string;
  ticket_price: float;
  is_operating: bool;
  route: station_info list; (* 停靠站信息 *)
}

(* 定义高铁车次信息的存储 *)
type system = {
  train_list: train_info list;
  passenger_list: 'a . 'a;
}

let add_train system train_info =
  let updated_train_list = train_info :: system.train_list in
  { system with train_list = updated_train_list }

let stop_train system train_id =
  let updated_train_list =
    List.map
      (fun train ->
         if train.train_id = train_id then
           { train with is_operating = false }
         else
           train)
      system.train_list
  in
  { system with train_list = updated_train_list }

let output_train train_info =
  Printf.printf "Train ID: %s\n" train_info.train_id;
  Printf.printf "From: %s to %s\n" train_info.start_station train_info.end_station;
  Printf.printf "Departure Time: %s\n" train_info.departure_time;
  Printf.printf "Ticket Price: %.2f\n" train_info.ticket_price;
  Printf.printf "Operating: %b\n" train_info.is_operating;

  Printf.printf "Stops:\n";
  List.iter (fun stop ->
    Printf.printf "  Station: %s\n" stop.station_name;
    Printf.printf "  Arrival Time: %s\n" stop.arrival_time;
    Printf.printf "  Departure Time: %s\n" stop.departure_time;
    Printf.printf "  Distance: %.2f km\n\n" stop.distance;
  ) train_info.route

(* 解析单个车站信息 *)
let parse_station_info lines =
  let rec aux acc lines =
    match lines with
    | [] -> List.rev acc
    | "station_name:" :: station_name :: "arrival_time:" :: arrival_time ::
        "departure_time:" :: departure_time :: "distance:" :: distance :: rest ->
        let distance_float = float_of_string distance in
        let station = { station_name; arrival_time; departure_time; distance = distance_float } in
        aux (station :: acc) rest
    | _ -> aux acc (List.tl lines)  (* 跳过无效的行 *)
  in
  aux [] lines


(* 解析火车信息 *)
let parse_train_info filename =
  let input_channel = open_in filename in
  let rec aux acc =
    try
      let line = input_line input_channel in
      let lines = ref [line] in
      (* 读取所有相关信息 *)
      while true do
        let next_line = input_line input_channel in
        lines := !lines @ [next_line]
      done;
      aux acc (* 递归解析 *)
    with End_of_file ->
      close_in input_channel;
      acc
  in
  aux []


(* let rec find_train_by_id system train_id =
  for i = 0 to system.index - 1 do
    if system.(i).train_id = train_id then *)

(*
let find_train_by_start_end (system: train_system) (start: string) (end_: string) : train_info list =
  List.filter (fun t -> t.start_station = start && t.end_station = end_) system

let find_train_by_time (system: train_system) (time: string) : train_info list =
  List.filter (fun t -> t.departure_time = time) system *)


