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
  route: station_info Data.vector; (* 停靠站信息 *)
}

(* 定义高铁车次信息的存储 *)
type train_system = train_info Data.vector

let add_train train system =
  Data.push train system

let stop_train system train_id = Data.vmap_inplace (fun x ->
  if x.train_id = train_id then {x with is_operating = false} else x)
  system

let output_train train_info =
  Printf.printf "id: %s\n%s to %s\n%s departs\nticket price: %f operating: %b"
    train_info.train_id train_info.start_station train_info.end_station
    train_info.departure_time train_info.ticket_price train_info.is_operating

(* let rec find_train_by_id system train_id =
  for i = 0 to system.index - 1 do
    if system.(i).train_id = train_id then *)

(*
let find_train_by_start_end (system: train_system) (start: string) (end_: string) : train_info list =
  List.filter (fun t -> t.start_station = start && t.end_station = end_) system

let find_train_by_time (system: train_system) (time: string) : train_info list =
  List.filter (fun t -> t.departure_time = time) system *)


