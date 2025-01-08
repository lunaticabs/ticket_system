open Ticket_system_lib.Train

(* 定义菜单选项 *)
type menu_option =
  | AddTrain
  | ListTrains
  | StopTrain
  | Exit

(* 解析用户输入 *)
let parse_input input =
  match input with
  | "1" -> Some AddTrain
  | "2" -> Some ListTrains
  | "3" -> Some StopTrain
  | "4" -> Some Exit
  | _ -> None

(* 打印菜单 *)
let print_menu () =
  print_endline "\nWelcome to the Train Management System!";
  print_endline "Choose an option:";
  print_endline "1. Add a new train";
  print_endline "2. List all trains";
  print_endline "3. Stop a train from operation";
  print_endline "4. Exit";
  read_line ()

(* 从文件加载火车信息 *)
let load_trains_from_file filename =
  read_train_info filename

(* 添加列车交互逻辑 *)
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
  let is_operating = match read_line () with
    | "yes" -> true
    | "no" -> false
    | _ -> false
  in
  print_endline "Enter route details (type 'done' when finished):";

  (* 收集停靠站信息 *)
  let rec collect_stations acc =
    print_endline "Enter station name:";
    let station_name = read_line () in
    if station_name = "done" then List.rev acc
    else
      begin
        print_endline "Enter arrival time:";
        let arrival_time = read_line () in
        print_endline "Enter departure time:";
        let departure_time = read_line () in
        print_endline "Enter distance (in km):";
        let distance = float_of_string (read_line ()) in
        let station = { station_name; arrival_time; departure_time; distance } in
        collect_stations (station :: acc)
      end
  in
  let route = collect_stations [] in
  let train_info = { train_id; start_station; end_station; departure_time; ticket_price; is_operating; route } in
  let updated_system = add_train system train_info in
  print_endline "Train added successfully!";
  updated_system

(* 列出所有列车信息 *)
let list_trains_interaction system =
  if List.length system.train_list = 0 then
    print_endline "No trains available."
  else
    List.iter (fun train -> output_train train) system.train_list

(* 停止列车运营 *)
let stop_train_interaction system =
  print_endline "Enter the train ID to stop:";
  let train_id = read_line () in
  let updated_system = stop_train system train_id in
  print_endline "Train operation stopped.";
  updated_system

(* 递归实现的主循环 *)
let rec main_loop system =
  let input = print_menu () in
  match parse_input input with
  | Some AddTrain -> 
      let updated_system = add_train_interaction system in
      main_loop updated_system
  | Some ListTrains -> 
      list_trains_interaction system;
      main_loop system
  | Some StopTrain -> 
      let updated_system = stop_train_interaction system in
      main_loop updated_system
  | Some Exit -> print_endline "Goodbye!"
  | None ->
      print_endline "Invalid choice, please try again.";
      main_loop system

(* 程序入口 *)
let () =
  (* 从文件加载火车数据 *)
  let system_with_trains = { train_list = load_trains_from_file "data/train.txt"; passenger_list = [] } in
  if Sys.file_exists "data/train.txt" then
    print_endline "File found"
  else
    print_endline "File not found";
  main_loop system_with_trains