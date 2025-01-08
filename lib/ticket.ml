(* 订票信息记录 *)
type ticket_info = {
  order_id : string; (* 订单号 *)
  passenger_name : string; (* 乘车人姓名 *)
  travel_date : string; (* 乘车日期 *)
  start_station : string; (* 起点站 *)
  end_station : string; (* 终点站 *)
  id_number : string; (* 证件号 *)
  departure_time : string; (* 开车时间 *)
  ticket_price : float; (* 票价 *)
  seat_number : int; (* 座位号，从 1 开始 *)
  booking_time : string; (* 订票时间 *)
  seat_class : string; (* 坐席等级，如 "一等座", "二等座" *)
}

(* 链式存储结构：车票节点 *)
type ticket_node = Empty | Node of ticket_info * ticket_node ref

(* 订票：插入一个车票信息 *)
let book_ticket head new_ticket =
  let rec aux node =
    match !node with
    | Empty -> node := Node (new_ticket, ref Empty)
    | Node (_, next) -> aux next
  in
  aux head

(* 退票 *)
let cancel_ticket head condition current_time =
  let rec aux prev node =
    match !node with
    | Empty -> false (* 未找到对应车票 *)
    | Node (ticket, next) ->
        if condition ticket then
          let travel_time =
            (* 解析 travel_date 和 departure_time 并计算差值 *)
            (* 假设存在一个函数 `time_difference` 返回时间差，单位为小时 *)
            (*time_difference*)
            current_time (ticket.travel_date ^ " " ^ ticket.departure_time)
          in
          if travel_time >= 2.0 then (
            (* 免费退票 *)
            prev := !next;
            true)
          else (
            (* 收取 5% 的退票费 *)
            Printf.printf "退票需扣除手续费: %.2f 元\n" (0.05 *. ticket.ticket_price);
            prev := !next;
            true)
        else aux node next
  in
  aux head head

(* 查询车票 *)
let find_ticket head condition =
  let rec aux node =
    match !node with
    | Empty -> None
    | Node (ticket, next) -> if condition ticket then Some ticket else aux next
  in
  aux head

(* 查询余票 *)
let available_tickets head travel_date start_station end_station total_seats =
  let rec aux node count =
    match !node with
    | Empty -> total_seats - count
    | Node (ticket, next) ->
        if
          ticket.travel_date = travel_date
          && ticket.start_station = start_station
          && ticket.end_station = end_station
        then aux next (count + 1)
        else aux next count
  in
  aux head 0

(* 输出所有车票信息 *)
let output_all_tickets head =
  let rec aux node =
    match !node with
    | Empty -> ()
    | Node (ticket, next) ->
        Printf.printf
          "订单号: %s, 姓名: %s, 日期: %s, 起点: %s, 终点: %s, 证件号: %s, 开车时间: %s, 票价: \
           %.2f, 座位: %d, 订票时间: %s, 坐席: %s\n"
          ticket.order_id ticket.passenger_name ticket.travel_date
          ticket.start_station ticket.end_station ticket.id_number
          ticket.departure_time ticket.ticket_price ticket.seat_number
          ticket.booking_time ticket.seat_class;
        aux next
  in
  aux head

(* 打印单张车票 *)
let print_ticket ticket =
  Printf.printf "==== 高铁车票 ====\n";
  Printf.printf "订单号: %s\n" ticket.order_id;
  Printf.printf "姓名: %s\n" ticket.passenger_name;
  Printf.printf "日期: %s\n" ticket.travel_date;
  Printf.printf "起点: %s\n" ticket.start_station;
  Printf.printf "终点: %s\n" ticket.end_station;
  Printf.printf "证件号: %s\n" ticket.id_number;
  Printf.printf "开车时间: %s\n" ticket.departure_time;
  Printf.printf "票价: %.2f\n" ticket.ticket_price;
  Printf.printf "座位号: %d\n" ticket.seat_number;
  Printf.printf "订票时间: %s\n" ticket.booking_time;
  Printf.printf "坐席等级: %s\n" ticket.seat_class;
  Printf.printf "===================\n"
