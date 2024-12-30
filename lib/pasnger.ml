(* 乘客信息记录 *)
type passenger = {
  id_type: string;             (* 证件类别 *)
  id_number: string;           (* 证件号 *)
  name: string;                (* 姓名 *)
  phone: string;               (* 手机号 *)
  emergency_contact: string;   (* 紧急联系人 *)
  emergency_phone: string;     (* 紧急联系人电话 *)
}

(* 链式存储结构：乘客节点 *)
type passenger_node =
  | Empty
  | Node of passenger * passenger_node ref

(* 增加乘客 *)
let add_passenger head new_passenger =
  let rec aux node =
    match !node with
    | Empty -> node := Node (new_passenger, ref Empty)
    | Node (_, next) -> aux next
  in
  aux head

(* 删除乘客 *)
let delete_passenger head condition =
  let rec aux prev node =
    match !node with
    | Empty -> ()  (* 没有匹配到则不操作 *)
    | Node (p, next) ->
        if condition p then
          prev := !next  (* 删除当前节点 *)
        else
          aux node next  (* 继续查找 *)
  in
  aux head head

(* 查询乘客信息 *)
let find_passenger head condition =
  let rec aux node =
    match !node with
    | Empty -> None  (* 未找到 *)
    | Node (p, next) ->
        if condition p then Some p else aux next
  in
  aux head

(* 修改乘客信息 *)
let modify_passenger head condition update =
  let rec aux node =
    match !node with
    | Empty -> false  (* 未找到，返回 false *)
    | Node (p, next) ->
        if condition p then (
          let updated_passenger = update p in
          node := Node (updated_passenger, next);
          true  (* 修改成功 *)
        ) else
          aux next
  in
  aux head

(* 从文件中读取乘客信息 *)
let read_passengers_from_file filename =
  let input_channel = open_in filename in
  let rec aux acc =
    try
      let line = input_line input_channel in
      match String.split_on_char ',' line with
      | [id_type; id_number; name; phone; emergency_contact; emergency_phone] ->
          let passenger = {
            id_type;
            id_number;
            name;
            phone;
            emergency_contact;
            emergency_phone;
          } in
          aux (passenger :: acc)
      | _ -> aux acc
    with End_of_file ->
      close_in input_channel;
      acc
  in
  aux []

(* 输出所有乘客信息 *)
let output_all_passengers head =
  let rec aux node =
    match !node with
    | Empty -> ()
    | Node (p, next) ->
        Printf.printf "证件类别: %s, 证件号: %s, 姓名: %s, 手机号: %s, 紧急联系人: %s, 紧急联系人电话: %s\n"
          p.id_type p.id_number p.name p.phone p.emergency_contact p.emergency_phone;
        aux next
  in
  aux head

  let () =
  (* 初始化链表头 *)
  let head = ref Empty in

  (* 增加乘客 *)
  add_passenger head {
    id_type = "身份证";
    id_number = "123456789012345678";
    name = "张三";
    phone = "13800138000";
    emergency_contact = "李四";
    emergency_phone = "13800138001";
  };

  (* 输出所有乘客信息 *)
  Printf.printf "所有乘客信息:\n";
  output_all_passengers head;

  (* 查询乘客 *)
  match find_passenger head (fun p -> p.name = "张三") with
  | Some p -> Printf.printf "找到乘客: %s\n" p.name
  | None -> Printf.printf "未找到乘客\n";

  (* 修改乘客信息 *)
  let updated = modify_passenger head (fun p -> p.name = "张三") (fun p -> { p with phone = "13900139000" }) in
  Printf.printf "修改成功: %b\n" updated;

  (* 删除乘客 *)
  delete_passenger head (fun p -> p.name = "张三");

  (* 再次输出所有乘客信息 *)
  Printf.printf "所有乘客信息:\n";
  output_all_passengers head;

(*
身份证,123456789012345678,张三,13800138000,李四,13800138001
护照,AB12345678,王五,13800138002,赵六,13800138003
身份证,987654321098765432,李四,13800138004,张三,13800138005
 *)