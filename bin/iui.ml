let () =
    print_endline "hello";
    let system = { Train.train_list = Train.parse_train_info "data/train.txt"; passenger_list = []; } in
        let input1 = input_line stdin in
            match input1 with
            | "1" -> 
                (let input2 = input_line stdin in
                    match input2 with
                    | "output" -> Train.output_all_trains system
                    | _ -> Train.output_all_trains system)
            | _ -> ()
  
        (* print_endline "Input 1 for train management";
        print_endline "Input 2 for passenger management"; *)

