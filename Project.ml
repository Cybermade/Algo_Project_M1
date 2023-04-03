open Owl
open Plplot

(*
White cell = 1
Black cell = 0
*)

(* Function to create a random board *)
let create_board n p =
  Array.init n (fun _ ->
      Array.init n (fun _ ->
          if Random.float 1.0 < p then 1 else 0
        ));;

(* Function to print the board *)
let rec print_board graph =
  print_string "\n\n";
  Array.iter (fun row -> 
    Array.iter (fun cell -> 
      if cell = 1 then print_string "1\t"
      else print_string "0\t"
    ) row;
    print_newline ()
  ) graph;
  print_string "\n\n";;

(* Function to check if there is a path from the first row to the last row *)
let rec path_exists_rows graph visited =
  match visited with
  | [] -> false
  | hd::tl -> if fst hd = Array.length graph - 1 then true else path_exists_rows graph tl;;


(* Function to check if there is a path from the first column to the last column *)
let rec path_exists_columns graph visited =
  match visited with
  | [] -> false
  | hd::tl -> if snd hd = Array.length graph.(0) - 1 then true else path_exists_columns graph tl;;


let rec print_visited visited =
  match visited with
  | [] -> ()
  | hd::tl -> print_string "("; print_int (fst hd); print_string ","; print_int (snd hd); print_string ")"; print_visited tl;;

let rec valid_neighbors_4_connectivity graph node =
  let i = fst node in
  let j = snd node in
  let neighbors = [(i-1,j);(i+1,j);(i,j-1);(i,j+1)] in
  List.filter (fun x -> if fst x >= 0 && fst x < Array.length graph && snd x >= 0 && snd x < Array.length graph.(0) && graph.(fst x).(snd x) = 0 then true else false) neighbors;;

let rec valid_neighbors_8_connectivity graph node =
  let i = fst node in
  let j = snd node in
  let neighbors = [(i-1,j);(i+1,j);(i,j-1);(i,j+1);(i-1,j-1);(i-1,j+1);(i+1,j-1);(i+1,j+1)] in
  List.filter (fun x -> if fst x >= 0 && fst x < Array.length graph && snd x >= 0 && snd x < Array.length graph.(0) && graph.(fst x).(snd x) = 0 then true else false) neighbors;;


let rec valid_neighbors_4_connectivity_white graph node =
  let i = fst node in
  let j = snd node in
  let neighbors = [(i-1,j);(i+1,j);(i,j-1);(i,j+1)] in
  List.filter (fun x -> if fst x >= 0 && fst x < Array.length graph && snd x >= 0 && snd x < Array.length graph.(0) && graph.(fst x).(snd x) = 1 then true else false) neighbors;;

let rec valid_neighbors_8_connectivity_white graph node =
  let i = fst node in
  let j = snd node in
  let neighbors = [(i-1,j);(i+1,j);(i,j-1);(i,j+1);(i-1,j-1);(i-1,j+1);(i+1,j-1);(i+1,j+1)] in
  List.filter (fun x -> if fst x >= 0 && fst x < Array.length graph && snd x >= 0 && snd x < Array.length graph.(0) && graph.(fst x).(snd x) = 1 then true else false) neighbors;;

let rec dfs_algorithm_4_connectivity graph visited node =
  let neighbors = valid_neighbors_4_connectivity graph node in
  let all_visited = node::visited in
  let unvisited_neighbors = List.filter (fun x -> not (List.mem x all_visited)) neighbors in
  List.fold_left (fun all_visited neighbor -> if not (List.mem neighbor all_visited) && not (path_exists_rows graph all_visited) then dfs_algorithm_4_connectivity graph all_visited neighbor else all_visited) all_visited unvisited_neighbors;;

let rec dfs_algorithm_8_connectivity graph visited node =
  let neighbors = valid_neighbors_8_connectivity graph node in
  let all_visited = node::visited in
  let unvisited_neighbors = List.filter (fun x -> not (List.mem x all_visited)) neighbors in
  List.fold_left (fun all_visited neighbor -> if not (List.mem neighbor all_visited) && not (path_exists_rows graph all_visited) then dfs_algorithm_8_connectivity graph all_visited neighbor else all_visited) all_visited unvisited_neighbors;;

let rec dfs_algorithm_4_connectivity_white graph visited node =
  let neighbors = valid_neighbors_4_connectivity_white graph node in
  let all_visited = node::visited in
  let unvisited_neighbors = List.filter (fun x -> not (List.mem x all_visited)) neighbors in
  List.fold_left (fun all_visited neighbor -> if not (List.mem neighbor all_visited) && not (path_exists_columns graph all_visited) then dfs_algorithm_4_connectivity_white graph all_visited neighbor else all_visited) all_visited unvisited_neighbors;;

let rec dfs_algorithm_8_connectivity_white graph visited node =
  let neighbors = valid_neighbors_8_connectivity_white graph node in
  let all_visited = node::visited in
  let unvisited_neighbors = List.filter (fun x -> not (List.mem x all_visited)) neighbors in
  List.fold_left (fun all_visited neighbor -> if not (List.mem neighbor all_visited) && not (path_exists_columns graph all_visited) then dfs_algorithm_8_connectivity_white graph all_visited neighbor else all_visited) all_visited unvisited_neighbors;;




let rec run_dfs_first_row_4_connectivity graph node =
  if snd node >= Array.length graph.(0) then false
  else if graph.(fst node).(snd node) = 0 && path_exists_rows graph (dfs_algorithm_4_connectivity graph [] node) then true
  else run_dfs_first_row_4_connectivity graph (fst node, snd node + 1);;

let rec run_dfs_first_row_8_connectivity graph node =
  if snd node >= Array.length graph.(0) then false
  else if graph.(fst node).(snd node) = 0 && path_exists_rows graph (dfs_algorithm_8_connectivity graph [] node) then true
  else run_dfs_first_row_8_connectivity graph (fst node, snd node + 1);;

let rec run_dfs_first_column_4_connectivity_white graph node =
  if fst node >= Array.length graph then false
  else if graph.(fst node).(snd node) = 1 && path_exists_columns graph (dfs_algorithm_4_connectivity_white graph [] node) then true
  else run_dfs_first_column_4_connectivity_white graph (fst node + 1, snd node);;


let rec run_dfs_first_column_8_connectivity_white graph node =
  if fst node >= Array.length graph then false
  else if graph.(fst node).(snd node) = 1 && path_exists_columns graph (dfs_algorithm_8_connectivity_white graph [] node) then true
  else run_dfs_first_column_8_connectivity_white graph (fst node + 1, snd node);;




let bfs_algorithm_4_connectivity graph node =
let q = Queue.create () in
Queue.push node q;
let rec aux q visited =
  if Queue.is_empty q || path_exists_rows graph visited then visited
  else
    let current_node = Queue.pop q in
    if (fst current_node) = (Array.length graph - 1) then visited
    else
      let neighbors = valid_neighbors_4_connectivity graph current_node in
      let unvisited_neighbors = List.filter (fun x -> not (List.mem x visited)) neighbors in
      List.iter (fun x -> Queue.push x q) unvisited_neighbors;
      aux q ( visited @ unvisited_neighbors)
in aux q [node];;

let bfs_algorithm_8_connectivity graph node =
let q = Queue.create () in
Queue.push node q;
let rec aux q visited =
  if Queue.is_empty q || path_exists_rows graph visited then visited
  else
    let current_node = Queue.pop q in
    if (fst current_node) = (Array.length graph - 1) then visited
    else
      let neighbors = valid_neighbors_8_connectivity graph current_node in
      let unvisited_neighbors = List.filter (fun x -> not (List.mem x visited)) neighbors in
      List.iter (fun x -> Queue.push x q) unvisited_neighbors;
      aux q ( visited @ unvisited_neighbors)
in aux q [node];;
    
(* 
let graph = create_board 200 0.;
let visited = dfs_algorithm_4_connectivity graph [] (0,0);;
let visited2 = bfs_algorithm_4_connectivity graph (0,0);;

print_visited visited;;
print_string "\n\n";;
print_visited visited2;;

Printf.printf "length of visited: %d\n" (List.length visited);;
Printf.printf "length of visited2: %d\n" (List.length visited2);;*)
  
type data = {p : float; n : int; percentage : float};;

  
let rec run_bfs_first_row_4_connectivity graph node =
  if snd node >= Array.length graph.(0) then false
  else if graph.(fst node).(snd node) = 0 && path_exists_rows graph (bfs_algorithm_4_connectivity graph node) then true
  else run_bfs_first_row_4_connectivity graph (fst node, snd node + 1);;

let rec run_bfs_first_row_8_connectivity graph node =
  if snd node >= Array.length graph.(0) then false
  else if graph.(fst node).(snd node) = 0 && path_exists_rows graph (bfs_algorithm_8_connectivity graph node) then true
  else run_bfs_first_row_8_connectivity graph (fst node, snd node + 1);;


  
let rec statistic_4_connectivity n p number_of_times =
  Printf.printf "%d %f \n" n p; 
  let rec aux number_of_times acc =
    if number_of_times = 0 then acc
    else
      let graph = create_board n p in
      let result = run_dfs_first_row_4_connectivity graph (0,0) in
      if result then aux (number_of_times - 1) (acc + 1)
      else aux (number_of_times - 1) acc
  in
  let number_of_times_with_path = aux number_of_times 0 in
  {p = p; n = n; percentage = (float_of_int number_of_times_with_path) /. (float_of_int number_of_times) *. 100.0};;

let rec statistic_8_connectivity n p number_of_times =
  let rec aux number_of_times acc =
    if number_of_times = 0 then acc
    else
      let graph = create_board n p in
      let result = run_dfs_first_row_8_connectivity graph (0,0) in
      if result then aux (number_of_times - 1) (acc + 1)
      else aux (number_of_times - 1) acc
  in
  let number_of_times_with_path = aux number_of_times 0 in
  {p = p; n = n; percentage = (float_of_int number_of_times_with_path) /. (float_of_int number_of_times) *. 100.0};;


let rec statistic_4_connectivity_white n p number_of_times =
  let rec aux number_of_times acc =
    if number_of_times = 0 then acc
    else
      let graph = create_board n p in
      let result = run_dfs_first_column_4_connectivity_white graph (0,0) in
      if result then aux (number_of_times - 1) (acc + 1)
      else aux (number_of_times - 1) acc
  in
  let number_of_times_with_path = aux number_of_times 0 in
  {p = p; n = n; percentage = (float_of_int number_of_times_with_path) /. (float_of_int number_of_times) *. 100.0};;

let rec statistic_8_connectivity_white n p number_of_times =
  let rec aux number_of_times acc =
    if number_of_times = 0 then acc
    else
      let graph = create_board n p in
      let result = run_dfs_first_column_8_connectivity_white graph (0,0) in
      if result then aux (number_of_times - 1) (acc + 1)
      else aux (number_of_times - 1) acc
  in
  let number_of_times_with_path = aux number_of_times 0 in
  {p = p; n = n; percentage = (float_of_int number_of_times_with_path) /. (float_of_int number_of_times) *. 100.0};;

let n_values = [5;10;20;50;100;200;400];;
let p_values = [0.0;0.1;0.2;0.3;0.4;0.5;0.6;0.7;0.8;0.9;1.0];;

    


let apply_func_to_pairs func n_values p_values =
  List.concat (
    List.map (fun x ->
      List.map (fun y ->
        let result = func x y 1000 in
        Printf.printf "Processing pair (%d, %d)\n" x y; (* Add print statement here *)
        result
      ) p_values
    ) n_values
  );;



let print_result result =
  List.iter (fun x -> print_string (string_of_float x.p);print_string "\t";print_string (string_of_int x.n);print_string "\t";print_string (string_of_float x.percentage);print_string "\n") result;;





let plot_scatter data_list title=
  (* Extract x and y values from the data list *)
  
  
  (* Initialize the PLplot library *)
  plinit ();
  
  let x_max = (float_of_int (List.fold_left (fun acc x -> if x.n > acc then x.n else acc) 0 data_list)) *. 1.1 in
  let y_max = List.fold_left (fun acc x -> if x.p > acc then x.p else acc) 0.0 data_list +. 0.1 in
  
  
  
  (* Define the RGB values for the colors *)
  
  let red = Array.init 103 (fun i -> if i = 101 then 255 else if i = 102 then 0 else int_of_float (float_of_int (100 - i) *. 2.55)) in
  let green = Array.init 103 (fun i -> if i = 101 then 255 else if i = 102 then 0 else int_of_float (float_of_int i *. 2.55)) in
  let blue = Array.init 103 (fun i -> if i = 101 then 255 else 0) in
  

  (* Set up the color map with the four colors *)
  plscmap0 red green blue;
  
  (* Set up the plot window *)
  plcol0 101;
  plenv 0.0 x_max (-0.1) y_max 0 0;
  plcol0 101;
  pllab "n" "p" title;
  
  
  (* Draw the scatter plot *)
  
  let rec loop data_list =
    match data_list with
    | [] -> ()
    | {n; p; percentage} :: t ->
      let color = int_of_float (percentage) in
      plcol0 color;
      plptex (float_of_int n) p 1.0 0.0 0.5 (string_of_float percentage);
      loop t
  in
  loop data_list;
  plscolbga 100 100 100 0.5;

  
  (* Finish the plot *)
  plend ();;

let result_4_connectivity = apply_func_to_pairs statistic_4_connectivity n_values p_values;;
plot_scatter result_4_connectivity "4-connectivity";;

let result_8_connectivity = apply_func_to_pairs statistic_8_connectivity n_values p_values;;
plot_scatter result_8_connectivity "8-connectivity";;

let result_4_connectivity_white = apply_func_to_pairs statistic_4_connectivity_white n_values p_values;;
plot_scatter result_4_connectivity_white "4-connectivity white cells";;

let result_8_connectivity_white = apply_func_to_pairs statistic_8_connectivity_white n_values p_values;;
plot_scatter result_8_connectivity_white "8-connectivity white cells";;






