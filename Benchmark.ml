open Unix

(* Function to create a random board *)
let create_board n p =
  Array.init n (fun _ ->
      Array.init n (fun _ ->
          Random.self_init ();
          if Random.float 1.0 < p then 1 else 0
        ));;
        
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

(* Function to find the valid neighbors of a node *)
let rec valid_neighbors_4_connectivity graph node =
  let i = fst node in
  let j = snd node in
  let neighbors = [(i+1,j);(i,j+1);(i,j-1);(i-1,j)] in
  List.filter (fun x -> if fst x >= 0 && fst x < Array.length graph && snd x >= 0 && snd x < Array.length graph.(0) && graph.(fst x).(snd x) = 0 then true else false) neighbors;;

(* Function to find the connected components of a graph *)
let rec dfs_algorithm_4_connectivity graph visited node =
  let neighbors = valid_neighbors_4_connectivity graph node in
  let all_visited = node::visited in
  let unvisited_neighbors = List.filter (fun x -> not (List.mem x all_visited)) neighbors in
  List.fold_left (fun all_visited neighbor -> if not (List.mem neighbor all_visited) && not (path_exists_rows graph all_visited) then dfs_algorithm_4_connectivity graph all_visited neighbor else all_visited) all_visited unvisited_neighbors;;

(* Function to check if there is a path from the first row to the last row *)
  let rec run_dfs_first_row_4_connectivity graph node =
    if snd node >= Array.length graph.(0) then false
    else if graph.(fst node).(snd node) = 0 && path_exists_rows graph (dfs_algorithm_4_connectivity graph [] node) then true
    else run_dfs_first_row_4_connectivity graph (fst node, snd node + 1);;

let graph = create_board 500 0.8;;
(* Function to print the board *)

(* print_board graph;; *)

let t1 = Unix.gettimeofday () in

let b = run_dfs_first_row_4_connectivity graph (0,0) in

let t2 = Unix.gettimeofday () in

Printf.printf "Time: %f" (t2 -. t1);;



