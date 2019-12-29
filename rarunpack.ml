open Core

let is_dir path p =
  match Sys.is_directory (Filename.concat path p) with
  | `Yes -> true
  | `No | `Unknown -> false

let is_file path p =
  match Sys.is_file (Filename.concat path p) with
  | `Yes -> true
  | `No | `Unknown -> false


let dir_fold path ~_init ~_f =
  let all = List.map (Filename.concat path) (Array.to_list (Sys.readdir path)) in
  let _dirs, _files = List.filter (is_dir path) all, List.filter (is_file path) all in
  "hej"
                      

let () =
  Printf.printf "hej\n"
