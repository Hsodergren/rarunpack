open Core

let is_dir path =
  match Sys.is_directory path with
  | `Yes -> true
  | `No | `Unknown -> false

let is_file path =
  match Sys.is_file path with
  | `Yes -> true
  | `No | `Unknown -> false

let rec dir_fold path ~init ~f =
  let all = List.map ~f:(Filename.concat path) (Array.to_list (Sys.readdir path)) in
  let dirs, files = List.filter ~f:is_dir all, List.filter ~f:is_file all in
  let new_val = f init path ~fs:files ~ds:dirs in
  List.fold dirs ~init:new_val ~f:(fun a d -> dir_fold d ~init:a ~f:f)

let f a _ ~fs:_ ~ds:_ =
  a+1


let filename_param =
  Command.Param.(anon ("filename" %: string))

let directory =
  Command.Arg_type.create
    (fun filename ->
       match is_dir filename with
       | true -> filename
       | false -> eprintf "'%s' is not a directory.\n%!" filename;
         exit 1)

let command = Command.basic
    ~summary:"Unpack all rar files found recursivly"
    Command.Let_syntax.(
      let%map_open
        _remove = flag "-r" no_arg ~doc:" remove rar files after unpack"
      and filename = anon ("filename" %: directory)
      in
      fun () ->
        let main () =
          dir_fold filename ~init:0 ~f:f |>
          printf "%d\n"
        in
        main ()
    )

let () = Command.run ~version:"1.0" command
