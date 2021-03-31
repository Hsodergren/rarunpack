let is_kind path =
  let%lwt {Unix.st_kind;_} = Lwt_unix.stat (Fpath.to_string path) in
  match st_kind with
  | Unix.S_DIR -> Lwt.return `Dir
  | Unix.S_REG -> Lwt.return `File
  | _ -> Lwt.return `Unknown

let is_file path = Lwt.map (fun k -> k = `File) (is_kind path)
let is_dir path = Lwt.map (fun k -> k = `Dir) (is_kind path)

let dir_iter ?(n_concur=10) path ~f =
  let pool = Lwt_pool.create n_concur (fun () -> Lwt.return_unit) in
  let rec dir_iter path ~f =
    let%lwt all = Lwt_pool.use pool (fun () ->
        let valid s = String.length s > 0 && String.get s 0 <> '.' in
        Lwt_unix.files_of_directory (Fpath.to_string path)
        |> Lwt_stream.filter_map (fun s ->
            if valid s then Some Fpath.(path / s) else None)
        |> Lwt_stream.to_list
      )
    in
    let%lwt dirs, files = Lwt_list.partition_p is_dir all in
    let this = f path ~fs:files ~ds:dirs in
    let others = Lwt_list.iter_p (fun path -> dir_iter path ~f) dirs in
    Lwt.join [this;others]
  in
  dir_iter path ~f
