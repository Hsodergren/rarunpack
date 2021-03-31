open Rarunpack
open Cmdliner

let stdout fname = function
  | `New -> Lwt_io.printf "unraring %s\n" fname
  | `Progress p -> Lwt_io.printf "%s - %d%%\n" fname p
  | `Done -> Lwt_io.printf "DONE %s\n" fname
  | `Visit -> Lwt_io.printf "VISITING %s\n" fname

let rec assoc_update key value = function
  | [] -> []
  | (k,v)::tl -> if key = k then (k,value)::tl else (k,v)::assoc_update key value tl

let rec assoc_remove key = function
  | [] -> []
  | (k,_) as hd::tl -> if k = key then tl else hd::assoc_remove key tl

let fancy () =
  let module T = ANSITerminal in
  T.erase T.Screen;
  let longest l =
    List.map fst l
    |> List.map String.length
    |> List.fold_left max 0
  in
  let m = ref [] in
  let show_print updated =
    match updated with
    | `List l ->
      let longest = longest l in
      let rec loop m row :unit=
        match m with
        | [] -> ()
        | (fname,progress)::tl ->
          T.set_cursor 1 row;
          T.erase T.Eol;
          let hashes = progress / 4 in
          Printf.printf "%s | %s | %2d%%\n%!"
            String.(fname ^ (make (longest - length fname)) ' ')
            String.(make hashes '#' ^ make (25 - hashes) ' ')
            progress;
          loop tl (row+1)
      in
      loop l 1
    | `Visit fname ->
      T.set_cursor 1 (List.length !m + 2);
      T.erase T.Eol;
      Printf.printf "Visiting: %s\n%!" fname
  in
  (fun fname ev ->
     (match ev with
     | `New -> m := (fname,0)::!m; show_print (`List !m)
     | `Progress p -> m := assoc_update fname p !m; show_print (`List !m)
     | `Done -> m := assoc_remove fname !m; show_print (`List !m)
     | `Visit -> show_print (`Visit fname));
     Lwt.return_unit)


let f ~pool ~remove ~printer a ~fs ~ds:_ =
  Lwt_pool.use pool (fun () ->
      let%lwt () = printer (Fpath.to_string a) `Visit in
      match Rarfiles.of_list fs with
      | Some r ->
        let%lwt () = Rarfiles.unrar ~f:printer r in
        if remove
        then Rarfiles.remove r
        else Lwt.return_unit
      | None -> Lwt.return_unit)

type printer = Fancy | Stdout [@@deriving show {with_path=false}]



let printer_conv = Arg.conv
    ((fun s ->
       match String.lowercase_ascii s with
       | "fancy"  | "f" -> Ok Fancy
       | "stdout" | "s" -> Ok Stdout
       | _str -> Error (`Msg "invalid printer"))
    , pp_printer)

let run path remove printer concur_unrar concur_dir=
  let printer = match printer with
    | Stdout -> stdout
    | Fancy -> fancy ()
  in
  let pool = Lwt_pool.create concur_unrar (fun () -> Lwt.return_unit) in
  Lwt_main.run (Fs.dir_iter
                  ~n_concur:concur_dir
                  ~f:(f ~pool ~remove ~printer)
                  (Fpath.v path))

let remove =
  let doc = "Should i remove the files after unpacked?" in
  Arg.(value & flag & info ["r";"remove"] ~doc)

let path =
  let doc = "From where should i unpack things recusivly from?" in
  Arg.(required & opt (some dir) None & info ["p";"path"] ~doc)

let concur_unrar =
  let doc = "How many concurrent unrar worker pool should i employ?" in
  Arg.(value & opt int 4 & info ["u";"n-unrar"] ~doc)

let concur_dir =
  let doc = "How many concurrent directory walkers should i employ?" in
  Arg.(value & opt int 10 & info ["d";"n-dir"] ~doc)

let printer =
  let doc = "How should i display progress?" in
  Arg.(value & opt printer_conv Stdout & info ["print"] ~doc)

let cmd =
  let doc = "I unpack rar files" in
  let exits = Term.default_exits in
  Term.(const run $ path $ remove $ printer $ concur_unrar $ concur_dir),
  Term.info "unpacker" ~doc ~exits

let () = Term.exit @@ Term.eval cmd
