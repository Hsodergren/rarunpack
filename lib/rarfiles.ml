type t = {
  all: Fpath.t list;
  rar: Fpath.t;
}

type ev = [`New | `Progress of int | `Done]

let digits = Re.(alt [repn digit 1 (Some 2); str "100"])
let re = Re.(compile @@ seq [bow; group (digits); char '%'])

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_rar r =
  match Fpath.get_ext r with
  | ".rar" -> true
  | r when String.length r <> 4 -> false
  | r ->
    let idx = String.get r in
    Char.equal '.' @@ idx 0
    && Char.equal 'r' @@ idx 1
    && is_digit @@ idx 2
    && is_digit @@ idx 3

let of_list l =
  let all = List.filter (fun x -> is_rar x) l in
  let rar =  List.find_opt (fun x -> match Fpath.get_ext x with
      | ".rar" -> true
      | _ -> false) all
  in
  Option.map (fun rar -> {all;rar}) rar

let print_lines ~f fname lines =
  let rec loop percents =
    match%lwt Lwt_stream.get percents with
    | Some line ->
      let%lwt () = f fname (`Progress line) in
      loop percents
    | None -> Lwt.return_unit
  in
  Lwt_stream.filter_map
    (fun s ->
       let group = Re.exec_opt re s in
       Option.map (fun g -> Re.Group.get g 1 |> int_of_string) group)
    lines
  |> loop

let noop _ _ = Lwt.return_unit

let unrar ?(f=noop) {rar;_} =
  let fname = Fpath.filename rar in
  let%lwt () = f fname `New in
  let cwd = Fpath.parent rar |> Fpath.to_string in
  let r_fd,w_fd = Lwt_unix.pipe_in () in
  let command = ("", [|"unrar"; "-y"; "x"; Fpath.to_string rar|]) in
  let process = Lwt_process.exec ~cwd ~stdout:(`FD_move w_fd) command in
  let lines = Lwt_io.of_fd ~mode:Lwt_io.input r_fd |> Lwt_io.read_lines in
  let%lwt (),status = Lwt.both (print_lines ~f fname lines) process in
  let%lwt () = f fname `Done in
  (match status with
  | WEXITED 0 -> `Ok
  | WEXITED i -> `Exit_error i
  | _ -> `Other)
  |> Lwt.return

let remove {all;_} =
  List.map Fpath.to_string all
  |> Lwt_list.iter_s Lwt_unix.unlink
