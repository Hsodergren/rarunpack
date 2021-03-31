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
    | None ->
      f fname `Done
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
  let lines = Lwt_process.pread_lines ~cwd ("", [|"unrar"; "-y"; "x"; Fpath.to_string rar|]) in
  print_lines ~f fname lines

let remove {all;_} =
  List.map Fpath.to_string all
  |> Lwt_list.iter_s Lwt_unix.unlink
