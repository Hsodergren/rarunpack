type t

val of_list: Fpath.t list -> t option

type ev = [`New | `Progress of int | `Done]

val unrar: ?f:(string -> [> ev] -> unit Lwt.t) -> t ->  [`Ok | `Exit_error of int | `Other] Lwt.t

val remove: t -> unit Lwt.t
