type t

val of_list: Fpath.t list -> t option

type ev = [`New | `Progress of int | `Done | `Visit]

val unrar: ?f:(string -> ev -> unit Lwt.t) -> t ->  unit Lwt.t

val remove: t -> unit Lwt.t
