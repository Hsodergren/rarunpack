type 'a t

val create: 'a -> ('a -> Progress.t -> 'a) -> ('a -> unit) -> 'a t
val update: 'a t -> Progress.t -> 'a t
val print: 'a t -> unit 
