open! Core

val resources_dir : string
val file_path : string -> string
val read_all : string -> string
val read_lines : string -> string list
val print_int : int -> unit
val split_lines : string -> string list
val run_in_parallel : f:(Parallel.t @ local -> 'a) @ portable -> 'a
val run_sequentially : f:(Parallel.t @ local -> 'a) @ portable -> 'a
