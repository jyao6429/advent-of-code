open! Core

let resources_dir = "/home/jyao6429/oxcaml/advent-of-code/aoc-2025/resources/"
let file_path filename = resources_dir ^/ filename
let read_all filename = In_channel.read_all (file_path filename)
let read_lines filename = In_channel.read_lines (file_path filename)
let print_int n = Int.to_string n |> print_endline
let split_lines s = String.strip s |> String.split_lines

let run_in_parallel ~(f : Parallel.t @ local -> 'a) : 'a =
  let module Scheduler = Parallel_scheduler_work_stealing in
  let scheduler = Scheduler.create () in
  let result = Scheduler.parallel scheduler ~f in
  Scheduler.stop scheduler;
  result
;;

let run_sequentially ~(f : Parallel.t @ local -> 'a) : 'a =
  let module Scheduler = Parallel.Scheduler.Sequential in
  let scheduler = Scheduler.create () in
  let result = Scheduler.parallel scheduler ~f in
  Scheduler.stop scheduler;
  result
;;
