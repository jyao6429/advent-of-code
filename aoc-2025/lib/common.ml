open! Core

let resources_dir = "/home/jyao6429/oxcaml/advent-of-code/aoc-2025/resources/"
let file_path filename = resources_dir ^/ filename
let read_all filename = In_channel.read_all (file_path filename)
let read_lines filename = In_channel.read_lines (file_path filename)
let print_int n = Printf.printf "%d\n" n
let split_lines s = String.strip s |> String.split_lines
