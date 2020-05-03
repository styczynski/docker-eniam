let path = "data/NKJP-PodkorpusMilionowy-1.2/"
let version = "1"

(*allows printing messege to a file in results/*)
let rec write_to message file=
  try
    if Sys.is_directory "results/"
    then
      let oc = open_out_gen [Open_append; Open_creat] 511 (path^ "/" ^file) in
       Printf.fprintf oc "%s\n" message;
       close_out oc
  with
  | Sys_error _ ->
    assert(Sys.command ("mkdir "^path) >= 0);
    write_to message file

(*returns how much time took proc from beg*)
let time_from_of beg proc =
  proc ^ " done in " ^ (string_of_float (Sys.time () -. beg)) ^ " seconds\n"

(*clears directory of current version*)
let clear_version () =
  try
    if Sys.is_directory ("results" ^ version ^ "/")
    then Sys.command ("rm -r results" ^ version)
    else 0
  with
  | Sys_error _ -> Sys.command ("mkdir results" ^ version ^ "/")


(*TESTING IN PROGRESS*)
let _ = clear_version ()

(*Usage of fold_text*)
let beg = Sys.time ()
let x = NKJP.fold_text path  ~source:[] ~channel:[] 0 (fun x y -> write_to (NKJP.string_of_text y) "fold_text.txt"; x+1)
let () =
  write_to (string_of_int x) "fold_text.txt";
  print_endline (time_from_of beg "fold_text")

(*Usage of fold_segm*)
let beg = Sys.time ()
let x = NKJP.fold_segm path ~source:[] ~channel:[] 0 (fun x y -> write_to (NKJP.string_of_text y) "fold_segm.txt"; x+1)
let () =
  write_to (string_of_int x) "fold_segm.txt";
  print_endline (time_from_of beg "fold_segm")

(*Usage of fold_morph*)
let beg = Sys.time ()
let x = NKJP.fold_morph path ~source:[] ~channel:[] 0 (fun x y -> write_to (NKJP.string_of_text y) "fold_morph.txt"; x+1)
let () =
  write_to (string_of_int x) "fold_morph.txt";
  print_endline (time_from_of beg "fold_morph")

(*Usage of fold_sense*)
let beg = Sys.time ()
let x = NKJP.fold_sense path ~source:[] ~channel:[] 0 (fun x y -> write_to (NKJP.string_of_text y) "fold_sense.txt"; x+1)
let () =
  write_to (string_of_int x) "fold_sense.txt";
  print_endline (time_from_of beg "fold_sense")
