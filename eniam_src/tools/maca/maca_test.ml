let start_maca () =
  Unix.open_process_full ("maca-analyse -ql morfeusz-nkjp-official")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|]

let stop_maca (maca_in, maca_out, maca_err) =
  ignore @@ Unix.close_process_full (maca_in, maca_out, maca_err)

let analyze ic oc ec s =
  let rec read_loop acc =
    let l = input_line ic in
    if String.length l > 0 then read_loop @@ l::acc
    else String.concat "\n" @@ List.rev acc in
  output_string oc @@ s ^ "\n";
  flush oc;
  read_loop []

let _ =
  let maca_in, maca_out, maca_err = start_maca () in
  print_endline @@ analyze maca_in maca_out maca_err "Ala ma kota.";
  print_endline @@ analyze maca_in maca_out maca_err "Ala ma psa.";
  stop_maca (maca_in, maca_out, maca_err)
