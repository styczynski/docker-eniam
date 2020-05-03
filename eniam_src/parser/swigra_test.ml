
let swigra_in, swigra_out = Unix.open_process "../swigra/parser/run.sh"

let _ =
  Printf.fprintf swigra_out "Ala ma kota\n%!";
  print_endline (input_line swigra_in);
  (* powyższą linię należy zastąpić przez:
  let xml = Xml.parse_in swigra_in in
  print_endline (Xml.to_string_fmt xml; *)
  Printf.fprintf swigra_out "Ela ma kota\n%!";
  print_endline (input_line swigra_in);
  (* powyższą linię należy zastąpić przez:
  let xml = Xml.parse_in swigra_in in
  print_endline (Xml.to_string_fmt xml; *)
  ()
