(* A mysterious bug currently prevents this from working correctly! *)

let test () =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^
  "<!DOCTYPE cesAna SYSTEM \"xcesAnaIPI.dtd\">\n" ^
  "<chunkList xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" ^
  " <chunk id=\"ch1\" type=\"p\">\n" ^
  "  <sentence>\n" ^
  "   <tok>\n" ^
  "    <orth>Ala</orth>\n" ^
  "    <lex><base>al</base><ctag>subst:sg:gen:m1</ctag></lex>\n" ^
  "    <lex><base>al</base><ctag>subst:sg:acc:m1</ctag></lex>\n" ^
  "    <lex><base>ala</base><ctag>subst:sg:nom:f</ctag></lex>\n" ^
  "   </tok>\n" ^
  "   <tok>\n" ^
  "    <orth>ma</orth>\n" ^
  "    <lex><base>mieć</base><ctag>fin:sg:ter:imperf</ctag></lex>\n" ^
  "    <lex><base>mój</base><ctag>adj:sg:nom:f:pos</ctag></lex>\n" ^
  "   </tok>\n" ^
  "   <tok>\n" ^
  "    <orth>kota</orth>\n" ^
  "    <lex><base>kot</base><ctag>subst:sg:gen:m2</ctag></lex>\n" ^
  "    <lex><base>kot</base><ctag>subst:sg:acc:m2</ctag></lex>\n" ^
  "    <lex><base>kota</base><ctag>subst:sg:nom:f</ctag></lex>\n" ^
  "   </tok>\n" ^
  "   <ns/>\n" ^
  "   <tok>\n" ^
  "    <orth>.</orth>\n" ^
  "    <lex><base>.</base><ctag>interp</ctag></lex>\n" ^
  "   </tok>\n" ^
  "  </sentence>\n" ^
  " </chunk>\n" ^
  "</chunkList>"

let start_wosedon () =
  Unix.open_process_full ("wosedon -c cfg/wosedon.ini -i")
    (Unix.environment ())

let stop_wosedon (wosedon_in, wosedon_out, wosedon_err) =
  ignore @@ Unix.close_process_full (wosedon_in, wosedon_out, wosedon_err)

let analyze ic oc ec s =
  let rec read_loop acc =
    let l = input_line ic in
    if l <> "</chunkList>" then read_loop @@ l::acc
    else String.concat "\n" @@ List.rev @@ l::acc in
  output_string oc @@ s ^ "\n\n";
  flush oc;
  read_loop []

let _ =
  let wosedon_in, wosedon_out, wosedon_err = start_wosedon () in
  print_endline @@ analyze wosedon_in wosedon_out wosedon_err @@ test ();
  stop_wosedon (wosedon_in, wosedon_out, wosedon_err)
