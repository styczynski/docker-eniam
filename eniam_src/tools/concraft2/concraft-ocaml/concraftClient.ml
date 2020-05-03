(*
 *  ENIAM: Categorial Syntactic-Semantic Parser for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf

let get_sock_addr host_name port =
  let he =
    try Unix.gethostbyname host_name
    with Not_found -> failwith ("get_sock_addr: host " ^ host_name ^ " not found") in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let get_header chan =
  let r = ref [] in
  let f = ref true in
  (try
    while !f do
      let s = input_line chan in
      if s = "\r" then f := false else
      r := s :: (!r)
    done
  with End_of_file -> failwith "get_header");
  List.rev !r

let get_trailer chan =
  let r = ref [] in
  (try
    while true do
      r := (input_line chan) :: (!r)
    done;
    !r
  with End_of_file -> List.rev !r)

let get_message chan =
  let s = input_line chan in
  let len = Scanf.sscanf s "%x\r" (fun x -> x) in
  printf "len=%d\n" len;
  really_input_string chan len


let render l =
  String.concat "\n" (List.rev (Xlist.rev_map l (fun (i,j,orth,lemma,interp,cat,cat2,prob,x1,x2) ->
    String.concat "\t" [i;j;orth;lemma;interp;cat;cat2;prob;x1;x2])))

let encode s =
  String.concat "" (List.rev (Xlist.rev_map (Xunicode.utf8_chars_of_utf8_string s) (function
      "\r" -> "\\r"
    | "\t" -> "\\t"
    | "\n" -> "\\n"
    | "\\" -> "\\\\"
    | "\"" -> "\\\""
    | c -> 
      if String.length c = 1 then c else
      (match Xunicode.unicode_chars_of_utf8_string c with
        [c] -> sprintf "\\u%04x" c
      | _ -> failwith "encode"))))

let send_message file host port msg =
  fprintf file "POST /parse HTTP/1.1\r\n";
  fprintf file "Host: %s:%d\r\n" host port;
  fprintf file "Content-Length: %d\r\n" (String.length msg);
  fprintf file "User-Agent: python-requests/1.9.1\r\n";
  fprintf file "Connection: keep-alive\r\n";
  fprintf file "Accept: */*\r\n";
  fprintf file "Accept-Encoding: gzip, deflate\r\n";
  fprintf file "\r\n";
  fprintf file "%s" msg;
  flush file

let analyse_header = function
    "HTTP/1.1 200 OK\r" :: header ->
      let header = Xlist.rev_map header (fun s ->
        if Xstring.check_sufix "\r" s then Xstring.cut_sufix "\r" s else failwith ("analyse_header 3: " ^ s)) in
      let header = Xlist.rev_map header (fun s ->
        match Xstring.split ": " s with
          [k;v] -> k,v
        | _ -> failwith ("analyse_header 2: " ^ s)) in
      Xlist.iter header (function 
          "Transfer-Encoding","chunked" -> ()
        | "Date",_ -> ()
        | "Server","Warp/3.2.11.2" -> ()
        | "Content-Type","application/json; charset=utf-8" -> ()
        | k,v -> failwith ("analyse_header 4: " ^ k ^ ": " ^ v))
  | header -> failwith ("analyse_header 1: " ^ String.concat "\n" header)

let analyse_trailer = function
    ["\r";"0\r";"\r"] -> ()
  | l -> failwith ("analyse_trailer: " ^ String.concat "\n" l)

let analyse_message s = 
  let s = if Xstring.check_prefix "{\"dag\":\"" s then Xstring.cut_prefix "{\"dag\":\"" s else failwith ("analyse_message 1: " ^ s) in
  let s = if Xstring.check_sufix "\\n\\n\"}" s then Xstring.cut_sufix "\\n\\n\"}" s else failwith ("analyse_message 2: " ^ s) in
  List.rev (Xlist.rev_map (Xstring.split "\\\\n" s) (fun line ->
   print_endline line;
   match Xstring.split "\\\\t" line with
      [i;j;orth;lemma;interp;cat;cat2;prob;x1;x2] -> ()
    | [i;j;orth;lemma;interp;cat;cat2;prob;x1;x2;disamb] -> ()
    | _ -> failwith ("analyse_message: " ^ line)))

let process_query host_name port query =
  let sock = get_sock_addr "localhost" 4322 in
  let ic,oc =
    try Unix.open_connection sock
    with e -> failwith ("server connection error: " ^ Printexc.to_string e) in
  let query = "{\"dag\": \"" ^ encode (render query) ^ "\\n\\n\"}" in
  send_message oc "localhost" 4322 query;
  Printf.fprintf oc "%s\n\n%!" query;
  analyse_header (get_header ic);
  let message = get_message ic in
  analyse_trailer (get_trailer ic);
  (*Printf.fprintf oc "\n%!";*)
  let _ = Unix.shutdown_connection ic in
  analyse_message message

let msg_struct = [
  "0",	 "1",	 "Ala",	 "Ala",	 "subst:sg:nom:f",	 "imię",	 "",	 "0.000",	 "",	 "";
  "0",	 "1",	 "Ala",	 "Al",	 "subst:sg:gen:m1",	 "imię",	 "",	 "0.000",	 "",	 "";
  "0",	 "1",	 "Ala",	 "Al",	 "subst:sg:acc:m1",	 "imię",	 "",	 "0.000",	 "",	 "";
  "0",	 "1",	 "Ala",	 "Alo",	 "subst:sg:gen:m1",	 "imię",	 "",	 "0.000",	 "",	 "";
  "0",	 "1",	 "Ala",	 "Alo",	 "subst:sg:acc:m1",	 "imię",	 "",	 "0.000",	 "",	 "";
  "1",	 "2",	 "ma",	 "mój:a",	 "adj:sg:nom:f:pos",	 "",	 "",	 "0.000",	 "",	 "";
  "1",	 "2",	 "ma",	 "mój:a",	 "adj:sg:voc:f:pos",	 "",	 "",	 "0.000",	 "",	 "";
  "1",	 "2",	 "ma",	 "mieć",	 "fin:sg:ter:imperf",	 "",	 "",	 "0.000",	 "",	 "";
  "2",	 "3",	 "kota",	 "kota",	 "subst:sg:nom:f",	 "nazwa pospolita",	 "",	 "0.000",	 "",	 "";
  "2",	 "3",	 "kota",	 "kot:s1",	 "subst:sg:gen:m2",	 "nazwa pospolita",	 "",	 "0.000",	 "",	 "";
  "2",	 "3",	 "kota",	 "kot:s1",	 "subst:sg:acc:m2",	 "nazwa pospolita",	 "",	 "0.000",	 "",	 "";
  "2",	 "3",	 "kota",	 "kot:s2",	 "subst:sg:gen:m1",	 "nazwa pospolita",	 "pot.,środ.",	 "0.000",	 "",	 "";
  "2",	 "3",	 "kota",	 "kot:s2",	 "subst:sg:acc:m1",	 "nazwa pospolita",	 "pot.,środ.",	 "0.000",	 "",	 "";
  "3",	 "4",	 ".",	 ".",	 "interp",	 "",	 "",	 "0.000",	 "",	 "";
  "4",	 "5",	 "Miał",	 "mieć",	 "praet:sg:m1:imperf",	 "",	 "",	 "0.000",	 "",	 "";
  "4",	 "5",	 "Miał",	 "mieć",	 "praet:sg:m2:imperf",	 "",	 "",	 "0.000",	 "",	 "";
  "4",	 "5",	 "Miał",	 "mieć",	 "praet:sg:m3:imperf",	 "",	 "",	 "0.000",	 "",	 "";
  "4",	 "6",	 "Miałem",	 "miał",	 "subst:sg:inst:m3",	 "nazwa pospolita",	 "",	 "0.000",	 "",	 "";
  "5",	 "6",	 "em",	 "być",	 "aglt:sg:pri:imperf:wok",	 "",	 "",	 "0.000",	 "",	 "";
  "6",	 "7",	 "miał",	 "mieć",	 "praet:sg:m1:imperf",	 "",	 "",	 "0.000",	 "",	 "";
  "6",	 "7",	 "miał",	 "mieć",	 "praet:sg:m2:imperf",	 "",	 "",	 "0.000",	 "",	 "";
  "6",	 "7",	 "miał",	 "mieć",	 "praet:sg:m3:imperf",	 "",	 "",	 "0.000",	 "",	 "";
  "6",	 "7",	 "miał",	 "miał",	 "subst:sg:nom:m3",	 "nazwa pospolita",	 "",	 "0.000",	 "",	 "";
  "6",	 "7",	 "miał",	 "miał",	 "subst:sg:acc:m3",	 "nazwa pospolita",	 "",	 "0.000",	 "",	 "";
  "7",	 "8",	 ".",	 ".",	 "interp",	 "",	 "",	 "0.000",	 "",	 "";
  ]

let msg = "0\t1\tAla\tAla\tsubst:sg:nom:f\timię\t\t0.000\t\t\n0\t1\tAla\tAl\tsubst:sg:gen:m1\timię\t\t0.000\t\t\n0\t1\tAla\tAl\tsubst:sg:acc:m1\timię\t\t0.000\t\t\n0\t1\tAla\tAlo\tsubst:sg:gen:m1\timię\t\t0.000\t\t\n0\t1\tAla\tAlo\tsubst:sg:acc:m1\timię\t\t0.000\t\t\n1\t2\tma\tmój:a\tadj:sg:nom:f:pos\t\t\t0.000\t\t\n1\t2\tma\tmój:a\tadj:sg:voc:f:pos\t\t\t0.000\t\t\n1\t2\tma\tmieć\tfin:sg:ter:imperf\t\t\t0.000\t\t\n2\t3\tkota\tkota\tsubst:sg:nom:f\tnazwa pospolita\t\t0.000\t\t\n2\t3\tkota\tkot:s1\tsubst:sg:gen:m2\tnazwa pospolita\t\t0.000\t\t\n2\t3\tkota\tkot:s1\tsubst:sg:acc:m2\tnazwa pospolita\t\t0.000\t\t\n2\t3\tkota\tkot:s2\tsubst:sg:gen:m1\tnazwa pospolita\tpot.,środ.\t0.000\t\t\n2\t3\tkota\tkot:s2\tsubst:sg:acc:m1\tnazwa pospolita\tpot.,środ.\t0.000\t\t\n3\t4\t.\t.\tinterp\t\t\t0.000\t\t\n4\t5\tMiał\tmieć\tpraet:sg:m1:imperf\t\t\t0.000\t\t\n4\t5\tMiał\tmieć\tpraet:sg:m2:imperf\t\t\t0.000\t\t\n4\t5\tMiał\tmieć\tpraet:sg:m3:imperf\t\t\t0.000\t\t\n4\t6\tMiałem\tmiał\tsubst:sg:inst:m3\tnazwa pospolita\t\t0.000\t\t\n5\t6\tem\tbyć\taglt:sg:pri:imperf:wok\t\t\t0.000\t\t\n6\t7\tmiał\tmieć\tpraet:sg:m1:imperf\t\t\t0.000\t\t\n6\t7\tmiał\tmieć\tpraet:sg:m2:imperf\t\t\t0.000\t\t\n6\t7\tmiał\tmieć\tpraet:sg:m3:imperf\t\t\t0.000\t\t\n6\t7\tmiał\tmiał\tsubst:sg:nom:m3\tnazwa pospolita\t\t0.000\t\t\n6\t7\tmiał\tmiał\tsubst:sg:acc:m3\tnazwa pospolita\t\t0.000\t\t\n7\t8\t.\t.\tinterp\t\t\t0.000\t\t\n\n"

let msg2 = "0\\t1\\tAla\\tAla\\tsubst:sg:nom:f\\timi\\u0119\\t\\t0.000\\t\\t\\n0\\t1\\tAla\\tAl\\tsubst:sg:gen:m1\\timi\\u0119\\t\\t0.000\\t\\t\\n0\\t1\\tAla\\tAl\\tsubst:sg:acc:m1\\timi\\u0119\\t\\t0.000\\t\\t\\n0\\t1\\tAla\\tAlo\\tsubst:sg:gen:m1\\timi\\u0119\\t\\t0.000\\t\\t\\n0\\t1\\tAla\\tAlo\\tsubst:sg:acc:m1\\timi\\u0119\\t\\t0.000\\t\\t\\n1\\t2\\tma\\tm\\u00f3j:a\\tadj:sg:nom:f:pos\\t\\t\\t0.000\\t\\t\\n1\\t2\\tma\\tm\\u00f3j:a\\tadj:sg:voc:f:pos\\t\\t\\t0.000\\t\\t\\n1\\t2\\tma\\tmie\\u0107\\tfin:sg:ter:imperf\\t\\t\\t0.000\\t\\t\\n2\\t3\\tkota\\tkota\\tsubst:sg:nom:f\\tnazwa pospolita\\t\\t0.000\\t\\t\\n2\\t3\\tkota\\tkot:s1\\tsubst:sg:gen:m2\\tnazwa pospolita\\t\\t0.000\\t\\t\\n2\\t3\\tkota\\tkot:s1\\tsubst:sg:acc:m2\\tnazwa pospolita\\t\\t0.000\\t\\t\\n2\\t3\\tkota\\tkot:s2\\tsubst:sg:gen:m1\\tnazwa pospolita\\tpot.,\\u015brod.\\t0.000\\t\\t\\n2\\t3\\tkota\\tkot:s2\\tsubst:sg:acc:m1\\tnazwa pospolita\\tpot.,\\u015brod.\\t0.000\\t\\t\\n3\\t4\\t.\\t.\\tinterp\\t\\t\\t0.000\\t\\t\\n4\\t5\\tMia\\u0142\\tmie\\u0107\\tpraet:sg:m1:imperf\\t\\t\\t0.000\\t\\t\\n4\\t5\\tMia\\u0142\\tmie\\u0107\\tpraet:sg:m2:imperf\\t\\t\\t0.000\\t\\t\\n4\\t5\\tMia\\u0142\\tmie\\u0107\\tpraet:sg:m3:imperf\\t\\t\\t0.000\\t\\t\\n4\\t6\\tMia\\u0142em\\tmia\\u0142\\tsubst:sg:inst:m3\\tnazwa pospolita\\t\\t0.000\\t\\t\\n5\\t6\\tem\\tby\\u0107\\taglt:sg:pri:imperf:wok\\t\\t\\t0.000\\t\\t\\n6\\t7\\tmia\\u0142\\tmie\\u0107\\tpraet:sg:m1:imperf\\t\\t\\t0.000\\t\\t\\n6\\t7\\tmia\\u0142\\tmie\\u0107\\tpraet:sg:m2:imperf\\t\\t\\t0.000\\t\\t\\n6\\t7\\tmia\\u0142\\tmie\\u0107\\tpraet:sg:m3:imperf\\t\\t\\t0.000\\t\\t\\n6\\t7\\tmia\\u0142\\tmia\\u0142\\tsubst:sg:nom:m3\\tnazwa pospolita\\t\\t0.000\\t\\t\\n6\\t7\\tmia\\u0142\\tmia\\u0142\\tsubst:sg:acc:m3\\tnazwa pospolita\\t\\t0.000\\t\\t\\n7\\t8\\t.\\t.\\tinterp\\t\\t\\t0.000\\t\\t\\n\\n"

let _ = process_query "localhost" 4322 msg_struct

(*let _ = 
  print_endline msg;
  print_endline "";
  let x = encode msg in
  print_endline (encode msg);
  print_endline "";
  if x = msg2 then print_endline "OK" else print_endline "ERROR"*)

(* uruchamianie serwera
./concraft-dag2-pl server --port=4322 -i ./concraft-pl-model-180317.gz +RTS -N4
*)
