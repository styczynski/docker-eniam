open Xstd
open Printf
open WalTypes

let print_semroles_usage_tab filename walenty =
  let set = StringMap.fold walenty StringSet.empty (fun set pos entries ->
    StringMap.fold entries set (fun set lemma frames ->
      Xlist.fold frames set (fun set -> function
          Frame(DefaultAtrs(m,r,o,neg,p,a),schema) ->
            let lemma =
              (match neg with Negation -> "nie " | Aff -> "aff " | _ -> "") ^
              lemma ^
              (match r with ReflSie -> " się" | _ -> "") in
            Xlist.fold schema set (fun set s ->
              if s.role = "" then set else
              let role = s.role ^ (if s.role_attr = "" then "" else " " ^ s.role_attr) in
              let gf = WalStringOf.gf s.gf in
              let sel_prefs = String.concat " " s.sel_prefs in
              Xlist.fold s.morfs set (fun set morf ->
                let morf =  WalStringOf.morf morf in
                StringSet.add set (String.concat "\t" [pos;lemma;gf;role;sel_prefs;morf])))
        | _ -> failwith "print_semroles_usage_tab"))) in
  File.file_out filename (fun file ->
    StringSet.iter set (fprintf file "%s\n"))

(* let _ =
  let walenty = WalTEI.load_walenty2 () in
  let frames_sem = try StringMap.find (StringMap.find walenty "verb") "abdykować" with Not_found -> failwith "walTEI" in
  Xlist.iter frames_sem (fun frame ->
    print_endline (WalStringOf.frame "abdykować" frame)) *)

let _ =
  let walenty = WalTEI.load_walenty2 () in
  print_semroles_usage_tab "sem_roles.tab" walenty
