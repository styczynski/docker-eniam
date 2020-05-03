(*
 *  ENIAMcorpora is a library that integrates ENIAM with corpora in CONLL format
 *  Copyright (C) 2016 Daniel Oklesinski <oklesinski dot daniel atSPAMfree gmail dot com>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
 *
 *  This library is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Xstd
open ENIAMsubsyntaxTypes
open ENIAMtokenizerTypes

let if_lemma lemmas = function
    Lemma(l,_,_) -> List.exists (fun x -> x = l) lemmas
  | _ -> false

let if_cat cats = function
    Lemma(_,cat,_) -> List.exists (fun x -> x = cat) cats
  | _ -> false

let if_interps interps token =
  let interp = match token with
      Lemma(_,_,i) -> i
    | _ -> [[[]]] in
  let if_interp nr value =
    List.exists (fun x ->
      try
        List.exists (fun y ->
          y = value) (List.nth x nr)
      with _ -> false
      ) interp in
  Xlist.fold interps true (fun acc (nr,value) -> acc && (if_interp nr value))

let change_dep paths i (id,super,label) =
  let id_S, super_S, label_S = paths.(super) in
    paths.(i) <- (id,super_S,label);
    paths.(super) <- (id_S, id, label_S)

let correct_injection paths tokens = Array.iteri (fun i (id,super,label) ->
  if label = "punct" then (*musi być pierwszym tokenem o tym ojcu*)
    let j = Int.fold (i+1) (Array.length paths - 1) 0 (fun acc n ->
        let i2,s2,l2 = paths.(n) in
        if super = s2
        then if l2 = "punct"
          then n
          else 0
        else acc
      ) in
    let k = Int.fold_down (i-1) 1 i (fun acc n ->
        let i2,s2,l2 = paths.(n) in
        if super = s2
          then 0
          else acc
      ) in
    if k == i && j <> 0 && i < super && super < j
      then
        (paths.(i) <- (0,-1,"");
        paths.(j) <- (0,-1,""))
    ) paths;
  paths

let correct_coordination1 paths tokens =
  let paths_ls () = List.mapi (fun i (id,super,label) ->
    (i,id,super,label)) (Array.to_list paths) in

  let l = [("subst:nom",0),(["fin";"praet"],0);
           ("subst:acc",0),(["inf"],0);
           ("ppron3:nom",0),(["fin";"praet"],0);
           ("ppron3:acc",0),(["fin";"praet"],0);
           ("adv",0),(["fin";"praet"],0);
           ("adv",0),(["inf"],0);
           ("adv",0),(["adj"],0);
           ("prep",0),(["fin";"praet"],0);
           ("prep",0),(["inf"],0);
           ("prep",0),(["ppas"],0);
           ("prep",0),(["subst"],0);
           ("prep:gen",0),(["subst:gen"],0);
           ("adj:nom",0),(["fin";"praet"],0);
           ("adj:nom",0),(["subst:nom"],0);
           ("adj:gen",0),(["subst:gen"],0);
           ("adj:dat",0),(["subst:dat"],0);
           ("adj:acc",0),(["subst:acc"],0);
           ("adj:inst",0),(["subst:inst"],0);
           ("adj:loc",0),(["subst:loc"],0);
           ("subst:gen",0),(["subst:nom"],0);
           (* ("subst:gen",0),(["subst:gen"],0); *)
           ("subst:gen",0),(["subst:dat"],0);
           ("subst:gen",0),(["subst:acc"],0);
           ("subst:gen",0),(["subst:inst"],0);
           ("subst:gen",0),(["subst:loc"],0);
           ("ppron3:gen",0),(["subst:nom"],0);
           ("ppron3:gen",0),(["subst:dat"],0);
           ("ppron3:gen",0),(["subst:acc"],0);
           ("ppron3:gen",0),(["subst:inst"],0);
           ("ppron3:gen",0),(["subst:loc"],0);
           ("qub",0),(["fin";"praet"],0);
           ("qub",0),(["subst"],0);
           ("qub",0),(["adj"],0);
           ("pact",0),(["subst"],0);
           ("ppas",0),(["subst"],0)
           ] in

  let find_dependents sons =

    let is (i,id,super,label) pattern = match Xstring.split ":" pattern with
        ["prep";case] -> if_cat ["prep"] (ExtArray.get tokens id).token &&
                  if_interps [0,case] (ExtArray.get tokens id).token
      | [cat;case] -> if_cat [cat] (ExtArray.get tokens id).token &&
                if_interps [1,case] (ExtArray.get tokens id).token
      | [cat] -> if_cat [cat] (ExtArray.get tokens id).token
      | _ -> failwith "is (in correct_coordination1)" in

    let incr_representative acc son = Xlist.map acc (fun ((one,a),(rest,b)) ->
      if is son one
      then (one,a + 1), (rest,b)
      else if List.exists (is son) rest
        then (one,a), (rest,b + 1)
        else (one,a), (rest,b)) in

    let get_from sons pattern = List.find (fun x -> is x pattern) sons in

    let l = Xlist.fold sons l incr_representative in
    let results = List.filter (fun ((_,a),(_,b)) -> a = 1 && b > 1) l in
      Xlist.map results (fun result ->
      get_from sons @@ fst @@ fst result,
      List.filter (fun son ->
        List.exists (fun one -> is son one) (fst (snd result))) sons) in

  let establish_neighbour super ((i_d,id_d,super_d,label_d),sons) =
    let not_between (i_s,_,_,_) =
      (super < i_d && super < i_s) ||
      (super > i_d && super > i_s) in
    let (i_n,id_n,super_n,label_n) = List.find (fun son ->
      not_between son) sons in
      paths.(i_d) <- (id_d, i_n, label_d) in

  let examine_coords (i,id,super,label) sons =
  try
    let rest, conjuncts = List.partition (fun (_,_,_,label) -> label <> "conjunct") sons in
    if List.length conjuncts > 0
    then List.iter (fun r -> establish_neighbour super (r,conjuncts)) rest;
    let sons = List.filter (fun (_,_,s,l) ->
                s = i && l <> "punct" && l <> "coord_punct" && l <> "pre_coord") (paths_ls ()) in
    let dependents = find_dependents sons in
    Xlist.iter dependents (establish_neighbour super)
  with
  | _ -> () in

  Array.iteri (fun i (id,super,label) ->
    if if_cat ["conj"] (ExtArray.get tokens i).token ||
      (ExtArray.get tokens i).orth = ","
    then (let sons = List.filter (fun (_,_,super,label) ->
                      super = i && label <> "punct" && label <> "coord_punct" && label <> "pre_coord") (paths_ls ()) in
      if (List.length sons > 2)
      then examine_coords (i,id,super,label) sons)) paths;
  paths

let correct_coordination2 omit_puncts paths tokens =
  let paths_c = Array.copy paths in
  let paths_ls () = List.mapi (fun i (id,super,label) ->
    (i,id,super,label)) (Array.to_list paths_c) in

  (* let ps a sons =
    print_endline a;
    List.iter (fun (i,_,_,_) -> print_endline (ExtArray.get tokens i).orth) sons;
    print_endline "" in *)

  let rec correct_rec (i,id,super,label) sons =
    let left_s, right_s = List.partition (fun (a,b,c,d) -> a < i) sons in
    (* ps "left:" (List.rev left_s);
    ps "right:" right_s; *)
    find_father i (List.rev left_s);
    find_father i right_s

  and find_father i0 = function
      [(i,id,super,label)] -> paths_c.(i) <- (id,i0,label)
    | (a,b,c,d) :: (i,id,super,label) :: t ->
        paths_c.(i) <- (id,i0,label);
        if not (if_cat ["conj"] (ExtArray.get tokens i).token ||
          (ExtArray.get tokens i).orth = ",")
        then failwith "find_father1";
        correct_rec (i,id,super,label) (if a < i
          then (a,b,c,d) :: t
          else List.rev @@ (a,b,c,d) :: t)
    | [] -> failwith "find_father2" in

  let check_previous_for_interp i =
    if i >= 0 && (ExtArray.get tokens i).orth = "," &&
      not (List.exists (fun (_,super,_) -> super = i) (Array.to_list paths_c))
    then paths_c.(i) <- (0,-1,"") in

  let remove_interps_on_edges sons =
    let i,id,super,label = List.hd sons in
    let sons = if if_cat ["interp"] (ExtArray.get tokens i).token then List.tl sons else sons in
    let i,id,super,label = List.hd (List.rev sons) in
    if if_cat ["interp"] (ExtArray.get tokens i).token then List.rev (List.tl (List.rev sons)) else sons in

  (* let filter_comp_construction sons =
    let rec pom acc = function
        (i1,id1,super1,label1) :: (i2,id2,super2,label2) :: t ->
          if if_cat ["interp"] (ExtArray.get tokens i1).token &&
             if_cat ["comp"] (ExtArray.get tokens i2).token
          then pom acc t
          else pom ((i1,id1,super1,label1) :: acc) ((i2,id2,super2,label2) :: t)
      | h :: t -> pom (h :: acc) t
      | [] -> List.rev acc in
    pom [] sons in *)

  Array.iteri (fun i (id,super,label) ->
    if if_cat ["conj"] (ExtArray.get tokens i).token ||
      (ExtArray.get tokens i).orth = ","
    then
      (check_previous_for_interp (i-1);
      let sons = if omit_puncts
        then List.filter (fun (_,_,super,label) -> super = i && label <> "punct") (paths_ls ())
        else List.filter (fun (_,_,super,label) -> super = i) (paths_ls ()) in
      (* let sons = filter_comp_construction sons in *)
      if List.length sons > 2
      then
        (let sons = remove_interps_on_edges sons in
        correct_rec (i,id,super,label) sons))) paths_c;
  paths_c

let praet_qub_aglt paths tokens =
  Array.iteri (fun i (id,super,label) ->
    if super >= 0 then
      (let id_s, super_s, label_s = paths.(super) in
      if if_cat ["aglt"] (ExtArray.get tokens id).token &&
         (ExtArray.get tokens id_s).orth = "by"
      then let id_gf,super_gf,label_gf = paths.(super_s) in
        if if_cat ["praet"] (ExtArray.get tokens id_gf).token
        then paths.(i) <- (id,super_s,label))) paths;
  paths

let replace_tokens paths tokens =
(* for i = 0 to ExtArray.size tokens - 1 do
 print_endline (string_of_int i ^ ": "^ (ExtArray.get tokens i).orth)
done; *)
  let find_token orth = Int.fold 0 (ExtArray.size tokens - 1) 0 (fun acc i ->
    if (ExtArray.get tokens i).orth = orth then i else acc) in

  let multidot i id0 super0 label0 =
    let id1, super1, label1 = paths.(super0) in
    if super1 >= 0 then
      let id2, super2, label2 = paths.(super1) in
      if (ExtArray.get tokens id1).orth = "." &&
        (ExtArray.get tokens id2).orth = "."
      then
        (paths.(super1) <- (find_token "..." ,super2, label2);
        paths.(super0) <- (0,-1,"");
        paths.(i) <- (0,-1,"")) in

  let brev i id super label =
    let if_the_last_dot () =
      try
        let (id_dot, s_dot, l_dot) = List.find (fun (i2,s,l) ->
          s = i && ((ExtArray.get tokens i2).orth = "." || (ExtArray.get tokens i2).orth = "...")) (Array.to_list paths) in
        Array.fold_left (fun acc (i2,s,l) ->
          acc && (ExtArray.get tokens i2).beg <= (ExtArray.get tokens id_dot).beg) true paths
      with Not_found -> true in

    let dot = if if_interps [0,"npun"] (ExtArray.get tokens id).token || if_the_last_dot ()
      then ""
      else "." in
    let n_orth = (ExtArray.get tokens id).orth ^ dot in
    paths.(i) <- (find_token n_orth,super,label) in

  Array.iteri (fun i (id,super,label) ->
    if (ExtArray.get tokens id).orth = "."
    then multidot i id super label;
    if if_cat ["brev"] (ExtArray.get tokens id).token
    then brev i id super label)
  paths;
  paths

let replace_hyphens paths tokens =
  let ref_paths = ref paths in
  let find_token token = Int.fold 0 (ExtArray.size tokens - 1) 0 (fun acc i ->
    if (ExtArray.get tokens i).token = token then i else acc) in
  let find_specific_token token beg next = Int.fold 0 (ExtArray.size tokens - 1) 0 (fun acc i ->
    if (ExtArray.get tokens i).token = token &&
       beg <= (ExtArray.get tokens i).beg &&
       (ExtArray.get tokens i).next <= next
    then i else acc) in

  let correct_last sons_of_zero = (* TODO: synowie zamiast syna *)
    let i1,s1,l1 = !ref_paths.(Array.length !ref_paths - 1) in
    if (ExtArray.get tokens i1).orth = "."
    then
      !ref_paths.(Array.length !ref_paths - 1) <- (find_token (Interp "</sentence>"),1,l1)
    else
      (ref_paths := Array.append !ref_paths [| (find_token (Interp "</sentence>"),1,"-") |];
      !ref_paths.(Array.length !ref_paths - 2) <- (i1,Array.length !ref_paths - 1,l1));
    Xlist.iter sons_of_zero (fun son_of_zero ->
      let i2,s2,l2 = !ref_paths.(son_of_zero) in
      !ref_paths.(son_of_zero) <- (i2,Array.length !ref_paths - 1,l2)) in

  let one_hyphen sons_of_zero =
    let i2,s2,l2 = !ref_paths.(1) in
    Xlist.iter sons_of_zero (fun son_of_zero ->
      let i1,s1,l1 = !ref_paths.(son_of_zero) in
      !ref_paths.(son_of_zero) <- (i1,1,l1));
    !ref_paths.(1) <- (find_token (Interp "<or-sentence>"),0,l2);
    correct_last sons_of_zero in

  let two_hyphens first second son parent =
    let i1,s1,l1 = !ref_paths.(first) in
    let i2,s2,l2 = !ref_paths.(second) in
    let beg, next = (ExtArray.get tokens i2).beg, (ExtArray.get tokens i2).next in
    let i3,s3,l3 = !ref_paths.(son) in
    let i4,s4,l4 = !ref_paths.(parent) in
    ref_paths := Array.append !ref_paths [| (find_token (Interp "</sentence>"),first,"-") |];
    !ref_paths.(first) <- (find_token (Interp "<or-sentence>"),0,l1);
    !ref_paths.(second) <- (find_specific_token (Interp "</or-sentence>") beg next,first,l2);
    !ref_paths.(son) <- (i3,second,l3);
    !ref_paths.(parent) <- (i4,first,l4) in

  let rec is_dep_correct a b out zero res i (id,super,label) = (* out = how many words in (a,b) have parent outside [a,b]*)
    (* print_endline ((string_of_int a) ^ " " ^ (string_of_int b) ^ " " ^ (string_of_int out) ^ " " ^ (string_of_int zero) ^ " " ^ (string_of_int i)); *)
    if out > 1 || zero > 1 ||                           (* zero = how many words (not interps) have parent 0 *)
       (a < i && i < b && super < a && label <> "interp") ||
       (a < super && super < b && (i < a || b < i))
    then false, res
    else
      if i+1 = Array.length !ref_paths
      then out = 1 && zero = 1, res
      else
        if a < i && i < b && b < super
        then is_dep_correct a b (out+1) zero (i,super) (i+1) !ref_paths.(i+1)
        else
          if super = 0 && not (if_cat ["interp"] (ExtArray.get tokens id).token)
          then is_dep_correct a b out (zero+1) res (i+1) !ref_paths.(i+1)
          else is_dep_correct a b out zero res (i+1) !ref_paths.(i+1) in

  let hyphens = snd @@ Array.fold_left (fun (i,acc) (id,super,label) ->
    if (ExtArray.get tokens id).orth = "-"
    then i+1, i :: acc
    else i+1, acc) (0,[]) !ref_paths in

  let sons_of_zero = snd @@ Array.fold_left (fun (i,acc) (id,super,label) ->
    if super = 0 && not (if_cat ["interp"] (ExtArray.get tokens id).token)
    then i+1, i :: acc
    else i+1, acc) (0,[]) !ref_paths in

  (if List.length sons_of_zero = 1
  then
    if List.length hyphens = 1 && hyphens = [1]
    then one_hyphen sons_of_zero
    else
      if List.length hyphens = 2
      then let a, b = List.nth hyphens 1, List.nth hyphens 0 in
           let is_good, (son,parent) = is_dep_correct a b 0 0 (0,0) 1 !ref_paths.(1) in
        if a = 1 && is_good
        then two_hyphens a b son parent);
  !ref_paths

let correct_interp_with_father_0 paths tokens =
  Array.iteri (fun i (id,super,label) ->
    if (super = 0 ||
        (ExtArray.get tokens id).token = Interp "<or-sentence>" ||
        (ExtArray.get tokens id).token = Interp "</or-sentence>") && (ExtArray.get tokens id).orth = ","
    then Array.iteri (fun i1 (id1,super1,label1) ->
      if super1 = i
      then paths.(i1) <- (id1,0,label1)) paths) paths;
  paths

let corect_complm paths tokens =
  Array.iteri (fun i (id,super,label) ->
    if label = "complm" && super > 0
    then
      let i2,s2,l2 = paths.(super) in
      if if_cat ["conj"] (ExtArray.get tokens i2).token
      then change_dep paths i (id,super,label)
    ) paths;
  paths

let remove_interps interp paths tokens =
  let paths_ls = Array.to_list paths in
    Array.iteri (fun i (id,super,label) ->
      if (ExtArray.get tokens id).orth = interp &&
        not (List.exists (fun (_,super,_) -> super = i) paths_ls)
      then paths.(i) <- (0,-1,"")) paths;
  paths

let correct_passive_voice paths tokens =
  Array.iteri (fun i (id,super,label) ->
    if super >= 0 then
      (let id_s, super_s, label_s = paths.(super) in
      if (if_cat ["praet"] (ExtArray.get tokens id).token &&
        if_cat ["ppas"] (ExtArray.get tokens id_s).token)
      then (paths.(i) <- (id,super_s,label);
        paths.(super) <- (id_s,i,label_s);
        Array.iteri (fun i_p (id_p,super_p,label_p) ->
          if super_p = super
          then paths.(i_p) <- (id_p,i,label_p)) paths))) paths;
  paths

let swap_dep paths tokens =
  let rec correct_dep i (id,super,label) =
    let adv_relators = ["kto";"co";"ile";"czyj";"jaki";"który";
      "jak";"skąd";"dokąd";"gdzie";"którędy";"kiedy";"odkąd";"dlaczego";"czemu";"gdy"] in
    if (if_cat ["comp"] (ExtArray.get tokens id).token &&
        if_cat ["fin"; "praet"; "winien"; "pred"; "imps"; "ppas"] (ExtArray.get tokens super).token) ||
       (if_cat ["conj"] (ExtArray.get tokens id).token &&
        if_cat ["fin"; "praet"; "winien"; "pred"; "imps"; "ppas"] (ExtArray.get tokens super).token &&
        not (List.exists (fun (_,super,_) -> super = i) (Array.to_list paths))) ||
       (if_cat ["ppron3"] (ExtArray.get tokens id).token &&
        if_interps [5,"praep"] (ExtArray.get tokens id).token) ||
       (if_lemma adv_relators (ExtArray.get tokens id).token &&
        if_cat ["fin"; "praet"; "winien"; "pred"; "imps"; "ppas"; "subst"] (ExtArray.get tokens super).token)
    then
        change_dep paths i (id,super,label);
    if (if_lemma adv_relators (ExtArray.get tokens id).token &&
        if_cat ["subst"; "pred"] (ExtArray.get tokens super).token)
    then correct_dep i paths.(i) in
  Array.iteri correct_dep paths; paths

  (*
  correct_coordination1 -> sąsiad słowem najbliższym po prawej, jeśli pomiędzy nim a mną spójnik, to najbliższym po lewej
  nieobsługiwana na razie koordynacja strony biernej - zarówno czasowniki posiłkowe, jak i imiesłowy
  nieobsługiwana na razie koordynacja podrzędników spójników podrzędnych *)

let translate_to_old_format paths =
  Array.map (function
      (id,[super,label],"") -> (id,super,label)
    | _ -> failwith "translate_to_old_format") paths

let translate_to_new_format paths =
  Array.map (fun (id,super,label)  -> id,[super,label],"") paths

let convert_dep_tree path first_try paths tokens =
    (* File.file_out (path ^ "/pre_text_unmodified_" ^ (string_of_bool first_try) ^ ".html") (fun file ->
      Printf.fprintf file "%s\n" ENIAMvisualization.html_header;
      Printf.fprintf file "%s\n" (ENIAMvisualization.html_of_dep_sentence tokens paths);
      Printf.fprintf file "%s\n" ENIAMvisualization.html_trailer); *)
    let if_error str_f f arg1 arg2 = try
      f arg1 arg2
    with
    | e -> (print_endline (str_f ^ " " ^ (Printexc.to_string e)); raise e) in
    let paths = Array.copy paths in
    let paths = translate_to_old_format paths in
    let paths =
      if first_try
      then
        let pom = if_error "replace_tokens" replace_tokens paths tokens in
        let pom = if_error "remove_interps." (remove_interps ".") pom tokens in
        let pom = if_error "replace_hyphens" replace_hyphens pom tokens in
        let pom = if_error "correct_injection" correct_injection pom tokens in
        let pom = if_error "correct_coordination1" correct_coordination1 pom tokens in
        let pom = if_error "correct_coordination1" correct_coordination1 pom tokens in
        let pom = if_error "correct_interp_with_father_0" correct_interp_with_father_0 pom tokens in
        (* File.file_out (path ^ "/pre_text_modified_" ^ (string_of_bool first_try) ^ ".html") (fun file ->
          Printf.fprintf file "%s\n" ENIAMvisualization.html_header;
          Printf.fprintf file "%s\n" (ENIAMvisualization.html_of_dep_sentence tokens paths);
          Printf.fprintf file "%s\n" ENIAMvisualization.html_trailer); *)
        let pom = if_error "corect_complm" corect_complm pom tokens in
        let pom =
            try
              correct_coordination2 false pom tokens
            with
            | _ -> (if_error "correct_coordination2" (correct_coordination2 true) pom tokens) in
        let pom = if_error "remove_interps," (remove_interps ",") pom tokens in
        let pom = if_error "correct_passive_voice" correct_passive_voice pom tokens in
        praet_qub_aglt pom tokens
      else
        paths in
        (* swap_dep paths tokens in *)
    (* File.file_out (path ^ "/pre_text_modified_" ^ (string_of_bool first_try) ^ ".html") (fun file ->
      Printf.fprintf file "%s\n" ENIAMvisualization.html_header;
      Printf.fprintf file "%s\n" (ENIAMvisualization.html_of_dep_sentence tokens paths);
      Printf.fprintf file "%s\n" ENIAMvisualization.html_trailer); *)
    let paths = translate_to_new_format paths in
    paths
