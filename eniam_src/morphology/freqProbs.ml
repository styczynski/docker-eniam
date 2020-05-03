(* Blame Szymon Rutkowski - szymon@szymonrutkowski.pl - Dec 2016-Jan 2017. *)

open FreqUtils
open Xstd

let count_fsuf_freq freq rules_by_id =
        (* Return a map: fsuf (find) in rules -> frequency to the matching 
         * forms *)
        Xlist.fold freq StringMap.empty
        (fun map etr ->
                if etr.rule_id = "" || etr.rule_id = "ALT" then map
                else let fsuf =
                (* we receive a list here, so unpack the only element *)
                (match try (List.hd (StringMap.find rules_by_id etr.rule_id))
                with _ -> failwith (Printf.sprintf "can't find rule %s" etr.rule_id)
                with
                | [id; fsuf] -> fsuf | _ -> failwith "bad entry in rule map")
                in
                StringMap.add_inc map fsuf (int_of_string etr.frq)
                (fun accum -> accum + (int_of_string etr.frq)))

let count_fsuf_cat_freq freq rules_by_id =
        (* Return a map: fsuf (find) in rules -> frequency to the matching 
         * forms *)
        Xlist.fold freq StringMap.empty
        (fun map etr ->
                if etr.rule_id = "" || etr.rule_id = "ALT" then map
                else if etr.cat = "" then failwith etr.orth
                else let fsuf =
                (* we receive a list here, so unpack the only element *)
                (match try (List.hd (StringMap.find rules_by_id etr.rule_id))
                with _ -> failwith (Printf.sprintf "can't find rule %s" etr.rule_id)
                with
                | [id; fsuf] -> fsuf | _ -> failwith "bad entry in rule map")
                in
                StringMap.add_inc map (fsuf^"~+~"^etr.cat) (int_of_string etr.frq)
                (fun accum -> accum + (int_of_string etr.frq)))

let _ =
        let gen_freq =
          (List.filter
            (* filter out errors and symbols *)
          (fun etr -> etr.corr = "CORR" && not (etr.word_type = "SYMB"
          || etr.word_type = "COMPD" || etr.word_type = "WEB"
          || etr.word_type = "ACRO"))
          (File.load_tab
          "../resources/NKJP1M/NKJP1M-generalized-frequency.tab"
          (function [o;l;i;f;c;s;w;cr;ri;ct] -> { orth=o; lemma=l; interp=i;
          frq=f; compos=c; sgjp_status=s; word_type=w; corr=cr; rule_id=ri;
          cat=ct}
          | [] -> failwith "Empty entry in the freq file"
          | _::_ -> failwith "Malformatted entry in the freq file")))
        in
        (* count the total frequency of all entries *)
        let total_freq = Xlist.fold gen_freq 0
        (fun tally etr -> tally + int_of_string etr.frq)
        in
        let float_total_freq = float_of_int total_freq
        in

        (* split the gen_freq into in_sgjp i non_sgjp parts *)
        let in_sgjp_freq = (List.filter
        (fun etr -> etr.sgjp_status != "NON-SGJP")
        gen_freq) in
        let non_sgjp_freq = (List.filter
        (fun etr -> etr.sgjp_status = "NON-SGJP")
        gen_freq) in
        let float_total_insgjp_freq = float_of_int (sum_list_freq in_sgjp_freq)
        in
        let float_total_nonsgjp_freq = float_of_int (sum_list_freq non_sgjp_freq)
        in

        (* make a map of the freq, indexed by word forms *)
        let freq_map = map_from_list gen_freq (fun etr -> etr.orth)
        in
        (* and another by lemma:cat *)
        let freq_insgjp_map_lmcat = map_from_list in_sgjp_freq
        (fun etr -> etr.lemma ^ "~" ^ etr.cat)
        in
        let freq_insgjp_map_lmitp = map_from_list in_sgjp_freq
        (fun etr -> etr.lemma ^ "~" ^ etr.interp)
        in
        (* count P(interp|cat)'s *)
        let insgjp_itp_given_cat = map_interp_given_cat in_sgjp_freq
        in 

        (* prepare a map of rules: id -> rule entry (as Rules) *)
        let rules_by_id =
                (map_from_list
                (* load the freq_rules *)
                (File.load_tab "../resources/SGJP/freq_rules.tab"
                (function [id; _; _; _; fsuf; _; _] -> [id; fsuf]
                | _ -> failwith "bad entry in freq_rules.tab"))
                (* index by id *)
                (function [id; fsuf] -> id
                | _ -> failwith "error when making a map of freq rules"))
        in

        (* print probabilities to be used by the model for SGJP entries *)
        print_list "doc/prob_lemmacat.txt"
        (StringMap.fold freq_insgjp_map_lmcat []
        (fun lst lmcat vnts -> Printf.sprintf "%s\t%f\n"
        (Str.global_replace (Str.regexp "~") "\t" lmcat)
        ((float_of_int (sum_list_freq vnts)) /. float_total_insgjp_freq) :: lst));

        print_list "doc/prob_itp_givencat.txt"
        (StringMap.fold insgjp_itp_given_cat []
        (fun lst cat itps ->
                lst @
                StringMap.fold itps [] (fun ilst itp prob ->
                        Printf.sprintf "%s\t%s\t%f\n" itp cat prob :: ilst)));

        let fsuf_probs = count_fsuf_freq non_sgjp_freq rules_by_id in
        print_list "doc/prob_fsuf.txt"
        (List.sort (fun e1 e2 -> compare
        (StringMap.find fsuf_probs (List.hd (Xstring.split_delim "\\\t" e2)))
        (StringMap.find fsuf_probs (List.hd (Xstring.split_delim "\\\t" e1))))
        (StringMap.fold fsuf_probs []
        (fun lst fsuf frq -> Printf.sprintf "%s\t%f\t%d\n" fsuf
        ((float_of_int frq) /. float_total_insgjp_freq) frq :: lst)));

        let fsuf_cat_probs = count_fsuf_cat_freq non_sgjp_freq rules_by_id in
        print_list "doc/prob_fsuf_cat.txt"
        (List.sort (fun e1 e2 -> compare
        (StringMap.find fsuf_cat_probs
        (join "~+~" (slice (Xstring.split_delim "\\\t" e2) 0 2)))
        (StringMap.find fsuf_cat_probs
        (join "~+~" (slice (Xstring.split_delim "\\\t" e1) 0 2))))
        (StringMap.fold fsuf_cat_probs []
        (fun lst fsufcat_str frq ->
        let fsufcat = Str.split_delim (Str.regexp "~\+~") fsufcat_str in 
        try
        (Printf.sprintf "%s\t%s\t%f\t%d\n" (List.nth fsufcat 0) (List.nth fsufcat 1)
        ((float_of_int frq) /. float_total_insgjp_freq) frq) :: lst
        with _->(Printf.printf "problem with fsufcat %s -> %d\n"
        fsufcat_str frq; lst))))
