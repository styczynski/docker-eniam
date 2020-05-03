(* Blame Szymon Rutkowski - szymon@szymonrutkowski.pl - Dec 2016-Jan 2017. *)

open Xstd
open FreqUtils

let simplify_qub tg =
        (* change qub:nwok/wok into plain qub *)
        try (Str.search_forward (Str.regexp "qub") tg 0;
             List.hd(Xstring.split ":" tg))
        with Not_found -> tg

let resolve_num etr inps =
        (* if word form of etr is purely numerical, assign to it the broadest
         * interpretation *)
        if not (str_contains etr.interp "num:") then etr
        else if (Str.string_match (Str.regexp "^[123456789IVXLCM]+$")
        etr.orth 0)
        then { etr with interp=longest_str inps }
        else { etr with interp=shortest_str inps }

let resolve_verb etr inps =
        (* choose interpretation of a verb that shares most chars with the
         * original one (should resolve perf.imperf issues etc.) *)
        let verbs = Xlist.rev_map ["praet:"; "imps:"; "imp"; "fin"; "inf";
        "ger:"; "pact:"; "ppas:"] (str_contains etr.interp) in
        let truth x = if x = true then true else false in
        if List.exists truth verbs
        then {etr with interp=strrev (most_samechars (Xlist.rev_map inps strrev)
                                                (strrev (etr.interp)))}
        else etr

let generalize etr itp_map sgjp_map =
        (* given an entry from frequency list, try to assign it a generalized
         * tag using itp_map and sgjp_map *)
        try (match uniq (StringMap.find itp_map (etr.interp))
        with
        | [] -> etr
        | h::[] -> { etr with interp=h }
        | inps -> resolve_verb (resolve_num
                        { etr with interp=(check_sgjp_interp etr sgjp_map) }
                        inps) inps)
        with Not_found -> (* Printf.printf "not found %s\n" etr.interp;*)
        {etr with interp= (simplify_qub etr.interp)}

let print_freq fname lst =
        print_list fname
        (Xlist.rev_map
        lst
        (fun etr -> Printf.sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
        etr.orth etr.lemma etr.interp etr.frq etr.compos etr.sgjp_status
        etr.word_type etr.corr etr.rule_id etr.cat))

let merge_entries lst =
        (* Given a list of freq entries, merge those that have the same (orth,
         * lemma, interp) triple if lowercased, summing the frequencies *)
        let lwr = Xunicode.lowercase_utf8_string in
        let etr_pnts = Xlist.fold lst StringMap.empty
        (fun m etr -> StringMap.add_inc m
                (lwr (join "~" [etr.orth; etr.lemma; etr.interp]))
                [etr] (fun l -> etr :: l))
        in
        StringMap.fold etr_pnts []
        (fun l k v -> let e = (List.hd v) in
        {orth=(lwr e.orth); lemma=(lwr e.lemma); interp=e.interp;
         (* sum the frequencies *)
         frq=string_of_int
                 (Xlist.fold v 0 (fun cnt vl -> cnt+(int_of_string vl.frq)));
         (* prefer the most general value of those parameters, among present in
          * all merged entries: *)
         compos=prefer ["COMPOS"; "COMPOS-ndm"; "COMPOS-*"; "COMPOS-ALT";
         "COMPOS-LWR"; "COMPOS-LWR-ndm"; "COMPOS-LWR-*"; "COMPOS-LWR-ALT";
         "NCOMPOS"] (Xlist.rev_map v (fun e -> e.compos));
         sgjp_status=prefer ["SGJP-EXACT"; "SGJP-LMM-UNCAPITAL";
         "SGJP-LMM-CAPITAL"; "SGJP-LMM-LOWER"; "SGJP-BTH-LOWER"; "NON-SGJP"]
         (Xlist.rev_map v (fun e -> e.sgjp_status));
         word_type=prefer ["CW"; "NCH"; "EXT"; "SYMB"; "ACRO"; "PN"; "SPEC";
         "NEOL"; "COMPD"; "WEB"] (Xlist.rev_map v (fun e -> e.word_type));
         corr=prefer ["CORR"; "CERR"; "DIAL"; "PHON"; "ERR"; "TAGD"; "PLTAN";
         "TAGE"; "ERR-TAGE"; "CERR-TAGE"] (Xlist.rev_map v (fun e -> e.corr));
         rule_id=""; cat=""}
         :: l)

let tag_rules alt_fname lst =
        (* tag freq entries with their rule ids - loading the exceptions from
         * alt_fname - currently rules ambiguity are resolved by choosing the
         * most specific rule *)
        let alts =
                Xlist.fold
                        (File.load_tab alt_fname (fun [orth; _; _] -> orth))
                        StringMap.empty (fun m e -> StringMap.add m e "")
        in
        Xlist.rev_map lst
        (fun etr ->
                if str_contains etr.compos "COMPOS-ALT"
                (* COMPOS-LWR-ALT are left off here *)
                then {etr with rule_id="ALT"}
                else if etr.compos="NCOMPOS" then etr
                else let rls = freq_etr_rules etr in
                if List.length rls = 0 then
                (if etr.compos!="NCOMPOS" then
                ((*Printf.printf "can't find rule for COMPOS %s\n" etr.orth;*)
                {etr with compos="NCOMPOS"})
                else etr)
                (* extract the first rule from the list: *)
                else let hdrule = (match List.hd rls with (_, rl) -> rl) in
                (* if it's the only one, just return it: *)
                if List.length rls = 1 then {etr with rule_id=hdrule.id}
                else ((*Printf.printf "too many rules match %s (%s )\n"
                        etr.orth (Xlist.fold rls "" (fun str r -> str ^ " " ^
                               (match r with (_, r) -> r.id)));*)
                      (* select the rule with the longest "set" (agglutinant) *)
                      {etr with rule_id = (Xlist.fold rls hdrule 
                              (fun choice opt ->
                               match opt with (_, opt) ->
                               if String.length choice.set<String.length opt.set
                               then opt else choice)).id}))

let _ =
        let itp_list = File.load_tab "data/interps_general.tab"
        (function [_; tag; _] -> tag
        | [] -> failwith "Empty entry in the interp file"
        | _::_ -> failwith "Malformatted entry in the interp file")
        in
        let sgjp_map = create_sgjp_map_nohyph
                "../../NLP resources/sgjp-20160724.tab"
        in
        (* generalize frequency: *)
        let gen_freq =
        (tag_cats
        (tag_rules "../resources/SGJP/alt.tab"
        (merge_entries
          (let interp_map = (create_interp_map itp_list) in
          let freq = File.load_tab
          "../resources/NKJP1M/NKJP1M-tagged-frequency.tab"
          (function [o;l;i;f;c;s;w;cr] -> { orth=o; lemma=l; interp=i; frq=f;
          compos=c; sgjp_status=s; word_type=w; corr=cr; rule_id=""; cat=""}
          | [] -> failwith "Empty entry in the freq file"
          | _::_ -> failwith "Malformatted entry in the freq file") in
          (* after loading the list, prune it and perform generalization *)
          Xlist.rev_map
          (List.filter
            (* filter out errors and symbols *)
          (fun etr -> etr.corr = "CORR" && not (etr.word_type = "SYMB"
          || etr.word_type = "COMPD" || etr.word_type = "WEB"
          || etr.word_type = "ACRO"))
          freq)
          (fun etr -> generalize etr interp_map sgjp_map)))))
        in

        (** print the generalized frequency to file *)
        print_freq "../resources/NKJP1M/NKJP1M-generalized-frequency.tab"
        gen_freq;
