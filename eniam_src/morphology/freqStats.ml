(* Blame Szymon Rutkowski - szymon@szymonrutkowski.pl - Dec 2016-Jan 2017. *)

open Xstd
open FreqUtils

let print_rules_freq rfreq rules_by_id fname =
        (* given a rfreq (rule id -> its frequency), and index of rules by their ids
         * and fname, print the rules from rfreq to file (in fname), adding to the
         * original data from rules_by_id the frequency from rfreq as 3rd column *)
        let string_of_star = function
                | Types.Productive -> ""
                | Types.Star -> "*"
                | Types.Ndm -> "ndm" in
        (print_list fname 
        (StringMap.fold rfreq []
        (fun lst rid frq ->
                if rid = "" then lst else 
                let (r_etr : Types.rule) =
                        List.hd (StringMap.find rules_by_id rid) in
                Printf.sprintf "%s\t%d\t%d\t%s\t%s\t%s\t%s\t%s\n" r_etr.id
                r_etr.freq frq (string_of_star r_etr.star)
                r_etr.pref r_etr.find r_etr.set r_etr.interp:: lst)))

let model_prob freq_map freq_map_lmcat itp_given_cat float_total_freq orth interp cat = 
        (* P (form|lemma, cat, interp): *) (1.0
        *. ((((* P(lemma, cat): *)
            (float_of_int
                (if strmap_contains freq_map_lmcat
                (interp ^ "~" ^ cat)
                (* sum frequencies of entries with given lemma:cat: *)
                then sum_list_freq (StringMap.find freq_map_lmcat
                (interp ^ "~" ^ cat)) + 1
                else 1))
            /. float_total_freq)
            (* P(interp|cat) - precalculated probability: *)
            *. try (let itps_for_cat = StringMap.find itp_given_cat cat in
                StringMap.find itps_for_cat interp)
               with Not_found ->
                        ((*Printf.printf (*failwith*) "Can't find freq for interp: %s\n"
                        interp*) 0.0))
        /. (* P(form): *)
        (float_of_int
        (if strmap_contains freq_map orth
        then sum_list_freq (StringMap.find freq_map orth) + 1
        else 1)
        /. float_total_freq)))

let all_model_probs orth itp_list mod_prob_func =  
        (* for interps from itp_list, return a tuple (probability, interp) *)
        Xlist.map itp_list
        (fun itp -> ((mod_prob_func orth itp (cat_of_tag itp)), itp))

let normalize_probs probs =
        let total = Xlist.fold probs 0.0 (fun t (p, i) -> t +. p) in
        Xlist.map probs (fun (p, i) -> (p /. total, i))

let most_probable probs threshold =
        (* probs contains tuples (prob, interp), return interps such that
         * their accumulated probability minimally exceeds the threshold *)
        List.map
        (fun (p, i) -> i)
        (Xlist.fold (List.sort compare probs) []
        (fun lst (prob, itp) ->
                if (Xlist.fold lst 0.0 (fun sum (p, i) -> sum +. p)) > threshold
                then lst else (prob, itp) :: lst))

let eval_model threshold freq_map itp_lst float_total_freq mod_prob_func =
        StringMap.fold freq_map 0.0
        (fun accum form etrs ->
        let total_local_freq = float_of_int (sum_list_freq etrs) in
        accum +.
        (* weight of this form: *)
        (total_local_freq /. float_total_freq)
        *. ((float_of_int (sum_list_freq
                (* get the interps below the threshold *)
                (Xlist.fold (most_probable
                (normalize_probs (all_model_probs form itp_lst mod_prob_func))
                threshold)
                []
                (* get entries for these interps, so we'll sum their freqs *)
                (fun lst interp ->
                        try ((List.find (fun etr -> etr.interp = interp) etrs)
                        :: lst)
                        with _ -> lst))))
        /. total_local_freq))

let _ =
        let itp_list = File.load_tab "data/interps_general.tab"
        (function [_; tag; _] -> tag
        | [] -> failwith "Empty entry in the interp file"
        | _::_ -> failwith "Malformatted entry in the interp file")
        in
        let sgjp_map = create_sgjp_map_nohyph
                "../../NLP resources/sgjp-20160724.tab"
        in
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
        let freq_insgjp_map = map_from_list in_sgjp_freq (fun etr -> etr.orth)
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

        (** print the number of forms from freq that are not NON-SGJP *)
        (Printf.printf "All forms that are in freq and not NON-SGJP %d/%d\n"
        (sum_list_freq non_sgjp_freq)
        total_freq;
        
        (* print the number of forms from freq that are present in SGJP, but not
         * in their correct interpretations *)
        (Printf.printf "Forms present in SGJP, w/o the correct interp: %d/%d\n"
        (Xlist.fold in_sgjp_freq 0
        (fun tally etr ->
        if strmap_contains sgjp_map etr.orth
        && List.length (Xlist.filter (StringMap.find sgjp_map etr.orth)
               (fun sg_etr -> sg_etr.sg_lemma = etr.lemma
                 && Str.string_match (regexp_of_tag etr.interp)
                            sg_etr.sg_interp 0))
        = 0
        then ((*Printf.printf "%s\n" etr.orth; (*FIXME printing 'traps' to stdout...*)*)
        (tally+int_of_string etr.frq)) else tally))
        total_freq);
    
        (** count non-uniform forms in SGJP *) 
        (let out = open_out "doc/multi_forms.txt" in
        StringMap.iter 

        (* fold SGJP into a map (lemma~interp)->their occurences *) 
        (StringMap.fold sgjp_map StringMap.empty
        (fun m _ entries ->
        Xlist.fold entries m 
        (fun m e -> let ident = (e.sg_lemma ^ "~" ^ e.sg_interp) in
        StringMap.add_inc m ident [e.sg_orth] (fun l -> e.sg_orth :: l))))

        (* for each entry in this map, print it if has more >1 occurence*)
        (fun k fs -> let fs = uniq fs in
        if List.length fs > 1
        then Printf.fprintf out "\nMore than 1 form of %s: %s %s" k
        (Xlist.fold fs "" (fun str fm -> str ^ " " ^ fm))
        (* look up the freq and print the entries if relevant *)
        (join " "
                (List.map (fun etr -> "(from freq "^etr.orth^": "^etr.frq^")")
                (if strmap_contains freq_insgjp_map_lmitp k
                then StringMap.find freq_insgjp_map_lmitp k else [])))
        else ());
        close_out out);

        (* rules productivity in the non-sgjp part *)
        let rules_by_id = map_from_list
        (Rules.load_freq_rules "../resources/SGJP/freq_rules.tab")
        (fun r -> r.id)
        in
        let rules_freq = (Xlist.fold non_sgjp_freq StringMap.empty
        (fun m etr -> StringMap.add_inc m etr.rule_id (int_of_string etr.frq)
        (fun n -> n + (int_of_string etr.frq))))
        in
        (* split the rules_freq into parts containing: 1. rules that alternate
         * the form from lemma, 2. as 1 but the form is the basic form of the
         * lexeme 3. alternating rules *)
        let (non_alt, non_alt_root, alt) = StringMap.fold rules_freq
        (StringMap.empty, StringMap.empty, StringMap.empty)
        (fun (non_alt, non_alt_root, alt) rid frq ->
                if rid = "" then (non_alt, non_alt_root, alt)
                else let (r_etr : Types.rule) =
                        List.hd (StringMap.find rules_by_id rid) in
                if r_etr.find = r_etr.set
                        then if r_etr.find!="" && is_root_form_tag r_etr.interp
                        then (non_alt, StringMap.add non_alt_root rid frq, alt)
                        else (StringMap.add non_alt rid frq, non_alt_root, alt)
                else (non_alt, non_alt_root, StringMap.add alt rid frq))
        in
        (print_rules_freq rules_freq rules_by_id "doc/rules_productivity.txt";
        print_rules_freq non_alt rules_by_id "doc/rules_productivity_nalt.txt";
        print_rules_freq non_alt_root rules_by_id "doc/rules_productivity_nalt_root.txt";
        print_rules_freq alt rules_by_id "doc/rules_productivity_alt.txt");

        Printf.printf "Model evaluation: %f\n"
        (eval_model 0.95 freq_insgjp_map itp_list
        float_total_insgjp_freq
        (* model probability function: *)
        (model_prob freq_insgjp_map freq_insgjp_map_lmcat insgjp_itp_given_cat
        float_total_insgjp_freq)))
