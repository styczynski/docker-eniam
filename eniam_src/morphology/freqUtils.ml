(* Blame Szymon Rutkowski - szymon@szymonrutkowski.pl - Dec 2016-Jan 2017. *)

open Xstd

(* utils *)

let uniq lst =
        let seen = Hashtbl.create (List.length lst) in
        List.filter (fun x -> let tmp = not (Hashtbl.mem seen x) in
                Hashtbl.replace seen x ();
                tmp) lst

let rec join delim lst =
        (* Concatenate list of strings into one string, delimited by delim *)
        if lst = [] then ""
        else List.hd lst ^ (if List.tl lst = [] then ""
                            else delim ^ join delim (List.tl lst))

let slice lst b e =
        (* Given a list, return a slice from b to e *)
        let rec slice_step lst b e accm =
        if lst = [] || b >= e then []
        else if e = 0 then accm
        else if b > 0 then slice_step (List.tl lst) (b-1) (e-1) accm
        else if b = 0 then
                slice_step (List.tl lst) (b-1) (e-1) (List.hd lst :: accm)
        else slice_step lst 0 e accm
        in
        List.rev (slice_step lst b e [])

let str_contains str frag =
        try (Str.search_forward (Str.regexp frag) str 0; true)
        with Not_found -> false

let longest_str lst =
        let rec longest_str_step lst win winlen =
                if lst = [] then win
                else if String.length (List.hd lst) > winlen 
                then longest_str_step (List.tl lst) (List.hd lst)
                                                (String.length (List.hd lst))
                else longest_str_step (List.tl lst) win winlen
        in
        longest_str_step lst "" 0

let shortest_str lst =
        let rec shortest_str_step lst win winlen =
                if lst = [] then win
                else if String.length (List.hd lst) < winlen 
                then shortest_str_step (List.tl lst) (List.hd lst)
                                                (String.length (List.hd lst))
                else shortest_str_step (List.tl lst) win winlen
        in
        shortest_str_step lst "" 999999

let most_samechars lst patrn =
        (* Return a string from lst that shares of the same characters in its
         * beginning with patrn. If no characters are the same, return the 
         * patrn. *)
        let rec samechars s1 s2 cnt =
                if s1 = "" || s2 = "" then cnt
                else if String.get s1 0 = String.get s2 0
                then samechars (String.sub s1 0 ((String.length s1)-1))
                (String.sub s1 0 ((String.length s1)-1)) (cnt+1)
                else cnt
        in
        let rec most_samechars_step lst patrn win winscr =
                if lst = [] then win
                else let score = (samechars (List.hd lst) patrn 0) in
                if score > winscr 
                then most_samechars_step (List.tl lst) patrn (List.hd lst) score
                else most_samechars_step (List.tl lst) patrn win winscr
        in
        most_samechars_step lst patrn patrn 0

let prefer pfs lst =
        (* Return first element from pfs that was found in lst. Throw Not_found
         * if nothing was found. pfs can't contain empty string. *)
        if lst = [] || pfs = [] then raise Not_found
        (* for each element from pfs, try to find it on lst *)
        else let findings = List.map
        (fun p -> try (List.find (fun x -> x = p) lst) with Not_found -> "")
        pfs
        in
        (* try to find some non-false finding: *)
        List.find (fun f -> String.length f > 0) findings
        (* Not_found uncatched /\ *)

let strrev s =
        (* Reverse a string. *)
        let rec strrev_step s ns =
                if s = "" then ns
                else strrev_step (String.sub s 1 ((String.length s)-1))
                                ((String.make 1 (String.get s 0)) ^ ns)
        in
        strrev_step s ""

let print_strlst lst =
        Printf.printf "[";
        Xlist.iter
        lst
        (fun e -> Printf.printf " %s " e);
        Printf.printf "]"

let strmap_contains m e = try (StringMap.find m e; true)
                                with Not_found -> false

let map_from_list lst idx_fun =
        (* return a StringMap, containing all entries sorted by string hash
         * obtained by applying idx_fun to given element of lst *)
        Xlist.fold lst StringMap.empty
        (fun m elem -> StringMap.add_inc m (idx_fun elem) [elem]
                (fun l -> elem :: l))

(* working code *)

let neutx_to_neut tag = Str.global_replace
                        (Str.regexp ("\(n[0-9]\.?\)*n[0-9]"))
                        "n" tag

let lists_of_tag tag =
                (Xlist.rev_map
                (Xstring.split ":" (neutx_to_neut tag))
                (fun elem ->
                Xstring.split "\." elem))

let variants_of tag = 
        (* variants_of takes a tag as string and returns all its variants as a
         * list of strings, unfolding all the variants *)
        let rec fold_as_str = fun l ->
                if l = [] then "" else (List.hd l)
                (* add the colon if needed: *)
                ^ (if (List.tl l) = [] then (fold_as_str (List.tl l))
                else (":" ^ fold_as_str (List.tl l)))
        in
        uniq
        (Xlist.rev_map
                (Xlist.multiply_list
                        (List.rev (* tag elems got reversed earlier *) 
                        (lists_of_tag tag)))
                fold_as_str)

let regexp_of_tag tg =
        Str.regexp
        (join ":" (Xlist.rev_map (lists_of_tag tg)
        (fun variants -> "\(\(" ^ (join "\|" variants) ^ "\)\.?\)+")))

let create_interp_map itp_list =
        (* create_interp takes itp_list and creates a string map, indexed by
         * short tags, to the long (full) tags *)
        Xlist.fold itp_list StringMap.empty
        (fun smap tag ->
                let tag = neutx_to_neut tag in
                (Xlist.fold
                (variants_of tag)
                smap
                (fun smap short_tag ->
                        (StringMap.add_inc smap short_tag [tag]
                        (fun l -> tag :: l)))))

type sgjp_entry = { sg_orth: string; sg_lemma: string; sg_interp: string}

let create_sgjp_map_nohyph fname =
        (* load SGJP from the fname, as a map indexed by word forms, removing
         * all entries that contain a hyphen *)
        let clean_lemma lm = List.hd (Xstring.split ":" lm) in
        map_from_list
        (List.filter (fun s -> not (str_contains s.sg_orth "-"))
                (File.load_tab fname
                (function [sg_orth; sg_lemma; sg_interp; _; _] ->
                        {sg_orth=sg_orth; sg_lemma=clean_lemma sg_lemma;
                        sg_interp=(neutx_to_neut sg_interp)}
                | [] -> failwith "Empty entry in SGJP file"
                | _::_ -> {sg_orth=""; sg_lemma=""; sg_interp=""})))
        (fun etr -> etr.sg_orth)

type freq_entry = { orth:string; lemma:string; interp:string; frq:string;
        compos:string; sgjp_status:string; word_type:string; corr:string;
        rule_id:string; cat:string }

let check_sgjp_interp etr sgjp =
        (* return interpretation for the entry, if possible, using sgjp (map) *)
        try (let retrieved = (StringMap.find sgjp etr.orth) in
            if List.length retrieved = 1
            && (List.hd retrieved).sg_lemma = etr.lemma
            then (List.hd retrieved).sg_interp
            else "AMBIG-" ^ etr.interp)
        with Not_found -> "AMBIG-" ^ etr.interp

let print_list fname lst =
        (let out = open_out fname in
        (Xlist.iter lst (fun etr -> output_string out etr));
        close_out out)

let freq_etr_rules etr =
        (* Given a freq entry, return a list of matching freq_rule records, as
         * in rules.ml (load_freq_rules) *)
        Xlist.filter
                (Rules.CharTrees.find Inflexion.rules etr.orth)
                (fun o -> match o with
                (stem, rl) -> ((stem ^ rl.set) = etr.lemma)
                && (Str.string_match (regexp_of_tag etr.interp)
                        (neutx_to_neut rl.interp) 0))

let cat_of_tag tag = match List.hd (Xstring.split ":" tag) with
        | "subst" -> "noun" | "depr" -> "noun"
        | "adj" -> "adj" | "adja" -> "adj" | "adjc" -> "adj" | "adjp" -> "adj"
        | "adv" -> "adv"
        | "inf" -> "verb" | "praet"-> "verb" | "fin" -> "verb"
        | "ppas" -> "verb" | "pact" -> "verb" | "pacta" -> "verb"
        | "impt" -> "verb" | "imps" -> "verb" | "pcon" -> "verb"
        | "pant" -> "verb" | "ger" -> "verb"
        | "bedzie" -> "other" | "pred"-> "other" | "prep" -> "other"
        | "num" -> "other" | "aglt" -> "other" | "winien" -> "other"
        | "qub" -> "other" | "brev" -> "other" | "comp" -> "other"
        | "interj" -> "other" | "burk" -> "other" | "numcol" -> "other"
        | "conj" -> "other" | "ppron12" -> "other" | "ppron3" -> "other"
        | "ppron2" -> "other" | "ppron1" -> "other" | "ppron" -> "other"
        | "interp" -> "other" | "xxx" -> "other" | "siebie" -> "other"
        | "cond" -> "cond"
        | _ -> failwith
        (Printf.sprintf "unknown part of speech in %s" tag)

let tag_cats lst =
        Xlist.rev_map lst (fun etr -> {etr with cat = cat_of_tag etr.interp })

let sum_list_freq lst =
        Xlist.fold lst 0 (fun t etr -> t+(int_of_string etr.frq))

let map_interp_given_cat freq =
        (* map_interp_given_cat prepares a map from cat's to map of interps to
         * their (normalized->probability) scores *)
        let normalize scoretable =
                StringMap.fold scoretable StringMap.empty
                (fun sctb key vals ->
                        (* count total frequency for this cat *)
                        let total = float_of_int
                                (StringMap.fold vals 0 (fun t _ f -> t + f))
                        in
                        (StringMap.add sctb key
                                (* normalize for each interp *)
                                (StringMap.fold vals StringMap.empty
                                (fun newvals v s -> StringMap.add newvals v
                                 ((float_of_int s) /. total)))))
        in
        normalize
        (Xlist.fold freq StringMap.empty
        (fun m etr -> StringMap.add_inc m etr.cat
                (* create an empty map, and add the first entry: *)
                (StringMap.add StringMap.empty etr.interp
                        (int_of_string etr.frq))
                (* if map exists, add or increment entry for this interp: *)
                (fun itps -> StringMap.add_inc itps etr.interp
                          (int_of_string etr.frq)
                          (fun score -> score+(int_of_string etr.frq)))))

let is_root_form_tag tg =
        (* consult "Narodowy Korpus Języka Polskiego", Warszawa 2012, p. 69-70 *)
        if str_contains tg "^subst:sg:nom" then true
        else if str_contains tg "^subst:pl:nom.*:p[0-9]" then true
        else if str_contains tg "^\(adj.?|num|numcol\):sg:nom.*m1" then true
        else if str_contains tg "^ppron.*nom" then true
        else if str_contains tg "^inf" then true
        else if str_contains tg "^winien:sg.*m1" then true
        else if str_contains tg "^\(siebie|pred|prep|conj|comp|interj|burk|qub|xxx|interp\)"
        then true
        else false
