(*
 *  ENIAM_LCGlexicon is a library that provides LCG lexicon form Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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
open ENIAM_LCGtypes
open ENIAM_LCGlexiconTypes
open ENIAMcategoriesPL

let rec find_selector s = function
    (t,Eq,x :: _) :: l -> if t = s then x else find_selector s l
  | (t,_,_) :: l -> if t = s then failwith "find_selector 1" else find_selector s l
  | [] -> failwith "find_selector 2"

let rec get_syntax rev = function
    Syntax syntax :: rule -> syntax, (List.rev rev) @ rule
  | t :: rule -> get_syntax (t :: rev) rule
  | [] -> failwith "get_syntax"

let rec get_quant rev = function
    Quant quant :: rule -> quant, (List.rev rev) @ rule
  | t :: rule -> get_quant (t :: rev) rule
  | [] -> [],  List.rev rev

let rec get_bracket rev = function
    Bracket :: rule -> true, (List.rev rev) @ rule
  | t :: rule -> get_bracket (t :: rev) rule
  | [] -> false, List.rev rev

let rec get_raised rev = function
    Raised raised :: rule -> raised, (List.rev rev) @ rule
  | t :: rule -> get_raised (t :: rev) rule
  | [] -> raise Not_found

let rec get_sem_term rev = function
    Sem sem_term :: rule -> sem_term, (List.rev rev) @ rule
  | t :: rule -> get_sem_term (t :: rev) rule
  | [] -> raise Not_found

let merge_quant pos_quants quants =
  let map = Xlist.fold quants SelectorMap.empty (fun map (k,v) -> SelectorMap.add map k v) in
  let l,map = Xlist.fold pos_quants ([],map) (fun (l,map) (cat,v) ->
      if SelectorMap.mem map cat then (cat,SelectorMap.find map cat) :: l, SelectorMap.remove map cat
      else (cat,v) :: l, map) in
  List.rev (SelectorMap.fold map l (fun l cat v -> (cat,v) :: l))

let assign_quantifiers (selectors,rule,weight) =
  let pos = find_selector Pos selectors in
  let categories =
    try StringMap.find pos_categories pos
    with Not_found -> failwith ("assign_quantifiers: unknown part of speech " ^ pos) in
  let categories = Xlist.map categories (fun s -> s,Top) in
  let syntax,rule = get_syntax [] rule in
  let quant,rule = get_quant [] rule in
  let bracket,rule = get_bracket [] rule in
  let quant = merge_quant categories quant in
  selectors, (bracket,quant,syntax),(rule,weight)

let rec check_quantifiers_int_rec (selectors,syntax) quants = function
    Atom x -> ()
  | AVar "schema" -> ()
  | AVar x ->
     if not (SelectorSet.mem quants (selector_of_string x))
     then failwith ("Variable '" ^ x ^ "' is not quantified in rule " ^ string_of_selectors selectors ^ ": " ^ ENIAM_LCGstringOf.grammar_symbol 0 syntax)
  | With l -> Xlist.iter l (check_quantifiers_int_rec (selectors,syntax) quants)
  | Zero -> ()
  | Top -> ()

let rec check_quantifiers_rec rule quants  = function
    Tensor l -> Xlist.iter l (check_quantifiers_int_rec rule quants)
  | Plus l -> Xlist.iter l (check_quantifiers_rec rule quants)
  | Imp(s,d,t) -> check_quantifiers_rec rule quants s; check_quantifiers_rec rule quants t
  | One -> ()
  | ImpSet(s,l) -> check_quantifiers_rec rule quants s;  Xlist.iter l (fun (_,t) -> check_quantifiers_rec rule quants t)
  | Star s -> check_quantifiers_rec rule quants s
  | Maybe s -> check_quantifiers_rec rule quants s
  | _ -> failwith "check_quantifiers_rec"

let check_quantifiers (selectors,(bracket,quant,syntax),_) =
  let quants = Xlist.fold quant SelectorSet.empty (fun quants (q,_) -> SelectorSet.add quants q) in
  check_quantifiers_rec (selectors,syntax) quants syntax

let assign_semantics (selectors,(bracket,quant,syntax),(rule,weight)) =
  let semantics = try
    let raised,rule = get_raised [] rule in
    if rule <> [] then failwith "assign_semantics 1" else
      RaisedSem(Xlist.map quant fst, raised)
    with Not_found -> (try
      let term,rule = get_sem_term [] rule in
      if rule <> [] then failwith "assign_semantics 2" else
        TermSem(Xlist.map quant fst,term)
      with Not_found -> BasicSem(Xlist.map quant fst)) in
  selectors,(bracket,quant,syntax),(semantics,weight)

let rec add_x_args_rec = function
    Imp(s,d,t) -> Imp(add_x_args_rec s,d,t)
  | ImpSet(s,l) -> ImpSet(add_x_args_rec s,l)
  | Tensor[Atom "<conll_root>"] ->  Tensor[Atom "<conll_root>"]
  | Tensor l -> ImpSet(Tensor l,[Backward,Maybe(Tensor[Atom "X"]);Forward,Maybe(Tensor[Atom "X"])])
  | t -> failwith ("add_x_args_rec: " ^ ENIAM_LCGstringOf.grammar_symbol 0 t)

let is_raised_semantics = function
    RaisedSem _ -> true
  | _ -> false

let rec is_raised_arg = function
    Imp _ -> true
  | Tensor _ -> false
  | Plus l -> Xlist.fold l false (fun b t -> is_raised_arg t || b)
  | Maybe t -> is_raised_arg t
  | One -> false
  | t -> failwith ("is_raised_arg: " ^ ENIAM_LCGstringOf.grammar_symbol 0 t)

let rec is_raised_syntax = function
    Imp(s,d,t) -> is_raised_syntax s || is_raised_arg t
  | ImpSet(s,l) -> is_raised_syntax s || Xlist.fold l false (fun b (_,t) -> is_raised_arg t || b)
  | Tensor _ -> false
  | t -> failwith ("is_raised_syntax: " ^ ENIAM_LCGstringOf.grammar_symbol 0 t)


let add_x_args (selectors,(bracket,quant,syntax),(semantics,weight)) =
  if is_raised_syntax syntax then (selectors,(bracket,quant,syntax),(semantics,weight))
  else (selectors,(bracket,quant,add_x_args_rec syntax),(semantics,weight))

let rec extract_category pat rev = function
    (cat,rel,v) :: l -> if cat = pat then rel,v,(List.rev rev @ l) else extract_category pat ((cat,rel,v) :: rev) l
  | [] -> raise Not_found

let dict_of_grammar grammar =
  (* print_endline "dict_of_grammar"; *)
  Xlist.fold grammar StringMap.empty (fun dict (selectors,(bracket,quant,syntax),semantics) ->
      let pos_rel,poss,selectors = try extract_category Pos [] selectors with Not_found -> failwith "dict_of_grammar 1" in
      let lemma_rel,lemmas,selectors = try extract_category Lemma [] selectors with Not_found -> Eq,[],selectors in
      if pos_rel <> Eq || lemma_rel <> Eq then failwith "dict_of_grammar 2" else
        let rule = selectors,(bracket,quant,syntax),semantics in
        Xlist.fold poss dict (fun dict pos ->
            let dict2,l = try StringMap.find dict pos with Not_found -> StringMap.empty,[] in
            let dict2,l =
              if lemmas = [] then dict2,rule :: l else
                Xlist.fold lemmas dict2 (fun dict2 lemma ->
                    StringMap.add_inc dict2 lemma [rule] (fun l -> rule :: l)),l in
            StringMap.add dict pos (dict2,l)))

let make_rules x_flag filename =
  let lexicon = ENIAM_LCGlexiconParser.load_lexicon filename in
  let lexicon = List.rev (Xlist.rev_map lexicon assign_quantifiers) in
  Xlist.iter lexicon check_quantifiers;
  let lexicon = List.rev (Xlist.rev_map lexicon assign_semantics) in
  let lexicon = if x_flag then List.rev (Xlist.rev_map lexicon add_x_args) else lexicon in
  dict_of_grammar lexicon

let find_rules rules cats =
  let lex_rules,rules = try StringMap.find rules cats.pos with Not_found -> failwith ("find_rules: unable to find rules for category '" ^ cats.pos ^ "' lemma='" ^ cats.lemma ^ "'") in
  (* Printf.printf "find_rules: %s %s |rules|=%d\n" cats.lemma cats.pos (Xlist.size rules); *)
  let rules = try StringMap.find lex_rules cats.lemma @ rules with Not_found -> rules in
  Xlist.fold rules [] (fun rules (selectors,syntax,semantics) ->
      try
        let cats = apply_selectors cats selectors in
        (cats,syntax,semantics) :: rules
      with Not_found -> rules)

let prepare_lex_entries rules lex_entries cats =
  Xlist.fold lex_entries rules (fun rules (selectors,rule) ->
      let selectors = (Pos,Eq,[cats.pos]) :: selectors in
      let selectors,(bracket,quant,syntax),(rule,weight) = assign_quantifiers (selectors,[Syntax rule],0.) in
      let selectors,(bracket,quant,syntax),(semantics,weight) = assign_semantics (selectors,(bracket,quant,syntax),(rule,weight)) in
      try
        let cats = apply_selectors cats selectors in
        (cats,(bracket,quant,syntax),(semantics,weight)) :: rules
      with Not_found -> rules)

let assign_valence valence rules =
  Xlist.fold rules [] (fun l (cats,(bracket,quant,syntax),semantics) ->
      (* Printf.printf "%s %s |valence|=%d\n" cats.lemma cats.pos (Xlist.size valence); *)
      if ENIAM_LCGrenderer.count_avar "schema" syntax > 0 then
        Xlist.fold valence l (fun l (selectors,schema) ->
            try
              let cats = apply_selectors cats selectors in
              (cats,(bracket,quant,ENIAM_LCGrenderer.substitute_schema "schema" schema syntax),semantics) :: l
            with Not_found -> l)
      else (cats,(bracket,quant,syntax),semantics) :: l)

type labels = {
  number: string;
  case: string;
  gender: string;
  person: string;
  aspect: string;
}

let get_label e = function
    Number -> e.number
  | Case -> e.case
  | Gender -> e.gender
  | Person -> e.person
  | Aspect -> e.aspect
  | _ -> ENIAM_LCGreductions.get_variant_label ()

let get_labels () = {
  number=ENIAM_LCGreductions.get_variant_label ();
  case=ENIAM_LCGreductions.get_variant_label ();
  gender=ENIAM_LCGreductions.get_variant_label ();
  person=ENIAM_LCGreductions.get_variant_label ();
  aspect=ENIAM_LCGreductions.get_variant_label ();
}

let make_quantification e rules =
  Xlist.map rules (fun (cats,(bracket,quant,syntax),semantics) ->
      let syntax = Xlist.fold (List.rev quant) syntax (fun syntax (cat,t) ->
          let t = if t = Top then ENIAM_LCGrenderer.make_quant_restriction (match_selector cats cat) else t in
          let category = string_of_selector cat in
          WithVar(category,t,get_label e cat,syntax)) in
      let syntax = if bracket then ENIAM_LCGtypes.Bracket(true,true,syntax) else ENIAM_LCGtypes.Bracket(false,false,syntax) in
      cats,syntax,semantics)

let make_node id orth lemma pos syntax weight cat_list is_raised =
  let attrs = Xlist.fold cat_list [] (fun attrs -> function
      | Lemma -> attrs
      | Pos -> attrs
      | Pos2 -> attrs
      | Cat -> ("CAT",SubstVar "cat") :: attrs
      | Coerced -> ("COERCED",SubstVar "coerced") :: attrs
      | Number -> ("NUM",SubstVar "number") :: attrs
      | Case -> ("CASE",SubstVar "case") :: attrs
      | Gender -> ("GEND",SubstVar "gender") :: attrs
      | Person -> ("PERS",SubstVar "person") :: attrs
      | Grad -> ("GRAD",SubstVar "grad") :: attrs
      | Praep -> attrs
      | Acm -> ("ACM",SubstVar "acm") :: attrs
      | Aspect -> ("ASPECT", SubstVar "aspect") :: attrs
      | Negation -> ("NEGATION",SubstVar "negation") :: attrs
      | Mood -> ("MOOD", SubstVar "mood") :: attrs
      | Tense -> ("TENSE", SubstVar "tense") :: attrs
      | Nsyn -> ("NSYN", SubstVar "nsyn") :: attrs
      | Nsem -> ("NSEM", SubstVar "nsem") :: attrs
      | Ctype -> ("CTYPE", SubstVar "ctype") :: attrs
      | Mode -> ("MODE", SubstVar "mode") :: attrs
      | Psem -> ("PSEM", SubstVar "psem") :: attrs
      | Icat -> attrs
      | Inumber -> attrs
      | Igender -> attrs
      | Iperson -> attrs
      | Nperson -> attrs
      | Ncat -> attrs
      | Plemma -> attrs
      | Unumber -> attrs
      | Ucase -> attrs
      | Ugender -> attrs
      | Uperson -> attrs
      | Amode -> attrs) in
      (* | s -> (string_of_selector s, Dot) :: attrs) in *)
  (* | "lex" -> ("LEX",Val "+") :: attrs *)
  (* | s -> failwith ("make_node: " ^ (string_of_selector s))) in *)
  let symbol = if is_raised then
      ENIAM_LCGrenderer.make_raised_symbol syntax
    else ENIAM_LCGrenderer.make_symbol syntax in
  {ENIAM_LCGrenderer.empty_node with
     orth=orth; lemma=lemma; pos=pos; symbol=symbol;
     weight=weight; id=id; attrs=List.rev attrs; args=Dot}

let or_frame node =
  (*Imp(Imp(Imp(Tensor[Atom "<root>"],Forward,
              Tensor[Atom "</speaker>"]),Forward,
          Imp(Tensor[Atom "ip"; Top; Top; Top],Forward,Tensor[Atom "or"])),Forward,
      Tensor[Atom "or2"]),*)
  (* Lambda("x",Lambda("y",Lambda("z",Node{node with gs=make_gs [] ["<root>"]; args=Tuple[
    Cut(SetAttr("AROLE",Val "Clause",SetAttr("GF",Gf CLAUSE,App(Var "y",Var "x"))))]}))) *)
  VariantVar("lemma",Lambda("x",Lambda("y",Lambda("z",Node{node with args=Tuple[
    Cut(SetAttr("ARG_SYMBOL",Tuple[Val "TODO"],App(Var "y",Var "x")))]}))))

let make_term id orth rules =
  Xlist.map rules (fun (cats,syntax,(semantics,weight)) ->
      ENIAM_LCGrenderer.reset_variable_names ();
      ENIAM_LCGrenderer.add_variable_numbers ();
      (* print_endline ("make_term 0: " ^ ENIAM_LCGstringOf.grammar_symbol 0 syntax); *)
      match semantics with
        BasicSem cat_list ->
        let node = make_node id orth cats.lemma cats.pos syntax weight(*+.token.ENIAMtokenizerTypes.weight*) cat_list false in
        (* print_endline ("make_term 1: " ^ ENIAM_LCGstringOf.grammar_symbol 0 syntax); *)
        let semantics = ENIAM_LCGrenderer.make_term node syntax in
        ENIAM_LCGrenderer.simplify (syntax,semantics)
      | RaisedSem(cat_list,outer_cat_list) ->
        (* FIXME: jakie atrybuty powinien mieć outer node (w szczególności jaką wagę?) *)
        let node = make_node id orth cats.lemma cats.pos syntax weight(*+.token.ENIAMtokenizerTypes.weight*) cat_list true in
        let outer_node = make_node id orth cats.lemma cats.pos syntax weight(*+.token.ENIAMtokenizerTypes.weight*) outer_cat_list false in
        (* print_endline ("make_term 2: " ^ ENIAM_LCGstringOf.grammar_symbol 0 syntax); *)
        let semantics = ENIAM_LCGrenderer.make_raised_term node outer_node syntax in
        ENIAM_LCGrenderer.simplify (syntax,semantics)
      | TermSem(cat_list,"λxλyλz.NODE(yx,z)") ->
        let node = make_node id orth cats.lemma cats.pos syntax weight(*+.token.ENIAMtokenizerTypes.weight*) cat_list false in
        (* print_endline ("make_term 3: " ^ ENIAM_LCGstringOf.grammar_symbol 0 syntax); *)
        let semantics = or_frame node in
        ENIAM_LCGrenderer.simplify (syntax,semantics)
      | _ -> failwith "make_term: ni")

let create_entries rules id orth cats valence lex_entries =
  Xlist.fold cats [] (fun l cats ->
      (* Printf.printf "create_entries: orth=%s lemma=%s pos=%s\n" orth cats.lemma cats.pos; *)
      (* variable_name_ref := []; *)
      if cats.pos="interp" && cats.lemma="<clause>" then (BracketSet(Forward),Dot) :: l else
      if cats.pos="interp" && cats.lemma="</clause>" then (BracketSet(Backward),Dot) :: l else
      if (cats.pos2="noun" ||  cats.pos2="verb" ||  cats.pos2="adj" ||  cats.pos2="adv") && cats.cat="X" && not !default_category_flag && cats.pos <> "aglt" then l else
        let e = get_labels () in
        (* print_endline "create_entries 1"; *)
        let rules = find_rules rules cats in
        let rules = prepare_lex_entries rules lex_entries cats in
        (* Printf.printf "create_entries 2: %s %s |rules|=%d\n" cats.lemma cats.pos (Xlist.size rules); *)
        let rules = assign_valence valence rules in
        (* print_endline "create_entries 3"; *)
        let rules = make_quantification e rules in
        (* print_endline "create_entries 4"; *)
        let rules = make_term id orth rules in
        (* print_endline "create_entries 5"; *)
        rules @ l)
