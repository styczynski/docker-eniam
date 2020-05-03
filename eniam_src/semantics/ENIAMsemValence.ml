(*
 *  ENIAMsemantics implements semantic processing for ENIAM
 *  Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2017 Institute of Computer Science Polish Academy of Sciences
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

open ENIAM_LCGtypes
open ENIAM_LCGlexiconTypes
open ENIAMlexSemanticsTypes
open Xstd

type pos = {role: linear_term; role_attr: linear_term; selprefs: linear_term; catprefs: string list; gf: ENIAMwalTypes.gf;
  cr: string list; ce: string list;
  is_necessary: bool; is_pro: bool; is_prong: bool; is_multi: bool; dir: string; morfs: StringSet.t}

let get_pro_lemma attrs =
  let pers,num,gend = Xlist.fold attrs ("","",[]) (fun (pers,num,gend) -> function
      "PERS",Val s -> s,num,gend
    | "NUM",Val s -> pers,s,gend
    | "GEND",Val s -> pers,num,[s]
    | "GEND",Variant(_,l) -> pers,num,Xlist.map l (function (_,Val s) -> s | _ -> failwith "get_pro_lemma")
    | _ -> failwith "get_pro_lemma") in
  match pers,num with
    "",_ -> "pro"
  | "pri","" -> "pro1"
  | "pri","sg" -> "ja"
  | "pri","pl" -> "my"
  | "sec","" -> "pro2"
  | "sec","sg" -> "ty"
  | "sec","pl" -> "wy"
  | "ter","" -> "pro3"
  | "ter","sg" ->
       (match Xlist.fold gend (false,false,false) (fun (m,n,f) -> function
           "m1" -> true,n,f
         | "m2" -> true,n,f
         | "m3" -> true,n,f
         | "n" -> m,true,f
         (* | "n1" -> m,true,f
         | "n2" -> m,true,f *)
         | "f" -> m,n,true
         | _ -> m,n,f) with
         true,false,false -> "on"
       | false,true,true -> "ono"
       | false,false,true -> "ona"
       | _ -> "pro3sg")
  | "ter","pl" ->
       (match Xlist.fold gend (false,false) (fun (mo,nmo) -> function
           "m1" -> true,nmo
         | "p1" -> true,nmo
         | _ -> mo,true) with
         true,false -> "oni"
       | false,true -> "one"
       | _ -> "pro3pl")
  | _ -> failwith "get_pro_lemma"

let make_sem_args sem_args =
  if sem_args = [] then Dot else ENIAM_LCGrules.make_variant (Xlist.map sem_args (fun s -> Val s))

(*let rec match_value v2 = function
    Val v -> if v = v2 then Val v else raise Not_found
  | Variant(e,l) -> (* Przykład kiedy warianty niepasujące do selektora są rozproszone po drzewie np "Wiele wody płynie." *)
      let chosen = Xlist.fold l [] (fun chosen (i,t) -> try let _ = match_value v2 t in i :: chosen with Not_found -> chosen) in
      if chosen = [] then raise Not_found else
      if Xlist.size chosen = Xlist.size l then Variant(e,l) else
      (*Choice(e,chosen,Variant(e,l))*)failwith "match_value: ni"
  | t -> failwith ("match_value: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec match_neg_value vals = function
    Val v -> if Xlist.mem vals v then raise Not_found else Val v
  | Variant(e,l) ->
      let chosen = Xlist.fold l [] (fun chosen (i,t) -> try let _ = match_neg_value vals t in i :: chosen with Not_found -> chosen) in
      if chosen = [] then raise Not_found else
      if Xlist.size chosen = Xlist.size l then Variant(e,l) else
      (*Choice(e,chosen,Variant(e,l))*)failwith "match_neg_value: ni"
  | t -> failwith ("match_neg_value: " ^ ENIAM_LCGstringOf.linear_term 0 t)*)

let check_chosen chosen_map e chosen =
  let chosen = StringSet.of_list chosen in
  StringMap.add_inc chosen_map e chosen (fun set ->
    let set = StringSet.intersection chosen set in
    if StringSet.is_empty set then raise Not_found else
    set)

let rec match_value v2 chosen_map = function
    Val v -> if v = v2 then chosen_map else raise Not_found
  | Variant(e,l) -> (* Przykład kiedy warianty niepasujące do selektora są rozproszone po drzewie np "Wiele wody płynie." *)
      let chosen,chosen_map = Xlist.fold l ([],chosen_map) (fun (chosen,chosen_map) (i,t) ->
        try let chosen_map = match_value v2 chosen_map t in i :: chosen, chosen_map
        with Not_found -> chosen, chosen_map) in
      if chosen = [] then raise Not_found else
      if Xlist.size chosen = Xlist.size l then chosen_map else
      check_chosen chosen_map e chosen
  | t -> failwith ("match_value: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec match_neg_value vals chosen_map = function
    Val v -> if Xlist.mem vals v then raise Not_found else chosen_map
  | Variant(e,l) ->
      let chosen,chosen_map = Xlist.fold l ([],chosen_map) (fun (chosen,chosen_map) (i,t) ->
        try let chosen_map = match_neg_value vals chosen_map t in i :: chosen, chosen_map
        with Not_found -> chosen, chosen_map) in
      if chosen = [] then raise Not_found else
      if Xlist.size chosen = Xlist.size l then chosen_map else
      check_chosen chosen_map e chosen
  | t -> failwith ("match_neg_value: " ^ ENIAM_LCGstringOf.linear_term 0 t)

(*let rec apply_selector v2 = function
    (sel,[]) -> failwith ("apply_selector: " ^ ENIAMcategoriesPL.string_of_selector sel)
  | Negation,("NEGATION",v) :: l -> ("NEGATION",match_value v2 v) :: l
  | Aspect,("ASPECT",v) :: l -> ("ASPECT",match_value v2 v) :: l
  | Mood,("MOOD",v) :: l -> ("MOOD",match_value v2 v) :: l
  | Nsyn,("NSYN",v) :: l -> ("NSYN",match_value v2 v) :: l
  | Nsem,("NSEM",v) :: l -> ("NSEM",match_value v2 v) :: l
  | Case,("CASE",v) :: l -> ("CASE",match_value v2 v) :: l
  | Mode,("MODE",v) :: l -> ("MODE",match_value v2 v) :: l
  | sel,(attr,v) :: l -> (*print_endline ("apply_selector: " ^ ENIAMcategoriesPL.string_of_selector sel ^ " " ^ attr);*) (attr,v) :: (apply_selector v2 (sel,l))

let rec apply_neg_selector vals = function
    (sel,[]) -> failwith ("apply_neg_selector: " ^ ENIAMcategoriesPL.string_of_selector sel)
  | Nsem,("NSEM",v) :: l -> ("NSEM",match_neg_value vals v) :: l
  | Case,("CASE",v) :: l -> ("CASE",match_neg_value vals v) :: l
  | sel,(attr,v) :: l -> (*print_endline ("apply_neg_selector: " ^ ENIAMcategoriesPL.string_of_selector sel ^ " " ^ attr);*) (attr,v) :: (apply_neg_selector vals (sel,l))

let rec apply_selectors attrs = function
    [] -> attrs
  | (sel,Eq,[v]) :: l -> apply_selectors (apply_selector v (sel,attrs)) l
  | (sel,Neq,vals) :: l -> apply_selectors (apply_neg_selector vals (sel,attrs)) l
  | _ -> failwith "apply_selectors"*)

let rec apply_selector v2 chosen_map = function
    (sel,[]) -> failwith ("apply_selector: " ^ ENIAMcategoriesPL.string_of_selector sel)
  | Negation,("NEGATION",v) :: l -> match_value v2 chosen_map v
  | Aspect,("ASPECT",v) :: l -> match_value v2 chosen_map v
  | Mood,("MOOD",v) :: l -> match_value v2 chosen_map v
  | Nsyn,("NSYN",v) :: l -> match_value v2 chosen_map v
  | Nsem,("NSEM",v) :: l -> match_value v2 chosen_map v
  | Case,("CASE",v) :: l -> match_value v2 chosen_map v
  | Mode,("MODE",v) :: l -> match_value v2 chosen_map v
  | Acm,("ACM",v) :: l -> match_value v2 chosen_map v
  | Cat,("CAT",v) :: l -> match_value v2 chosen_map v
  | sel,(attr,v) :: l -> (*print_endline ("apply_selector: " ^ ENIAMcategoriesPL.string_of_selector sel ^ " " ^ attr);*) apply_selector v2 chosen_map (sel,l)

let rec apply_neg_selector vals chosen_map = function
    (sel,[]) -> failwith ("apply_neg_selector: " ^ ENIAMcategoriesPL.string_of_selector sel)
  | Nsem,("NSEM",v) :: l -> match_neg_value vals chosen_map v
  | Case,("CASE",v) :: l -> match_neg_value vals chosen_map v
  | sel,(attr,v) :: l -> (*print_endline ("apply_neg_selector: " ^ ENIAMcategoriesPL.string_of_selector sel ^ " " ^ attr);*) apply_neg_selector vals chosen_map (sel,l)

let rec apply_selectors attrs chosen_map = function
    [] -> chosen_map
  | (sel,Eq,[v]) :: l -> apply_selectors attrs (apply_selector v chosen_map (sel,attrs)) l
  | (sel,Neq,vals) :: l -> apply_selectors attrs (apply_neg_selector vals chosen_map (sel,attrs)) l
  | _ -> failwith "apply_selectors"

let rec select_variant chosen_map = function
    Variant(e,l) ->
      (try
        let chosen = StringMap.find chosen_map e in
        let l = Xlist.fold l [] (fun l (i,t) ->
          if StringSet.mem chosen i then (i,select_variant chosen_map t) :: l else l) in
        match l with
          [] -> failwith "select_variant"
        | [_,t] -> t
        | l -> Variant(e,l)
      with Not_found -> Variant(e,List.rev (Xlist.rev_map l (fun (i,t) -> i,select_variant chosen_map t))))
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l (select_variant chosen_map)))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("select_variant: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let apply_selectors2 attrs sels symbol =
  let chosen_map = apply_selectors attrs StringMap.empty sels in
  List.rev (Xlist.rev_map attrs (fun (s,t) ->
    s, select_variant chosen_map t)),
  select_variant chosen_map symbol

module OrderedStringDir =
  struct
    type t = string * string
    let compare = compare
  end

module StringDirMap = Xmap.Make(OrderedStringDir)

let rec get_arg_symbols_variant arg_symbols = function
    Ref i ->
      let l,dir = arg_symbols.(i) in
      Xlist.map l (fun s -> (s,dir),Ref i)
  | Variant(e,l) ->
      let map = Xlist.fold l StringDirMap.empty (fun map (i,t) ->
        Xlist.fold (get_arg_symbols_variant arg_symbols t) map (fun map (arg_symbol,t) ->
          StringDirMap.add_inc map arg_symbol [i,t] (fun l -> (i,t) :: l))) in
      StringDirMap.fold map [] (fun found arg_symbol l -> (arg_symbol,Variant(e,l)) :: found)
  | t -> failwith ("get_arg_symbols_variant: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec get_arg_symbols_tuple arg_symbols rev = function
    Dot -> rev
  | Tuple l -> Xlist.fold l rev (get_arg_symbols_tuple arg_symbols)
  | t -> (get_arg_symbols_variant arg_symbols t) :: rev

let string_of_argdir = function
    "forward" -> "/"
  | "backward" -> "\\"
  | "both" -> "|"
  | _ -> failwith "string_of_argdir"

let string_of_arg arg =
  String.concat ", " (Xlist.map arg (fun ((arg_symbol,dir),t) -> (string_of_argdir dir) ^ arg_symbol ^ ":" ^ ENIAM_LCGstringOf.linear_term 0 t))

let string_of_position p =
  (string_of_argdir p.dir) ^
  (if p.is_multi then "?" else "") ^
  String.concat "+" (StringSet.to_list p.morfs)

let manage_arg p t =
  let t = SetAttr("gf",Val (ENIAMwalStringOf.gf p.gf),t) in
  let t =
    if p.gf = ENIAMwalTypes.SUBJ || p.gf = ENIAMwalTypes.OBJ || p.gf = ENIAMwalTypes.ARG then
      SetAttr("role",p.role,SetAttr("role-attr",p.role_attr,SetAttr("selprefs",p.selprefs,t)))
    else if p.gf = ENIAMwalTypes.CORE then SetAttr("selprefs",p.selprefs,t)
    else if p.gf = ENIAMwalTypes.ADJUNCT || p.gf = ENIAMwalTypes.NOSEM || p.gf = ENIAMwalTypes.CORE then t
    else failwith "manage_arg: ni 2" in
  let t = Xlist.fold p.cr t (fun t cr -> SetAttr("controller",Val cr,t)) in
  let t = Xlist.fold p.ce t (fun t ce -> SetAttr("controllee",Val ce,t)) in
  let t = if p.gf = ENIAMwalTypes.NOSEM then Dot else t in
  t

let rec match_arg_positions lemma arg rev = function
    p :: positions ->
      (* Printf.printf "match_arg_positions 1: arg=%s rev=[%s] positions=%s :: [%s]\n%!" (string_of_arg arg) (String.concat "; " (Xlist.map rev string_of_position)) (string_of_position p) (String.concat "; " (Xlist.map positions string_of_position)); *)
      let l = Xlist.fold arg [] (fun l ((arg_symbol,dir),t) ->
        if StringSet.mem p.morfs arg_symbol && p.dir = dir then t :: l else l) in
      (match l with
        [] -> (*print_endline "match_arg_positions: not matched";*) match_arg_positions lemma arg (p :: rev) positions
      | [t] ->
          let t = manage_arg p t in
          if p.is_multi then (t, rev @ (p :: positions)) :: (match_arg_positions lemma arg (p :: rev) positions)
          else (t, rev @ positions) :: (match_arg_positions lemma arg (p :: rev) positions)
      | [t1;t2] -> (* FIXME: przydałoby się to uogólnić na listę dowolnej długości *)
          let t1 = manage_arg p t1 in
          let t2 = manage_arg p t2 in
          let t = Variant("",["1",t1;"2",t2]) in
          if p.is_multi then (t, rev @ (p :: positions)) :: (match_arg_positions lemma arg (p :: rev) positions)
          else (t, rev @ positions) :: (match_arg_positions lemma arg (p :: rev) positions)
      | _ -> failwith ("match_arg_positions: lemma=" ^ lemma ^ " arg=" ^ string_of_arg arg ^ " position=" ^ string_of_position p))
  | [] -> (*Printf.printf "match_arg_positions 2: arg=%s rev=[%s] positions=[]\n%!" (string_of_arg arg) (String.concat "; " (Xlist.map rev string_of_position));*) []

(* Jeśli ta funkcja zwróci pustą listę, oznacza to, że argumentów nie dało się dopasować do pozycji *)
let rec match_args_positions_rec lemma prong_attrs positions = function
    arg :: args ->
      (* Printf.printf "match_args_positions_rec: args=%s :: [%s] positions=[%s]\n%!" (string_of_arg arg) (String.concat "; " (Xlist.map args string_of_arg)) (String.concat "; " (Xlist.map positions string_of_position)); *)
      Xlist.fold (match_arg_positions lemma arg [] positions) [] (fun found (arg_pos,positions) ->
        Xlist.fold (match_args_positions_rec lemma prong_attrs positions args) found (fun found l -> (arg_pos :: l) :: found))
  | [] ->
      (* Printf.printf "match_args_positions_rec: args=[] positions=[%s]\n%!" (String.concat "; " (Xlist.map positions string_of_position)); *)
      let b = Xlist.fold positions false (fun b p -> p.is_necessary || b) in
      (* if b then print_endline "match_args_positions: not matched"; *)
      if b then [] else
        [Xlist.fold positions [] (fun found p ->
          if not p.is_pro then found else
          let attrs = if p.is_prong then prong_attrs else [] in
          let cats = p.catprefs(*ENIAM_LCGrules.make_variant (ENIAMwalRenderer.extract_sel_prefs p.sel_prefs)*) in
          let lemma = get_pro_lemma attrs in
          let sem_args = try StringMap.find ENIAMlexSemanticsData.pron_sem_args lemma with Not_found -> failwith "match_args_positions_rec" in
          let attrs = ["sense",Val lemma;"hipero",Tuple[Val "ALL"; Val "0"];"role",p.role;
            "role-attr",p.role_attr; "selprefs",p.selprefs; "gf",Val (ENIAMwalStringOf.gf p.gf);
            "agf",Val ""; "sem-args",make_sem_args sem_args; "rev-hipero",Val "+"] @ attrs in
          let attrs = Xlist.fold p.cr attrs (fun attrs cr -> ("controller",Val cr) :: attrs) in
          let attrs = Xlist.fold p.ce attrs (fun attrs ce -> ("controllee",Val ce) :: attrs) in
          Xlist.fold cats found (fun found cat ->
            let attrs = ["CAT",Val cat;"COERCED",Val cat] @ attrs in
            Node{ENIAM_LCGrenderer.empty_node with lemma=lemma; pos="pro"; attrs=attrs} :: found))]

(* FIXME: opcjonalność podrzędników argumentów zleksykalizowanych *)

(* Jeśli ta funkcja zwróci pustą listę, oznacza to, że argumentów nie dało się dopasować do pozycji *)
let match_args_positions lemma prong_attrs args positions =
  (* Printf.printf "match_args_positions: args=[%s] positions=[%s]\n%!" (String.concat "; " (Xlist.map args string_of_arg)) (String.concat "; " (Xlist.map positions string_of_position)); *)
  Xlist.rev_map (match_args_positions_rec lemma prong_attrs positions args) (function
      [] -> Dot
    | [t] -> t
    | l -> Tuple l)

let translate_selprefs = function
    ENIAMwalTypes.SynsetId _ -> failwith "translate_selprefs"
  | ENIAMwalTypes.Predef _ -> failwith "translate_selprefs"
  | ENIAMwalTypes.SynsetName s -> s
  | ENIAMwalTypes.RelationRole _ -> "ALL"

let string_of_internal_morf = function
    Atom s -> s
  | AVar s -> s
  | Top -> "T"
  | t -> failwith ("string_of_internal_morf: " ^ ENIAM_LCGstringOf.internal_grammar_symbol_prime t)


let string_of_morf = function
    ENIAMwalTypes.LCG Tensor l -> String.concat "*" (Xlist.map l string_of_internal_morf)
  | ENIAMwalTypes.LCG t -> failwith ("string_of_morf: " ^ ENIAM_LCGstringOf.grammar_symbol_prime t)
  | _ -> failwith "string_of_morf"

let rec string_of_arg_symbol = function
    Dot -> ""
  | Val s -> s
  | Tuple l -> String.concat "*" (Xlist.map l string_of_arg_symbol)
  | t -> failwith ("string_of_arg_symbol: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let translate_dir = function
    ENIAMwalTypes.Both_ -> "both"
  | ENIAMwalTypes.Forward_ -> "forward"
  | ENIAMwalTypes.Backward_ -> "backward"

let translate_position id p =
  {role = Val p.ENIAMwalTypes.role;
   role_attr = Val p.ENIAMwalTypes.role_attr;
   selprefs = (match Xlist.map p.ENIAMwalTypes.sel_prefs translate_selprefs with
      [] -> Dot
    | [s] -> Val s
    | l -> Tuple(Xlist.rev_map l (fun s -> Val s)));
   catprefs = p.ENIAMwalTypes.cat_prefs;
   gf=p.ENIAMwalTypes.gf;
   cr=Xlist.map p.ENIAMwalTypes.cr (fun cr -> id ^ "-" ^ cr);
   ce=Xlist.map p.ENIAMwalTypes.ce (fun ce -> id ^ "-" ^ ce);
   is_necessary = p.ENIAMwalTypes.is_necessary = ENIAMwalTypes.Req(*Xlist.fold p.ENIAMwalTypes.morfs true (fun b -> function ENIAMwalTypes.LCG One -> false | _ -> b)*);
   is_pro = p.ENIAMwalTypes.is_necessary = ENIAMwalTypes.Pro || p.ENIAMwalTypes.is_necessary = ENIAMwalTypes.ProNG;
   is_prong = p.ENIAMwalTypes.is_necessary = ENIAMwalTypes.ProNG;
   is_multi = p.ENIAMwalTypes.is_necessary = ENIAMwalTypes.Multi;
   dir= translate_dir p.ENIAMwalTypes.dir;
   morfs =
     if p.ENIAMwalTypes.morfs=[ENIAMwalTypes.LCG One] then StringSet.empty else
     Xlist.fold p.ENIAMwalTypes.morfs StringSet.empty (fun morfs morf ->
       if morf = ENIAMwalTypes.LCG One then ((*Printf.printf "translate_position: One%!\n";*) morfs) else
       StringSet.add morfs (string_of_morf morf))}

let get_phrase_symbol = function
    Tuple[Val "lex";Val "się";Val "qub"] -> "lex-się-qub"
  | Tuple(Val s :: _) -> s
  | Val s -> s
  (* | Dot -> "dot" *)
  | t -> failwith ("get_phrase_symbol: " ^ ENIAM_LCGstringOf.linear_term 0 t)

exception NoFrame of string * string * IntSet.t

let get_prong_attrs attrs =
  Xlist.fold attrs [] (fun attrs -> function
      "NUM",t -> ("NUM",t) :: attrs
    | "GEND",t -> ("GEND",t) :: attrs
    | "PERS",t -> ("PERS",t) :: attrs
    | _ -> attrs)

let rec assign_frames_rec tokens lex_sems tree arg_symbols visited = function
    Ref i ->
      if IntSet.mem visited i then ((*Printf.printf "assign_frames_rec: skipping reference %d\n%!" i;*)Ref i,visited) else (
      (* Printf.printf "assign_frames_rec: entering reference %d\n%!" i; *)
      let t,visited = assign_frames_rec tokens lex_sems tree arg_symbols visited tree.(i) in
      (* Printf.printf "assign_frames_rec: leaving reference %d\n%!" i; *)
      tree.(i) <- t;
      Ref i,IntSet.add visited i)
  | Node t ->
      let args,visited = assign_frames_rec tokens lex_sems tree arg_symbols visited t.args in
      let t = {t with args=args} in
      (* print_endline ("assign_frames_rec 1: " ^ t.lemma); *)
      if t.lemma = "<root>" then Node t,visited else
      let args = get_arg_symbols_tuple arg_symbols [] args in
      let s = ExtArray.get lex_sems t.id in
      let phsymbol = get_phrase_symbol t.symbol in
      (* print_endline ("assign_frames_rec:  phsymbol='" ^ phsymbol ^ "' for node " ^ t.lemma); *)
      let frames = Xlist.fold s.ENIAMlexSemanticsTypes.frames [] (fun frames frame ->
        (* print_endline ("selectors: " ^ ENIAMcategoriesPL.string_of_selectors frame.selectors); *)
        (* Printf.printf "assign_frames_rec 2: lemma=%s positions=[%s]\n%!" t.lemma (ENIAMwalStringOf.schema frame.positions); *)
        try
          let attrs,symbol = apply_selectors2 t.attrs frame.selectors t.symbol in
          let frame = ENIAMsemLexicon.extend_frame phsymbol frame in
          (* print_endline "passed"; *)
          (attrs,symbol,frame,Xlist.rev_map frame.positions (translate_position (string_of_int t.id))) :: frames
        with Not_found ->
          (* print_endline "rejected"; *)
          frames) in
      if frames = [] then failwith ("assign_frames_rec: no frame phsymbol='" ^ phsymbol ^ "' node='" ^ t.lemma ^ "'") else
      let prong_attrs = get_prong_attrs t.attrs in
      let e = ENIAM_LCGreductions.get_variant_label () in
      let l,_ = Xlist.fold frames ([],1) (fun (l,n) (attrs,symbol,frame,positions) ->
        (* Printf.printf "assign_frames_rec 3: lemma=%s args=[%s] positions=[%s]\n%!" t.lemma (String.concat "; " (Xlist.map args string_of_arg)) (String.concat "; " (Xlist.map positions string_of_position)); *)
        if frame.senses = [] then failwith ("assign_frames_rec: no senses '" ^ t.lemma ^ "'") else
        Xlist.fold (match_args_positions t.lemma prong_attrs args positions) (l,n) (fun (l,n) args ->
          Xlist.fold frame.senses (l,n) (fun (l,n) (sense,hipero,weight) ->
            (string_of_int n, Node{t with attrs=
              ("sense",Val sense) ::
              ("hipero",ENIAM_LCGrules.make_variant (Xlist.map hipero (fun (h,n) -> Tuple[Val h;Val(string_of_int n)]))) ::
              ("arole",Val frame.arole) ::
              ("arole-attr",Val frame.arole_attr) ::
              ("arev",Val (if frame.arev then "+" else "-")) ::
              ("agf",Val frame.agf) ::
              ("sem-args",make_sem_args frame.sem_args) ::
              ("rev-hipero",Val (if frame.rev_hipero then "+" else "-")) ::
              ("fopinion",Val (ENIAMwalStringOf.opinion frame.fopinion)) ::
              ("sopinion",Val (ENIAMwalStringOf.opinion frame.sopinion)) :: attrs; args=args; symbol=symbol}) ::
              l,n+1))) in
      if l = [] then (
        (* print_endline ("assign_frames_rec 4: no frame assingment found for " ^ t.lemma ^ " " ^ ENIAM_LCGstringOf.linear_term 0 t.symbol); *)
        raise (NoFrame(t.lemma,ENIAM_LCGstringOf.linear_term 0 t.symbol,visited))) else
      Variant(e,l),visited
  | Variant(e,l) ->
      let a = ref "" in
      let b = ref "" in
      let l,visited = Xlist.fold l ([],visited) (fun (l,visited) (i,t) ->
        try
          let t,visited = assign_frames_rec tokens lex_sems tree arg_symbols visited t in
          (i,t) :: l, visited
        with NoFrame(x,y,visited) -> a:=x; b:=y; l, visited) in
      if l = [] then raise (NoFrame(!a,!b,visited)) else
      Variant(e,List.rev l),visited
  | Tuple l ->
      let l,visited = Xlist.fold l ([],visited) (fun (l,visited) t ->
        let t,visited = assign_frames_rec tokens lex_sems tree arg_symbols visited t in
        t :: l, visited) in
      Tuple(List.rev l),visited
  | Dot -> Dot,visited
  | t -> failwith ("assign_frames_rec: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec get_arg_symbols = function
    Node{arg_symbol=Tuple([Val "cp"; Val "T"; Val "T"]);
         symbol=Tuple([Val "cp"; ctype; comp]); arg_dir=dir} ->
      [string_of_arg_symbol (Tuple([Val "cp"; Val "T"; Val "T"]));
       string_of_arg_symbol (Tuple([Val "cp"; ctype; comp]))],dir
  | Node{arg_symbol=Tuple([Val "ncp"; Val "T"; Val arg_case; Val "T"; Val "T"; Val "T"; Val "T"]);
         symbol=Tuple([Val "ncp"; number; case; gender; person; ctype; comp]); arg_dir=dir} ->
      [string_of_arg_symbol (Tuple([Val "ncp"; Val "T"; Val arg_case; Val "T"; Val "T"; Val "T"; Val "T"]));
       string_of_arg_symbol (Tuple([Val "ncp"; Val "T"; Val arg_case; Val "T"; Val "T"; ctype; comp]))],dir
  | Node{arg_symbol=Tuple([Val "prepncp"; Val arg_prep; Val arg_case; Val "T"; Val "T"]);
         symbol=Tuple([Val "prepncp"; prep; case; ctype; comp]); arg_dir=dir} ->
      [string_of_arg_symbol (Tuple([Val "prepncp"; Val arg_prep; Val arg_case; Val "T"; Val "T"]));
       string_of_arg_symbol (Tuple([Val "prepncp"; prep; case; ctype; comp]))],dir
  | Node t -> [string_of_arg_symbol t.arg_symbol], t.arg_dir
  | t -> failwith ("get_arg_symbols: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let assign_frames tokens lex_sems tree =
  (* print_endline "assign_frames"; *)
  let tree = Array.copy tree in
  let arg_symbols = Array.make (Array.length tree) ([],"") in
  Int.iter 0 (Array.length tree - 1) (fun i ->
    arg_symbols.(i) <- get_arg_symbols tree.(i));
  let _ = assign_frames_rec tokens lex_sems tree arg_symbols IntSet.empty (Ref 0) in
  tree (* FIXME: dodac reshape_tree ??? *)

let rec extract_attr pat rev = function
    [] -> raise Not_found
  | (s,v) :: l ->
      if s = pat then (List.rev rev) @ l, v
      else extract_attr pat ((s,v) :: rev) l

let rec get_attr pat = function
    [] -> raise Not_found
  | (s,v) :: l ->
      if s = pat then v
      else get_attr pat l

let rec cut_nodes result_tree = function
  | Node t ->
      let i = ExtArray.add result_tree (Node t) in
      Ref i
  | Variant(e,l) ->
      let l = Xlist.rev_map l (fun (i,t) -> i, cut_nodes result_tree t) in
      Variant(e,List.rev l)
  | Tuple l ->
      let l = Xlist.rev_map l (cut_nodes result_tree) in
      Tuple(List.rev l)
  | Dot -> Dot
  | t -> failwith ("cut_nodes: " ^ ENIAM_LCGstringOf.linear_term 0 t)

exception AGF

let rec manage_agf = function
  | Node t ->
      (* print_endline ("manage_agf 1 " ^ ENIAM_LCGstringOf.linear_term 0 (Node t)); *)
      let attrs,agf = try extract_attr "agf" [] t.attrs with Not_found -> failwith ("manage_agf: " ^ t.lemma ^ " " ^ string_of_int t.id) in
      (* print_endline "manage_agf 2"; *)
      let gf = try get_attr "gf" t.attrs with Not_found -> Dot in (* FIXME: to by się chyba przydało poprawić, żeby gf było zawsze ustalone *)
      (* print_endline "manage_agf 3"; *)
      if agf = Val "" || agf=gf then Node{t with attrs=attrs} else raise AGF
  | Variant(e,l) ->
      (* print_endline ("manage_agf 4: " ^ ENIAM_LCGstringOf.linear_term 0 (Variant(e,l))); *)
      let l = Xlist.fold l [] (fun l (i,t) -> try (i, manage_agf t) :: l with AGF -> l) in
      (* print_endline ("manage_agf 5: " ^ ENIAM_LCGstringOf.linear_term 0 (Variant(e,l))); *)
      if l = [] then raise AGF else Variant(e,List.rev l)
  | Tuple l ->
      (* print_endline "manage_agf 6"; *)
      let l = Xlist.rev_map l manage_agf in
      (* print_endline "manage_agf 7"; *)
      Tuple(List.rev l)
  | Dot -> Dot
  | t -> failwith ("cut_nodes: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec reduce_set_attr attr v = function
    Node t -> Node{t with attrs=(attr,v) :: t.attrs}
  | Variant(e,l) ->
      Variant(e,List.rev (Xlist.rev_map l (fun (i,t) ->
        i, reduce_set_attr attr v t)))
  | t -> failwith ("reduce_set_attr: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec reduce_tree_rec tokens lex_sems result_tree mid_tree orig_tree = function
    Ref i ->
      if mid_tree.(i) <> Dot then mid_tree.(i) else
      let t = reduce_tree_rec tokens lex_sems result_tree mid_tree orig_tree orig_tree.(i) in
      mid_tree.(i) <- t;
      t
  | Node t ->
      let args = reduce_tree_rec tokens lex_sems result_tree mid_tree orig_tree t.args in
      (* print_endline ("reduce_tree_rec 1: " ^ ENIAM_LCGstringOf.linear_term 0 args); *)
      let args = try manage_agf args with AGF -> failwith "reduce_tree_rec: AGF" in (* FIXME: to nie musi być błąd, należałoby przechwytywać wyjątek na poziorie wariantu powyżej *)
      let args = cut_nodes result_tree args in
      (* print_endline ("reduce_tree_rec 2: " ^ ENIAM_LCGstringOf.linear_term 0 args); *)
      (*let id =
        if t.id = 0 then
          let id = ExtArray.add tokens {ENIAMtokenizerTypes.empty_token_env with ENIAMtokenizerTypes.token=ENIAMtokenizerTypes.Lemma("pro","pro",[[]])} in
          let _ = ExtArray.add lex_sems empty_lex_sem in
          id
        else t.id in*)
      Node{t with args=args; (*id=id*)}
  | Variant(e,l) ->
      let l = Xlist.rev_map l (fun (i,t) -> i, reduce_tree_rec tokens lex_sems result_tree mid_tree orig_tree t) in
      Variant(e,List.rev l)
  | Tuple l ->
      let l = Xlist.rev_map l (reduce_tree_rec tokens lex_sems result_tree mid_tree orig_tree) in
      Tuple(List.rev l)
  | Dot -> Dot
  | SetAttr(attr,v,t) ->
      let t = reduce_tree_rec tokens lex_sems result_tree mid_tree orig_tree t in
      reduce_set_attr attr v t
  | t -> failwith ("reduce_tree_rec: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let reduce_tree tokens lex_sems orig_tree =
  (* print_endline "reduce_tree"; *)
  let mid_tree = Array.make (Array.length orig_tree) Dot in
  let result_tree = ExtArray.make (Array.length orig_tree) Dot in
  let _ = ExtArray.add result_tree Dot in
  let t = reduce_tree_rec tokens lex_sems result_tree mid_tree orig_tree orig_tree.(0) in
  ExtArray.set result_tree 0 t;
  result_tree

let is_subj = function
  | Node t ->
          let gf = try get_attr "gf" t.attrs with Not_found -> failwith "is_subj" in
          gf = Val "subj"
  | t -> failwith ("is_subj: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let is_core = function
  | Node t ->
          let gf = try get_attr "gf" t.attrs with Not_found -> failwith "is_core" in
          gf = Val "core"
  | t -> failwith ("is_core: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let set_subj_coref ce = function
  | Node t ->
          let gf = try get_attr "gf" t.attrs with Not_found -> failwith "set_subj_coref" in
          if gf = Val "subj" then Node{t with attrs=("coref",ce) :: t.attrs} else Node t
  | t -> failwith ("set_subj_coref: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let set_core_selprefs selprefs = function (* FIXME: trzeba usunąć dotychczasowe selprefs. *)
  | Node t ->
          let gf = try get_attr "gf" t.attrs with Not_found -> failwith "set_core_selprefs" in
          if gf = Val "core" then Node{t with attrs=("selprefs",selprefs) :: t.attrs} else Node t
  | t -> failwith ("set_core_selprefs: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec set_subj_coref_args tree ce = function
    Ref i ->
      if is_subj (ExtArray.get tree i) then
        let id = ExtArray.add tree (set_subj_coref ce (ExtArray.get tree i)) in
        Ref id
      else Ref i
  | Variant(e,l) ->
      let l = Xlist.rev_map l (fun (i,t) -> i, set_subj_coref_args tree ce t) in
      Variant(e,List.rev l)
  | Tuple l ->
      let l = Xlist.rev_map l (set_subj_coref_args tree ce) in
      Tuple(List.rev l)
  | Dot -> Dot
  | t -> failwith ("set_subj_coref_args: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec set_selprefs_core tree selprefs = function
    Ref i ->
      if is_core (ExtArray.get tree i) then
        let id = ExtArray.add tree (set_core_selprefs selprefs (ExtArray.get tree i)) in
        Ref id
      else Ref i
  | Variant(e,l) ->
      let l = Xlist.rev_map l (fun (i,t) -> i, set_selprefs_core tree selprefs t) in
      Variant(e,List.rev l)
  | Tuple l ->
      let l = Xlist.rev_map l (set_selprefs_core tree selprefs) in
      Tuple(List.rev l)
  | Dot -> Dot
  | t -> failwith ("set_selprefs_core: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec transfer_attributes_rec tree visited = function
    Ref i ->
      if visited.(i) then Ref i else (
      visited.(i) <- true;
      ExtArray.set tree i (transfer_attributes_rec tree visited (ExtArray.get tree i));
      Ref i)
  | Node t ->
      let t = {t with args = transfer_attributes_rec tree visited t.args} in
      (* print_endline ("transfer_attributes_rec 1: " ^ ENIAM_LCGstringOf.linear_term 0 args); *)
      let t =
        if t.pos = "inf" || t.pos = "pcon" || t.pos = "pant" then
          try
            let attrs,ce = extract_attr "controllee" [] t.attrs in
            let args = set_subj_coref_args tree ce t.args in
            {t with attrs=attrs; args=args}
          with Not_found -> t else
        if t.pos = "prep" && get_attr "gf" t.attrs = Val "arg" then
          let attrs,selprefs = extract_attr "selprefs" [] t.attrs in
          let args = set_selprefs_core tree selprefs t.args in
          {t with attrs=("selprefs", Val "ALL") :: attrs; args=args}
        else t in
      Node t
  | Variant(e,l) ->
      let l = Xlist.rev_map l (fun (i,t) -> i, transfer_attributes_rec tree visited t) in
      Variant(e,List.rev l)
  | Tuple l ->
      let l = Xlist.rev_map l (transfer_attributes_rec tree visited) in
      Tuple(List.rev l)
  | Dot -> Dot
  | t -> failwith ("transfer_attributes_rec: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let transfer_attributes tree =
  let visited = Array.make (ExtArray.size tree) false in
  visited.(0) <- true;
  let t = transfer_attributes_rec tree visited (ExtArray.get tree 0) in
  ExtArray.set tree 0 t;
  ()
