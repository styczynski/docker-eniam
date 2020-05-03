open SkladnicaTypes

let i_o_s x =
  try int_of_string x with _ -> failwith x

let sort_tags = function
  Xml.Element(str,tags,children) ->
    Xml.Element(str,List.sort Pervasives.compare tags,children)
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "sort_tags")

let to_ocaml_startnode startnode = match sort_tags startnode with
    Xml.Element("startnode",[("from",from);("to",sto)],[Xml.PCData text]) ->
      {from = i_o_s from; sto = i_o_s sto; text = text}
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_startnode")

let to_ocaml_stats stats = match sort_tags stats with
    Xml.Element("stats",["cputime",cputime;"inferences",inferences;"nodes",nodes;"trees",trees],[]) ->
      {trees = trees;
       nodes = i_o_s nodes;
       inferences = inferences;
       cputime = float_of_string cputime}
  | Xml.Element("stats",["cputime",cputime;"inferences",inferences;"trees",trees],[]) ->
      {trees = trees;
       nodes = 0;
       inferences = inferences;
       cputime = float_of_string cputime}
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_stats")

let to_ocaml_comment = function
    Xml.Element("comment",[],[Xml.PCData text]) -> text
  | Xml.Element("comment",[],[]) -> ""
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_comment")

let to_ocaml_type = function
    "FULL" -> FULL
  | "MORPH" -> MORPH
  | "NOT_SENTENCE" -> NOT_SENTENCE
  | "NO_TREE" -> NO_TREE
  | "TOO_DIFFICULT" -> TOO_DIFFICULT
  | "WRONG_SENTENCE" -> WRONG_SENTENCE
  | text -> failwith ("to_ocaml_type: " ^ text)

let to_ocaml_base_answer base_answer = match sort_tags base_answer with
    Xml.Element("base-answer",["type",stype;"username",username],[comment]) ->
      {tree_type = to_ocaml_type stype; username = username; comment = to_ocaml_comment comment}
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_base_answer")

let to_ocaml_extra_answer extra_answer = match sort_tags extra_answer with
    Xml.Element("extra-answer",["type",stype;"username",username],[comment]) ->
      {tree_type = to_ocaml_type stype; username = username; comment = to_ocaml_comment comment}
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_extra_answer")

let to_ocaml_answer_data = function
    Xml.Element("answer-data",[],[base;extra1;extra2]) ->
      begin
        { base_answer = to_ocaml_base_answer base;
        extra_answers = (to_ocaml_extra_answer extra1, to_ocaml_extra_answer extra2) }
      end
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_answer_data")

let to_ocaml_category = function
    Xml.Element("category",[],[Xml.PCData text]) -> text
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_category")

let to_ocaml_f = function
    Xml.Element("f",["type",stype],[Xml.PCData text]) -> { tag = stype; text = text }
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_f")

let to_ocaml_child child = match sort_tags child with
    Xml.Element("child",["from",from;"head",head;"nid",nid;"to",sto],[]) ->
      {nid = i_o_s nid;
      from = i_o_s from;
      sto = i_o_s sto;
      head = bool_of_string head}
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_child")

let to_ocaml_children children = match sort_tags children with
    Xml.Element("children",["rule",rule],children) ->
      ChoiceUndef (rule, List.fold_right (fun h a -> to_ocaml_child h :: a) children [])
  | Xml.Element("children",["chosen",chosen;"rule",rule],children) ->
      ChoiceDef (rule, bool_of_string chosen, List.fold_right (fun h a -> to_ocaml_child h :: a) children [])
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_children")

let to_ocaml_base = function
    Xml.Element("base",[],[Xml.PCData text]) -> text
  | Xml.Element("base",[],[]) -> ""
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_base")

let to_ocaml_orth = function
    Xml.Element("orth",[],[Xml.PCData text]) -> text
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_orth")

let to_ocaml_nonterminal = function
    Xml.Element("nonterminal",[],category::fs) ->
      begin
        to_ocaml_category category,
        List.fold_right (fun h a -> to_ocaml_f h :: a) fs []
      end
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_nonterminal")

let to_ocaml_terminal terminal = match sort_tags terminal with
    Xml.Element("terminal",["token_id",token_id;"interp_id",interp_id;"disamb",disamb;"nps",nps],orth::base::[f]) ->
      begin
        {token_id = token_id; interp_id = interp_id;
        disamb = bool_of_string disamb; nps = bool_of_string nps},
        to_ocaml_orth orth,
        to_ocaml_base base,
        to_ocaml_f f
      end
  | Xml.Element("terminal",["token_id",token_id;"interp_id",interp_id;"disamb",disamb],orth::base::[f]) ->
      begin
        {token_id = token_id; interp_id = interp_id;
        disamb = bool_of_string disamb; nps = false},
        to_ocaml_orth orth,
        to_ocaml_base base,
        to_ocaml_f f
      end
  | Xml.Element("terminal",[],orth::base::[f]) ->
      begin
        {token_id = ""; interp_id = "";
        disamb = false; nps = false},
        to_ocaml_orth orth,
        to_ocaml_base base,
        to_ocaml_f f
      end
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_terminal")

let to_ocaml_node node = match sort_tags node with
    Xml.Element("node",["chosen",chosen;"from",from;"nid",nid;"subtrees",subtrees;"to",sto],nonterminal::first_child::children) ->
      Nonterminal ( {nid = i_o_s nid;
          from = i_o_s from;
          sto = i_o_s sto;
          subtrees = subtrees;
          chosen = bool_of_string chosen},
        fst (to_ocaml_nonterminal nonterminal),
        snd (to_ocaml_nonterminal nonterminal),
        List.fold_right (fun h a -> to_ocaml_children h :: a) (first_child::children) [])
  | Xml.Element("node",["chosen",chosen;"from",from;"nid",nid;"subtrees",subtrees;"to",sto],[terminal]) ->
    begin
      let a, b, c, d = to_ocaml_terminal terminal in
      Terminal ( {nid = i_o_s nid;
          from = i_o_s from;
          sto = i_o_s sto;
          subtrees = subtrees;
          chosen = bool_of_string chosen},
        a,b,c,d )
    end
  | Xml.Element("node",["from",from;"nid",nid;"subtrees",subtrees;"to",sto],nonterminal::first_child::children) ->
      Nonterminal ( {nid = i_o_s nid;
          from = i_o_s from;
          sto = i_o_s sto;
          subtrees = subtrees;
          chosen = false},
        fst (to_ocaml_nonterminal nonterminal),
        snd (to_ocaml_nonterminal nonterminal),
        List.fold_right (fun h a -> to_ocaml_children h :: a) (first_child::children) [])
  | Xml.Element("node",["from",from;"nid",nid;"subtrees",subtrees;"to",sto],[terminal]) ->
    begin
      let a, b, c, d = to_ocaml_terminal terminal in
      Terminal ( {nid = i_o_s nid;
          from = i_o_s from;
          sto = i_o_s sto;
          subtrees = subtrees;
          chosen = false},
        a,b,c,d )
    end
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_node")

let to_ocaml_forest forest = match sort_tags forest with
    Xml.Element("forest",["grammar_no",grammar_no;"sent_id",sent_id],
      Xml.Element("text",[],[Xml.PCData text]) ::
      startnode :: stats :: (Xml.Element("answer-data",[],_) as answer_data) :: first_node :: nodes) ->
        Forest ( {sent_id = sent_id; grammar_no = grammar_no}, text,
          to_ocaml_startnode startnode,
          to_ocaml_stats stats,
          to_ocaml_answer_data answer_data,
          List.fold_right (fun h a -> to_ocaml_node h :: a) (first_node :: nodes) [])
  | Xml.Element("forest",["grammar_no",grammar_no;"sent_id",sent_id],
    Xml.Element("text",[],[Xml.PCData text]) ::
    startnode :: stats :: first_node :: nodes) ->
      Forest ( {sent_id = sent_id; grammar_no = grammar_no}, text,
        to_ocaml_startnode startnode,
        to_ocaml_stats stats,
        empty_answer_data,
        List.fold_right (fun h a -> to_ocaml_node h :: a) (first_node :: nodes) [])
  | Xml.Element("forest",["grammar_no",grammar_no;"sent_id",sent_id],
      Xml.Element("text",[],[Xml.PCData text]) ::
      stats :: (Xml.Element("answer-data",[],_) as answer_data) :: []) ->
        NoTree ( {sent_id = sent_id; grammar_no = grammar_no}, text,
          to_ocaml_stats stats,
          to_ocaml_answer_data answer_data )
  | Xml.Element("forest",["grammar_no",grammar_no;"sent_id",sent_id],
      Xml.Element("text",[],[Xml.PCData text]) ::
      stats :: []) ->
        NoTree ( {sent_id = sent_id; grammar_no = grammar_no}, text,
          to_ocaml_stats stats,
          empty_answer_data_no_tree )
  | xml -> (print_endline (Xml.to_string_fmt xml); failwith "to_ocaml_forest")
