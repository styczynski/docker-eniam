type forest_attributes = {sent_id : string; grammar_no : string}

type startnode = {from : int; sto : int; text : string}

type stats = {trees : string; nodes : int; inferences : string; cputime : float}

type tree_type = FULL | MORPH | NOT_SENTENCE | NO_TREE | TOO_DIFFICULT | WRONG_SENTENCE
type answer = {tree_type : tree_type; username : string; comment : string}
let empty_answer = {tree_type = FULL; username = ""; comment = ""}
let empty_answer_nt = {tree_type = NO_TREE; username = ""; comment = ""}
type answer_data = {base_answer : answer ; extra_answers : answer * answer}
let empty_answer_data = {base_answer = empty_answer ; extra_answers =  empty_answer, empty_answer}
let empty_answer_data_no_tree = {base_answer = empty_answer_nt ; extra_answers =  empty_answer_nt, empty_answer_nt}

type node_attributes = {nid : int; from : int; sto : int; subtrees : string; chosen : bool}
type terminal_attributes = {token_id : string; interp_id : string; disamb : bool; nps : bool}
type f = { tag : string; text : string}
type child = {nid : int; from : int; sto : int; head : bool}
type children = ChoiceDef of string * bool * child list
              | ChoiceUndef of string * child list
type node = Nonterminal of node_attributes * string * f list * children list
             | Terminal of node_attributes * terminal_attributes * string * string * f

type forest = Forest of forest_attributes * string * startnode * stats * answer_data * node list
            | NoTree of forest_attributes * string * stats * answer_data

type tree = Node of int * int * (string * f list) list * tree list * tree list
          | Leaf of int * int * terminal_attributes * string * string * f
          | TreeNotFound

module Words =
  struct
    type t = string * string list
    let compare a b = Pervasives.compare a b
  end

module TreeMap = Map.Make(Words)

exception NoForest
