(*TYPE DEFINITIONS*)
type segm = {
  id_seg: string; (* -; [segm_]1.1-seg; [morph_]1.1-seg; [senses_]1.1-seg *)
  pos: int;
  length: int option;
  orth: string option;
  disamb: string option;
  sense: string option}

type sentence = {
  id_s: string; (* -; [segm_]1.57-s; [morph_]1.57-s; [senses_]1.57-s *)
  segments: segm list}

type abs = {
  id_ab: string; (* [txt_]1.1-ab; -; -; - *)
  contents: string option;
  sentences: sentence list}

type text = {
  id_source: string;
  id_p: string; (* [txt_]1[-div]; [segm_]1[-p]; [morph_]1[-p]; [senses_]1[-p] *)
  abs: abs list}

type 'a fold = string -> source:string list -> channel:string list -> 'a -> ('a -> text -> 'a) -> 'a

val string_of_text: text -> string

val fold_text: 'a fold

val fold_segm: 'a fold

val fold_morph: 'a fold

val fold_sense: 'a fold
