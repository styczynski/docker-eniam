open PreTypes

let initSize = 16 (* Initial size of token array *)
let fileName = "fold_morph.txt" (* Name of the files to be parsed *)

let rec getSome (x : 'a option) : 'a =
	match x with
	| Some a -> a
	| None -> failwith "Unexpected None\n"

(* Returns a list equal to l without its n first elements *)
let rec drop (l : 'a list) (n : int) : 'a list =
	if n < 0 then failwith "Cannot drop a negative number of elements"
	else if n = 0 then l
	else match l with
		| h::t -> drop t (n - 1)
		| [] -> failwith "drop exceeds the list's length"

(* Returns a list of n first elements of l *)
let take (l : 'a list) (n : int) : 'a list =
	if n < 0 then failwith "Cannot take a negative number of elements"
	else let rec loop n l a =
		if n = 0 then List.rev a
		else match l with
			| h::t -> loop (n - 1) t (h::a)
			| [] -> failwith "take exceeds the list's length" in
	loop n l []

(* Returns a sublist of l starting at start of length len *)
let sublist (l : 'a list) (start : int) (len : int) : 'a list =
	take (drop l start) len

(* Returns a sublist of a unicode string s starting at start of length len *)
let unicodeSub (s : string) (start: int) (len : int) : string =
	let is = Xunicode.unicode_chars_of_utf8_string s in
	let subIs = sublist is start len in
	let bs = List.map Xunicode.string_of_uchar subIs in
	let ss = List.map Bytes.to_string bs in
	String.concat "" ss

(* Determines the indentation of a line s. One level of indentation is marked with a single tab. Tabs are separated by spaces *)
let indent (s : string) : int =
	if s = "" then 0
	else let rec loop (s: string) (pos: int) : int =
		if s.[pos] = '\t' then loop s (pos + 2)
		else pos / 2 in
	loop s 0

(* Skips to the next paragraph i.e. after running skipToParagraph the next line should be the text of a paragraph *)
let rec skipToParagraph (i: in_channel) : unit =
	let s = input_line i in
	if indent s <> 1 then skipToParagraph i

(* Returns a string equal to s without its pre first and post last elements *)
let cutString (s : string) (pre : int) (post : int) : string =
	String.sub s pre (String.length s - pre - post)

(* Reads beg from a line in the following format:
	 	 	<segId>; <beg>; <len>; <form>; <interp>; ;*)
let segBeg (s : string) : int =
	int_of_string (List.nth (Str.split (Str.regexp "; ") s) 1)

(* Reads len from a line in the following format:
	 	 	<segId>; <beg>; <len>; <form>; <interp>; ;*)
let segLen (s : string) : int =
	int_of_string (List.nth (Str.split (Str.regexp "; ") s) 2)

(* Return the raw text of a sentence *)
let rec rawSentenceString (s : sentence) : string option =
	match s with
	| RawSentence rs -> Some rs
	| StructSentence (_, _) -> None
	| DepSentence _ -> None
	| QuotedSentences _ -> None
	| AltSentence asnt ->
		let rec loop asnt =
			match asnt with
			| [] -> None
			| (_, snt)::t ->
				let s = rawSentenceString snt in
				if s = None then loop t
				else s in
		loop asnt

(* Returns the raw text of a paragraph *)
let rec rawParagraphString (p : paragraph) : string option =
	match p with
	| RawParagraph rp -> Some rp
	| StructParagraph _ -> None
	| AltParagraph ap ->
		let rec loop ap =
			match ap with
			| [] -> None
			| (_, p)::t ->
				let s = rawParagraphString p in
				if s = None then loop t
				else s in
		loop ap

(* Return the raw text of a text *)
let rec rawTextString (t : text) : string option =
	match t with
	| RawText rt -> Some rt
	| StructText (_, _) -> None
	| AltText at ->
		let rec loop at =
			match at with
			| [] -> None
			| (_, t)::tail ->
				let s = rawTextString t in
				if s = None then loop tail
				else s in
		loop at

(* Returns a list of all the sentences in a paragraph *)
let rec paragraphSentences (p: paragraph) : sentence list =
	match p with
	| RawParagraph _ -> []
	| StructParagraph prs -> List.map (fun pr -> pr.psentence) prs
	| AltParagraph ap -> List.fold_left (fun ss (_, prg) -> ss@(paragraphSentences prg)) [] ap

(* Returns a list of all the sentences in a text *)
let rec textSentences (t: text) : sentence list =
	match t with
	| RawText _ -> []
	| StructText (ps, _) -> List.fold_left (fun ss p -> ss@(paragraphSentences p)) [] ps
	| AltText at -> List.fold_left (fun ss (_, txt) -> ss@(textSentences txt)) [] at

(* Reads a sentence from channel i assuming text is the raw text of the sentences paragraph *)
let input_sentence (i: in_channel) (text: string) : paragraph_record option =
	let rec loop i l =
		let s = input_line i in
		if indent s = 3 then loop i ((cutString s 5 0)::l)
		else l in
	let s = input_line i in
	if indent s <> 2 then None
	else
	(
		let id = cutString s 3 1 in
		let revSeg = loop i [] in
		let lstBeg = segBeg (List.hd revSeg) in
		let lstLen = segLen (List.hd revSeg) in
		let seg = List.rev revSeg in
		let fstBeg = segBeg (List.hd seg) in
		let len = lstBeg + lstLen - fstBeg in
		let rawSen = RawSentence (unicodeSub text fstBeg len) in
		Some {pid=id; pbeg=fstBeg; plen=len; pnext=(-1); psentence=AltSentence[(Raw, rawSen)]; pfile_prefix=""}
	)

(* Reads a paragraph from channel i *)
let input_paragraph (i: in_channel) : paragraph =
	let text = cutString (input_line i) 0 1 in
	let rec loop i l =
		let senOpt = input_sentence i text in
		match senOpt with
		| Some sen -> loop i (sen::l)
		| None -> List.rev l in
	AltParagraph [Raw, RawParagraph text; Struct, StructParagraph (loop i [])]

(* Reads a text from channel i *)
let input_text (i: in_channel) : text =
	let rec loop i l =
		match
			skipToParagraph i;
			input_paragraph i
		with
		| p -> loop i (p::l)
		| exception End_of_file -> List.rev l in
	let emptyArray = ExtArray.make initSize empty_token in
	let l = loop i [] in
	let t = List.fold_left (fun t p -> t ^ (getSome (rawParagraphString p)) ^ " ") "" l in
	AltText [Raw, RawText t; Struct, StructText (l, emptyArray)]

let rec string_of_sentence (s: paragraph_record) : string =
	getSome (rawSentenceString s.psentence)
	(* Alternative conversion showing the sentence structure *)
	(*let rec tmp ps = match ps with
		| RawSentence rs -> "Raw: " ^ rs ^ " "
		| StructSentence (_, _) -> "Struct "
		| DepSentence _ -> "Dep "
		| QuotedSentences prs -> "Quoted: (" ^ (List.fold_left (fun a pr -> a ^ string_of_sentence pr) "" prs) ^ ") "
		| AltSentence asnt -> List.fold_left (fun a (_, snt) -> a ^ (tmp snt)) "" asnt in
	tmp s.psentence ^ "\n"*)

let rec string_of_paragraph (p: paragraph) : string =
	match p with
	| RawParagraph rp -> rp ^ "\n"
	| StructParagraph sp -> List.fold_left (fun s sen -> s ^ (string_of_sentence sen) ^ "\n") "" sp
	| AltParagraph ap -> List.fold_left (fun s (_, p) -> s ^ (string_of_paragraph p)) "" ap

let rec string_of_text (t: text) : string =
	match t with
	| RawText rt -> rt ^ "\n"
	| StructText (ps, _) -> List.fold_left (fun s p -> s ^ (string_of_paragraph p) ^ "\n") "" ps
	| AltText ap -> List.fold_left (fun s (_, t) -> s ^ (string_of_text t)) "" ap

let equalSentences (s0: sentence) (s1: sentence) : bool =
	let str0 = getSome (rawSentenceString s0) in
	let str1 = getSome (rawSentenceString s1) in
	str0 = str1

(* Prints an error if l1 does not contain all the sentences in l0 *)
let rec subsentences (l0 : sentence list) (l1 : sentence list) : unit =
	match l0 with
	| [] -> ()
	| h0::t0 -> match l1 with
		| [] -> print_string ("MISSING SENTENCE:\n" ^ (getSome (rawSentenceString h0)) ^ "\n")
		| h1::t1 ->
			if equalSentences h0 h1 then subsentences t0 t1
			else subsentences (h0::t0) t1

let compareFile (s: string) : unit =
	let i = open_in s in
	let t0 = input_text i in
	let rt = (getSome (rawTextString t0)) in
	try 
		let t1 = PreProcessing.parse_text (RawText rt) in
		let s0 = textSentences t0 in
		let s1 = textSentences t1 in
		subsentences s0 s1
	with
	| Invalid_argument _ -> ()
	| Failure msg -> print_string (msg ^ "\n");
	close_in i

let rec compareDir (dir: string) : unit =
	print_string (dir ^ "\n");
	let process s =
		let absS = (dir ^ "/" ^ s) in
		if s = fileName then compareFile absS
		else if Sys.is_directory absS then compareDir absS in
	Array.iter process (Sys.readdir dir)

let _ =
	if Array.length Sys.argv < 2 then print_string ("Please provide a directory containing " ^ fileName ^ " files.\n")
	else compareDir Sys.argv.(1)
