
open Xstd
open Printf

let path = "../resources/NKJP1M/"

let load_freq path filename =
  File.fold_tab (path ^ filename) StringSet.empty (fun set -> function
      [orth;lemma;interp;freq] -> StringSet.add set (String.concat "\t" [orth;lemma;interp;freq])
    | l -> failwith ("load_freq " ^ filename ^ " line: " ^ (String.concat "\t" l)))

let load_tagged_freq path filename =
  File.fold_tab (path ^ filename) StringMap.empty (fun map -> function
      [orth;lemma;interp;freq;compos;sgjp;status;correctness] -> StringMap.add map (String.concat "\t" [orth;lemma;interp;freq]) (compos,sgjp,status,correctness)
    | l -> failwith ("load_tagged_freq " ^ filename ^ " line: " ^ (String.concat "\t" l)))

let print_qmap name qmap =
  printf "%s:\n" name;
  StringQMap.iter qmap (fun k v ->
    printf "%8d %s\n" v k)

let test_tags freq =
  let compos_qmap,sgjp_qmap,status_qmap,correctness_qmap =
    StringMap.fold freq (StringQMap.empty,StringQMap.empty,StringQMap.empty,StringQMap.empty)
      (fun (compos_qmap,sgjp_qmap,status_qmap,correctness_qmap) k (compos,sgjp,status,correctness) ->
        StringQMap.add compos_qmap compos,
        StringQMap.add sgjp_qmap sgjp,
        StringQMap.add status_qmap status,
        StringQMap.add correctness_qmap correctness) in
  print_qmap "compos" compos_qmap;
  print_qmap "sgjp" sgjp_qmap;
  print_qmap "status" status_qmap;
  print_qmap "correctness" correctness_qmap;
  ()

let test_entries freq tagged_freq =
  let freq = StringMap.fold tagged_freq freq (fun freq k _ ->
    if StringSet.mem freq k then StringSet.remove freq k else (
    printf "test_entries only tagged: %s\n" k;
    freq)) in
  StringSet.iter freq (fun k ->
    printf "test_entries only raw: %s\n" k)

let compare_entries tagged_freq validated_freq =
  StringMap.iter tagged_freq (fun k (compos,sgjp,status,correctness) ->
    let compos2,sgjp2,status2,correctness2 = StringMap.find validated_freq k in
    let l =
      (if compos = compos2 then [] else [compos,compos2]) @
      (if sgjp = sgjp2 then [] else [sgjp,sgjp2]) @
      (if status = status2 then [] else [status,status2]) @
      (if correctness = correctness2 then [] else [correctness,correctness2]) in
    if l <> [] then
      printf "%s\t%s\n" k (String.concat "\t" (Xlist.map l (fun (a,b) -> a ^ "-->" ^ b))))

let freq_filename = "NKJP1M-frequency.tab"
let tagged_filename = "NKJP1M-tagged-frequency.tab"
let validated_filename = "NKJP1M-tagged-frequency-AFTER-VALIDATION.tab"

 let _ =
  let freq = load_freq path freq_filename in
  let tagged_freq = load_tagged_freq path tagged_filename in
  (* let validated_freq = load_tagged_freq path validated_filename in *)
  printf "\ntest_tags %s\n" tagged_filename;
  test_tags tagged_freq;
  (* printf "\ntest_tags %s\n" validated_filename;
  test_tags validated_freq; *)
  printf "\ntest_entries %s\n" tagged_filename;
  test_entries freq tagged_freq;
  (* printf "\ntest_entries %s\n" validated_filename;
  test_entries freq validated_freq; *)
  (* printf "\ncompare_entries %s %s\n" tagged_filename validated_filename;
  compare_entries tagged_freq validated_freq; *)
  () 

type compos = COMPOS | COMPOS_STAR | COMPOS_ALT | COMPOS_LWR | COMPOS_LWR_STAR |
     COMPOS_LWR_ALT | COMPOS_LWR_ndm | COMPOS_ndm | NCOMPOS

type sgjp = NON_SGJP | SGJP_BTH_LOWER | SGJP_EXACT | SGJP_LMM_CAPITAL | SGJP_LMM_LOWER |
     SGJP_LMM_UNCAPITAL

type status = ACRO | COMPD | CW | EXT | NCH | NEOL | PN | SPEC | SYMB | WEB

type correctness = CERR | CERR_TAGE | CORR | DIAL | ERR | PHON | PLTAN | TAGD | TAGE

let compos_of_string = function
    "COMPOS" -> COMPOS
  | "COMPOS-*" -> COMPOS_STAR
  | "COMPOS-ALT" -> COMPOS_ALT
  | "COMPOS-LWR" -> COMPOS_LWR
  | "COMPOS-LWR-*" -> COMPOS_LWR_STAR
  | "COMPOS-LWR-ALT" -> COMPOS_LWR_ALT
  | "COMPOS-LWR-ndm" -> COMPOS_LWR_ndm
  | "COMPOS-ndm" -> COMPOS_ndm
  | "NCOMPOS" -> NCOMPOS
  | s -> failwith ("compos_of_string: " ^ s)

let sgjp_of_string = function
    "NON-SGJP" -> NON_SGJP
  | "SGJP-BTH-LOWER" -> SGJP_BTH_LOWER
  | "SGJP-EXACT" -> SGJP_EXACT
  | "SGJP-LMM-CAPITAL" -> SGJP_LMM_CAPITAL
  | "SGJP-LMM-LOWER" -> SGJP_LMM_LOWER
  | "SGJP-LMM-UNCAPITAL" -> SGJP_LMM_UNCAPITAL
  | s -> failwith ("sgjp_of_string: " ^ s)

let status_of_string = function
    "ACRO" -> ACRO
  | "COMPD" -> COMPD
  | "CW" -> CW
  | "EXT" -> EXT
  | "NCH" -> NCH
  | "NEOL" -> NEOL
  | "PN" -> PN
  | "SPEC" -> SPEC
  | "SYMB" -> SYMB
  | "WEB" -> WEB
  | s -> failwith ("status: " ^ s)

let correctness_of_string = function
    "CERR" -> CERR
  | "CERR-TAGE" -> CERR_TAGE
  | "CORR" -> CORR
  | "DIAL" -> DIAL
  | "ERR" -> ERR
  | "PHON" -> PHON
  | "PLTAN" -> PLTAN
  | "TAGD" -> TAGD
  | "TAGE" -> TAGE
  | s -> failwith ("correctness_of_string: " ^ s)

type entry = {orth: string; lemma: string; interp: string; freq: int;
  compos: compos; sgjp: sgjp; status: status; correctness: correctness}

let load_tagged_freq2 path filename =
  File.load_tab (path ^ filename) (function
      [orth;lemma;interp;freq;compos;sgjp;status;correctness] ->
        {orth=orth; lemma=lemma; interp=interp; freq=int_of_string freq;
         compos=compos_of_string compos;
         sgjp=sgjp_of_string sgjp;
         status=status_of_string status;
         correctness=correctness_of_string correctness}
    | l -> failwith ("load_tagged_freq2 " ^ filename ^ " line: " ^ (String.concat "\t" l)))

let remove dict selector =
  Xlist.fold dict [] (fun dict entry -> if selector entry then dict else entry :: dict)

let is_symbolic entry =
  entry.status = COMPD || entry.status = EXT || entry.status = SYMB || entry.status = WEB

let is_incorrect_strict entry =
  entry.correctness = ERR || entry.correctness = PHON || entry.correctness = CERR || entry.correctness = CERR_TAGE

let is_incorrect_loose entry =
  entry.correctness <> CORR

let is_non_sgjp entry =
  entry.sgjp = NON_SGJP

let is_ncompos entry =
  entry.compos = NCOMPOS

let quantity dict =
  Xlist.fold dict 0 (fun n entry -> n + entry.freq)

(*let _ =
  let dict = load_tagged_freq2 path tagged_filename in
  let dict = remove dict is_symbolic in
  let dict_s = remove dict is_incorrect_strict in
  let dict_l = remove dict is_incorrect_loose in
  printf "strict SGJP coverage: %f\n"
    ((float (quantity (remove dict_s is_non_sgjp))) /. (float (quantity dict_s)));
  printf "loose SGJP coverage: %f\n"
    ((float (quantity (remove dict_l is_non_sgjp))) /. (float (quantity dict_l)));
  printf "strict COMPOS coverage: %f\n"
    ((float (quantity (remove dict_s is_ncompos))) /. (float (quantity dict_s)));
  printf "loose COMPOS coverage: %f\n"
    ((float (quantity (remove dict_l is_ncompos))) /. (float (quantity dict_l)));
  ()*)
