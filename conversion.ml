(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Cs

type cfg = {
  decoder_dir : string;
  encoder_dir : string;
}

let aliases = Hashtbl.create 266

(* TODO: IBM -> CP, Windows -> CP, [\d]+ -> CP, etc *)
let normalize_name name =
  String.uppercase name
      
let add_alias name alias =
  Hashtbl.add aliases (normalize_name alias) (normalize_name name)

let get_real_name name =
  let normalized = normalize_name name in
    try Hashtbl.find aliases normalized with Not_found -> normalized
      
let decoders = Hashtbl.create 10
let fdecoders = Hashtbl.create 10
let encoders = Hashtbl.create 10
let fencoders = Hashtbl.create 10

let get_decoder_map cfg name =
  try
    let w = Hashtbl.find decoders name in
	    match Weak.get w 0 with
	      | Some map -> map
	      | None ->
		        Hashtbl.remove decoders name;
		        raise Not_found
  with Not_found ->
    let file = Filename.concat cfg.decoder_dir (name ^ ".dec") in
    let bin = 
	    try open_in_bin file with Sys_error _ -> raise (UnknownEncoding name) in
    let mar = input_value bin in
    let w = Weak.create 1 in
	    Weak.set w 0 (Some mar);
	    Hashtbl.add decoders name w;
	    mar

let get_encoder_map cfg name =
  try
    let w = Hashtbl.find encoders name in
	    match Weak.get w 0 with
	      | Some map -> map
	      | None ->
		        Hashtbl.remove encoders name;
		        raise Not_found
  with Not_found ->
    let file = Filename.concat cfg.encoder_dir (name ^ ".enc") in
    let bin = 
	    try open_in_bin file with Sys_error _ -> raise (UnknownEncoding name) in
    let mar = input_value bin in
    let w = Weak.create 1 in
	    Weak.set w 0 (Some mar);
	    Hashtbl.add encoders name w;
	    mar
        
let make_decoder cfg name =
  let real_name = get_real_name name in
  let decoder =
    try
	    Hashtbl.find fdecoders real_name
    with Not_found ->
	    let map = get_decoder_map cfg real_name in
	      match map with
	        | SBtoUCS4 a ->
		          let decode_sb2ucs4 a str i =
                if i < String.length str then
		              let b = Char.code str.[i] in
			              match a.(b) with
			                | Some ucs4 -> Result (i+1, ucs4)
			                | None -> Invalid
                else
                  TooFew
		          in
		            decode_sb2ucs4 a
	        | MBtoUCS4 (a1, a2) ->
		          let decode_mb2ucs4 (a1, a2) str i =
                if i+2 < String.length str then
			            let b1 = Char.code str.[i]
			            and b2 = Char.code str.[i+1] in
				            if b1 < Array.length a1 then
				              match a1.(b1) with
				                | Some row -> (
					                  match a2.(row) with
					                    | Some a3 ->
						                      if b2 < Array.length a3 then
						                        match a3.(b2) with
							                        | Some ucs4 -> Result (i+2, ucs4)
							                        | None -> Invalid
						                      else
                                    Invalid
					                    | None ->
                                  Invalid
					                )
				                | None -> raise Illegal
				            else
                      Invalid
                else
                  TooFew
              in 
	              decode_mb2ucs4 (a1, a2)
  in
    decoder
      
let make_encoder cfg name =
  let real_name = get_real_name name in
  let encoder =
    try
	    Hashtbl.find fencoders real_name
    with Not_found ->
	    let map = get_encoder_map cfg real_name in
	      match map with
	        | UCS4toSB map ->
		          let encode_ucs42sb map ucs4 =
		            let ch = 
			            try UMap.find ucs4 map
			            with Not_found -> 
                    raise Illegal
		            in
                  [ch]
		          in
		            encode_ucs42sb map
	        | UCS4toMB map ->
		          let encode_ucs42mb map ucs4 =
		            let clist =
			            try UMap.find ucs4 map
			            with Not_found -> raise Illegal
		            in
                  clist
		          in
		            encode_ucs42mb map
  in
    encoder
	    
let recode_string cfg ~in_enc ~out_enc str =
  let decoder = make_decoder cfg in_enc
  and encoder = make_encoder cfg out_enc in
  let buf = Buffer.create (String.length str) in
  let rec fstream i =
    if i < String.length str then
      match decoder str i with
        | Result (j, ucs4) ->
            List.iter (Buffer.add_char buf) (encoder ucs4);
            fstream j
        | Shift j ->
            fstream j
        | Invalid ->
            raise Illegal
        | TooFew ->
            raise Illegal
  in
    fstream 0;
    Buffer.contents buf
      
let init ~decoder_dir ~encoder_dir () =
  let cfg = {
    decoder_dir = decoder_dir;
    encoder_dir = encoder_dir;
  } in
    
    Hashtbl.add fdecoders "UTF-8" Cs_utf8.decode_utf8;
    Hashtbl.add fencoders "UTF-8" Cs_utf8.encode_utf8;
  
    Hashtbl.add fdecoders "UTF-16" (Cs_utf16.decode_utf16 BE);
    Hashtbl.add fencoders "UTF-16" (Cs_utf16.encode_utf16 BE);
  
    Hashtbl.add fdecoders "UTF-16BE" (Cs_utf16.decode_utf16 BE);
    Hashtbl.add fencoders "UTF-16BE" (Cs_utf16.encode_utf16 BE);
  
    Hashtbl.add fdecoders "UTF-16LE" (Cs_utf16.decode_utf16 LE);
    Hashtbl.add fencoders "UTF-16LE" (Cs_utf16.encode_utf16 LE);
  
    Hashtbl.add fdecoders "UTF-32" (Cs_utf32.decode_utf32 BE);
    Hashtbl.add fencoders "UTF-32" (Cs_utf32.encode_utf32 BE);
  
    Hashtbl.add fdecoders "UTF-32BE" (Cs_utf32.decode_utf32 BE);
    Hashtbl.add fencoders "UTF-32BE" (Cs_utf32.encode_utf32 BE);
  
    Hashtbl.add fdecoders "UTF-32LE" (Cs_utf32.decode_utf32 LE);
    Hashtbl.add fencoders "UTF-32LE" (Cs_utf32.encode_utf32 LE);
  
    Hashtbl.add fdecoders "UCS-2" Cs_ucs2.decode_ucs2;
    Hashtbl.add fencoders "UCS-2" Cs_ucs2.encode_ucs2;
  
    Hashtbl.add fdecoders "UCS-4" Cs_ucs4.decode_ucs4;
    Hashtbl.add fencoders "UCS-4" Cs_ucs4.encode_ucs4;
  
    (* defaul aliases *)
    add_alias "ANSI_X3.4" "ANSI_X3.4-1968";
    add_alias "ANSI_X3.4" "iso-ir-6";
    add_alias "ANSI_X3.4" "ANSI_X3.4-1986";
    add_alias "ANSI_X3.4" "ISO_646.irv:1991";
    add_alias "ANSI_X3.4" "ASCII";
    add_alias "ANSI_X3.4" "ISO646-US";
    add_alias "ANSI_X3.4" "US-ASCII";
    add_alias "ANSI_X3.4" "us";
    add_alias "ANSI_X3.4" "csASCII";
    add_alias "ANSI_X3.4" "IBM367";
    add_alias "ANSI_X3.4" "CP367";
    add_alias "ISO-8859-1" "ISO_8859-1:1987";
    add_alias "ISO-8859-1" "iso-ir-100";
    add_alias "ISO-8859-1" "ISO_8859-1";
    add_alias "ISO-8859-1" "latin1";
    add_alias "ISO-8859-1" "l1";
    add_alias "ISO-8859-1" "IBM819";
    add_alias "ISO-8859-1" "csISOLatin1];";
    add_alias "ISO-8859-2" "ISO_8859-2:1987";
    add_alias "ISO-8859-2" "iso-ir-101";
    add_alias "ISO-8859-2" "ISO_8859-2";
    add_alias "ISO-8859-2" "latin2";
    add_alias "ISO-8859-2" "l2";
    add_alias "ISO-8859-2" "IBM912";
    add_alias "ISO-8859-2" "csISOLatin2";
    add_alias "ISO-8859-3" "ISO_8859-3:1988";
    add_alias "ISO-8859-3" "iso-ir-109";
    add_alias "ISO-8859-3" "ISO_8859-3";
    add_alias "ISO-8859-3" "latin3";
    add_alias "ISO-8859-3" "l3";
    add_alias "ISO-8859-3" "IBM913";
    add_alias "ISO-8859-3" "csISOLatin3";
    add_alias "ISO_8859-4" "ISO_8859-4:1988";
    add_alias "ISO_8859-4" "iso-ir-110";
    add_alias "ISO_8859-4" "ISO_8859-4";
    add_alias "ISO_8859-4" "latin4";
    add_alias "ISO_8859-4" "l4";
    add_alias "ISO_8859-4" "IBM914";
    add_alias "ISO_8859-4" "csISOLatin4";
    add_alias "ISO-8859-5" "ISO_8859-5:1988";
    add_alias "ISO-8859-5" "iso-ir-144";
    add_alias "ISO-8859-5" "ISO_8859-5";
    add_alias "ISO-8859-5" "cyrillic";
    add_alias "ISO-8859-5" "IBM915";
    add_alias "ISO-8859-5" "csISOLatinCyrillic";
    add_alias "ISO-8859-6" "ISO_8859-6:1987";
    add_alias "ISO-8859-6" "iso-ir-127";
    add_alias "ISO-8859-6" "ISO_8859-6";
    add_alias "ISO-8859-6" "ECMA-114";
    add_alias "ISO-8859-6" "ASMO-708";
    add_alias "ISO-8859-6" "arabic";
    add_alias "ISO-8859-6" "IBM1089";
    add_alias "ISO-8859-6" "csISOLatinArabic";
    add_alias "ISO-8859-7" "ISO_8859-7:1987";
    add_alias "ISO-8859-7" "iso-ir-126";
    add_alias "ISO-8859-7" "ISO_8859-7";
    add_alias "ISO-8859-7" "ELOT_928";
    add_alias "ISO-8859-7" "ECMA-118";
    add_alias "ISO-8859-7" "greek";
    add_alias "ISO-8859-7" "greek8";
    add_alias "ISO-8859-7" "csISOLatinGreek";
    add_alias "ISO-8859-8" "ISO_8859-8:1988";
    add_alias "ISO-8859-8" "iso-ir-138";
    add_alias "ISO-8859-8" "ISO_8859-8";
    add_alias "ISO-8859-8" "hebrew";
    add_alias "ISO-8859-8" "IBM816";
    add_alias "ISO-8859-8" "csISOLatinHebrew";
    add_alias "ISO-8859-9" "ISO_8859-9:1989";
    add_alias "ISO-8859-9" "iso-ir-148";
    add_alias "ISO-8859-9" "ISO_8859-9";
    add_alias "ISO-8859-9" "latin5";
    add_alias "ISO-8859-9" "l5";
    add_alias "ISO-8859-9" "IBM920";
    add_alias "ISO-8859-9" "csISOLatin5";
    add_alias "ISO-8859-10" "iso-ir-157";
    add_alias "ISO-8859-10" "ISO_8859-10:1992";
    add_alias "ISO-8859-10" "csISOLatin6";
    add_alias "ISO-8859-10" "latin6";
    add_alias "ISO-8859-10" "l6";
    add_alias "ISO_6937-2-add" "iso-ir-142";
    add_alias "ISO_6937-2-add" "csISOTextComm";
    add_alias "IBM10001" "Shift_JIS";
    add_alias "IBM10001" "MS_Kanji";
    add_alias "IBM10001" "csShiftJIS";
    add_alias "ISO-8859-13" "IBM921";
    add_alias "ISO-8859-14" "iso-ir-199";
    add_alias "ISO-8859-14" "ISO_8859-14:1998";
    add_alias "ISO-8859-14" "ISO_8859-14";
    add_alias "ISO-8859-14" "latin8";
    add_alias "ISO-8859-14" "iso-celtic";
    add_alias "ISO-8859-14" "l8";
    add_alias "ISO-8859-15" "ISO_8859-15";
    add_alias "ISO-8859-15" "IBM923";
    add_alias "ISO-8859-15" "ISO-IR-203";
    add_alias "ISO-8859-15" "ISO_8859-15:1998";
    add_alias "ISO-8859-15" "Latin9";
    add_alias "ISO-8859-15" "L9";
    add_alias "ISO-8859-16" "iso-ir-226";
    add_alias "ISO-8859-16" "ISO_8859-16:2001";
    add_alias "ISO-8859-16" "ISO_8859-16";
    add_alias "ISO-8859-16" "latin10";
    add_alias "ISO-8859-16" "l10";
    add_alias "IBM936" "GBK";
    add_alias "IBM936" "MS936";
    add_alias "IBM936" "windows-936";
    add_alias "IBM9487" "GB18030";
    add_alias "IBM850" "850";
    add_alias "IBM850" "csPC850Multilingual";
    add_alias "IBM862" "862";
    add_alias "IBM862" "csPC862LatinHebrew";
    add_alias "MAC-ROMAN" "macintosh";
    add_alias "MAC-ROMAN" "mac";
    add_alias "MAC-ROMAN" "csMacintosh";
    add_alias "MAC-LATIN2" "IBM1282";
    add_alias "MAC-LATIN2" "MAC-CENTRAL-EUROPE";
    add_alias "MAC-ICELAND" "CP10079";
    add_alias "MAC-CROATIAN" "CP10082";
    add_alias "MAC-ROMANIA" "CP10010";
    add_alias "MAC-CYRILLIC" "CP10007";
    add_alias "MAC-CYRILLIC" "IBM1283";
    add_alias "MAC-UKRAINE" "CP10017";
    add_alias "MAC-GREEK" "CP10006";
    add_alias "MAC-GREEK" "IBM1280";
    add_alias "MAC-TURKISH" "CP10081";
    add_alias "MAC-ARABIC" "CP10004";
    add_alias "IBM037" "ebcdic-cp-us";
    add_alias "IBM037" "ebcdic-cp-ca";
    add_alias "IBM037" "ebcdic-cp-wt";
    add_alias "IBM037" "ebcdic-cp-nl";
    add_alias "IBM037" "csIBM037";
    add_alias "IBM038" "EBCDIC-INT";
    add_alias "IBM038" "csIBM038";
    add_alias "IBM273" "csIBM273";
    add_alias "IBM274" "EBCDIC-BE";
    add_alias "IBM274" "csIBM274";
    add_alias "IBM275" "EBCDIC-BR";
    add_alias "IBM275" "csIBM275";
    add_alias "IBM277" "EBCDIC-CP-DK";
    add_alias "IBM277" "EBCDIC-CP-NO";
    add_alias "IBM277" "csIBM277";
    add_alias "IBM278" "ebcdic-cp-fi";
    add_alias "IBM278" "ebcdic-cp-se";
    add_alias "IBM278" "csIBM278";
    add_alias "IBM280" "ebcdic-cp-it";
    add_alias "IBM280" "csIBM280";
    add_alias "IBM281" "EBCDIC-JP-E";
    add_alias "IBM281" "csIBM281";
    add_alias "IBM284" "ebcdic-cp-es";
    add_alias "IBM284" "csIBM284";
    add_alias "IBM285" "ebcdic-cp-gb";
    add_alias "IBM285" "csIBM285";
    add_alias "IBM290" "EBCDIC-JP-kana";
    add_alias "IBM290" "csIBM290";
    add_alias "IBM297" "ebcdic-cp-fr";
    add_alias "IBM297" "csIBM297";
    add_alias "IBM420" "ebcdic-cp-ar1";
    add_alias "IBM420" "csIBM420";
    add_alias "IBM423" "ebcdic-cp-gr";
    add_alias "IBM423" "csIBM423";
    add_alias "IBM424" "ebcdic-cp-he";
    add_alias "IBM424" "csIBM424";
    add_alias "IBM437" "437";
    add_alias "IBM437" "csPC8CodePage437";
    add_alias "IBM500" "ebcdic-cp-be";
    add_alias "IBM500" "ebcdic-cp-ch";
    add_alias "IBM500" "csIBM500";
    add_alias "IBM851" "851";
    add_alias "IBM851" "csIBM851";
    add_alias "IBM852" "852";
    add_alias "IBM852" "csPCp852";
    add_alias "IBM855" "855";
    add_alias "IBM855" "csIBM855";
    add_alias "IBM857" "857";
    add_alias "IBM857" "csIBM857";
    add_alias "IBM860" "cp860";
    add_alias "IBM860" "860";
    add_alias "IBM860" "csIBM860";
    add_alias "IBM861" "861";
    add_alias "IBM861" "cp-is";
    add_alias "IBM861" "csIBM861";
    add_alias "IBM863" "863";
    add_alias "IBM863" "csIBM863";
    add_alias "IBM864" "csIBM864";
    add_alias "IBM865" "865";
    add_alias "IBM865" "csIBM865";
    add_alias "IBM868" "cp-ar";
    add_alias "IBM868" "csIBM868";
    add_alias "IBM869" "869";
    add_alias "IBM869" "cp-gr";
    add_alias "IBM869" "csIBM869";
    add_alias "IBM870" "ebcdic-cp-roece";
    add_alias "IBM870" "ebcdic-cp-yu";
    add_alias "IBM870" "csIBM870";
    add_alias "IBM871" "ebcdic-cp-is";
    add_alias "IBM871" "csIBM871";
    add_alias "IBM880" "EBCDIC-Cyrillic";
    add_alias "IBM880" "csIBM880";
    add_alias "IBM891" "CP891";
    add_alias "IBM891" "csIBM891";
    add_alias "IBM903" "csIBM903";
    add_alias "IBM904" "904";
    add_alias "IBM904" "csIBBM904";
    add_alias "IBM905" "ebcdic-cp-tr";
    add_alias "IBM905" "csIBM905";
    add_alias "IBM918" "ebcdic-cp-ar2";
    add_alias "IBM918" "csIBM918";
    add_alias "IBM1026" "csIBM1026";
    add_alias "KOI8-R" "csKOI8R";
    add_alias "KOI8-R" "IBM878";
    add_alias "IBM866" "866";
    add_alias "IBM866" "csIBM866";
    add_alias "IBM775" "csPC775Baltic";
    add_alias "KOI8-U" "IBM1168";
    add_alias "IBM00858" "CCSID00858";
    add_alias "IBM00858" "PC-Multilingual-850+euro";
    add_alias "IBM00924" "CCSID00924";
    add_alias "IBM00924" "ebcdic-Latin9--euro";
    add_alias "IBM01140" "CCSID01140";
    add_alias "IBM01140" "ebcdic-us-37+euro";
    add_alias "IBM01141" "CCSID01141";
    add_alias "IBM01141" "ebcdic-de-273+euro";
    add_alias "IBM01142" "CCSID01142";
    add_alias "IBM01142" "ebcdic-dk-277+euro";
    add_alias "IBM01142" "ebcdic-no-277+euro";
    add_alias "IBM01143" "CCSID01143";
    add_alias "IBM01143" "01143";
    add_alias "IBM01143" "ebcdic-fi-278+euro";
    add_alias "IBM01143" "ebcdic-se-278+euro";
    add_alias "IBM01144" "CCSID01144";
    add_alias "IBM01144" "ebcdic-it-280+euro";
    add_alias "IBM01145" "CCSID01145";
    add_alias "IBM01145" "ebcdic-es-284+euro";
    add_alias "IBM01146" "CCSID01146";
    add_alias "IBM01146" "ebcdic-gb-285+euro";
    add_alias "IBM01147" "CCSID01147";
    add_alias "IBM01147" "ebcdic-fr-297+euro";
    add_alias "IBM01148" "IBM01148";
    add_alias "IBM01148" "CCSID01148";
    add_alias "IBM01148" "ebcdic-international-500+euro";
    add_alias "IBM01149" "IBM01149";
    add_alias "IBM01149" "CCSID01149";
    add_alias "IBM01149" "ebcdic-is-871+euro";
    add_alias "IBM1047" "IBM1047";
    add_alias "IBM154" "Cyrillic-Asian";
    add_alias "IBM806" "TSCII";
    add_alias "IBM806" "csTSCII";
    add_alias "CP1250" "MS-EE";
    add_alias "CP1250" "windows-1250";
    add_alias "CP1251" "windows-1251";
    add_alias "CP1251" "MS-CYRL";
    add_alias "CP1252" "IBM1252";
    add_alias "CP1252" "windows-1252";
    add_alias "CP1252" "MS-ANSI";
    add_alias "CP1253" "windows-1253";
    add_alias "CP1253" "MS-GREEK";
    add_alias "CP1254" "windows-1254";
    add_alias "CP1254" "MS-TURK";
    add_alias "CP1255" "windows-1255";
    add_alias "CP1255" "MS-HEBR";
    add_alias "CP1256" "IBM5352";
    add_alias "CP1256" "windows-1256";
    add_alias "CP1256" "MS-ARAB";
    add_alias "CP1257" "windows-1257";
    add_alias "CP1257" "WINBALTRIM";
    add_alias "CP1258" "windows-1258";
    add_alias "KOI8-RU" "IBM1167";

    cfg
