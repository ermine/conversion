(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Cs

let aliases = Hashtbl.create 10

(* TODO: IBM -> CP, Windows -> CP, [\d]+ -> CP, etc *)
let normalize_name name =
  String.uppercase name
      
let add_alias name alias =
  Hashtbl.add aliases (normalize_name alias) (normalize_name name)

let get_real_name name =
  let normalized = normalize_name name in
    try Hashtbl.find aliases normalized with Not_found -> normalized
      
let load_aliases file =
  let ini = Ini_config.parse file in
    List.iter (fun (name, aliases) ->
		             List.iter (fun (_, alias) -> add_alias name alias) aliases
		          ) ini

let decoders = Hashtbl.create 10
let fdecoders = Hashtbl.create 10
let encoders = Hashtbl.create 10
let fencoders = Hashtbl.create 10

let get_decoder_map name =
  try
    let w = Hashtbl.find decoders name in
	    match Weak.get w 0 with
	      | Some map -> map
	      | None ->
		        Hashtbl.remove decoders name;
		        raise Not_found
  with Not_found ->
    let file = Filename.concat Cs_config.decoder_dir (name ^ ".dec") in
    let bin = 
	    try open_in_bin file with Sys_error _ -> raise (UnknownEncoding name) in
    let mar = input_value bin in
    let w = Weak.create 1 in
	    Weak.set w 0 (Some mar);
	    Hashtbl.add decoders name w;
	    mar

let get_encoder_map name =
  try
    let w = Hashtbl.find encoders name in
	    match Weak.get w 0 with
	      | Some map -> map
	      | None ->
		        Hashtbl.remove encoders name;
		        raise Not_found
  with Not_found ->
    let file = Filename.concat Cs_config.encoder_dir (name ^ ".enc") in
    let bin = 
	    try open_in_bin file with Sys_error _ -> raise (UnknownEncoding name) in
    let mar = input_value bin in
    let w = Weak.create 1 in
	    Weak.set w 0 (Some mar);
	    Hashtbl.add encoders name w;
	    mar
        
let make_decoder name =
  let real_name = get_real_name name in
  let decoder =
    try
	    Hashtbl.find fdecoders real_name
    with Not_found ->
	    let map = get_decoder_map real_name in
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
      
let make_encoder name =
  let real_name = get_real_name name in
  let encoder =
    try
	    Hashtbl.find fencoders real_name
    with Not_found ->
	    let map = get_encoder_map real_name in
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
	    
let recode_string ~in_enc ~out_enc str =
  let decoder = make_decoder in_enc
  and encoder = make_encoder out_enc in
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
      
let () =
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
  
  load_aliases Cs_config.aliases_ini
