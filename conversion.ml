open Fstream
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
      let file = Filename.concat Config.decoder_dir (name ^ ".dec") in
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
      let file = Filename.concat Config.encoder_dir (name ^ ".enc") in
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
		    let rec decoder_sb2ucs4 a ch =
		       let b = Char.code ch in
		       let ucs4 =
			  match a.(b) with
			     | Some ucs4 -> ucs4
			     | None -> raise Illegal
		       in
			  R (ucs4, decoder_sb2ucs4 a)
		    in
		       decoder_sb2ucs4 a
	       | MBtoUCS4 (a1, a2) ->
		    let rec decoder_mb2ucs4 (a1, a2) ch1 =
		       F (fun ch2 ->
			     let b1 = Char.code ch1 in
			     let b2 = Char.code ch2 in
			     let ucs4 =
				if b1 < Array.length a1 then
				   match a1.(b1) with
				      | Some row -> (
					   match a2.(row) with
					      | Some a3 ->
						   if b2 < Array.length a3 then
						      match a3.(b2) with
							 | Some ucs4 -> ucs4
							 | None -> raise Illegal
						   else
						      raise Illegal
					      | None ->
						   raise Illegal
					)
				      | None -> raise Illegal
				else
				   raise Illegal
			     in
				R (ucs4, decoder_mb2ucs4 (a1, a2))
			 )
		    in 
		       decoder_mb2ucs4 (a1, a2)
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
		    let rec encoder_ucs42sb map ucs4 =
		       let ch = 
			  try UMap.find ucs4 map
			  with Not_found -> 
Printf.printf "Encoding failed: %04x\n" ucs4;
raise Illegal
		       in
			  R ([ch], encoder_ucs42sb map)
		    in
		       encoder_ucs42sb map
	       | UCS4toMB map ->
		    let rec encoder_ucs42mb map ucs4 =
		       let clist =
			  try UMap.find ucs4 map
			  with Not_found -> raise Illegal
		       in
			  R (clist, encoder_ucs42mb map)
		    in
		       encoder_ucs42mb map
   in
      encoder
	 
let frecoder ~in_enc ~out_enc in_f out_f =
   let decoder = make_decoder in_enc
   and encoder = make_encoder out_enc in
   let rec fstream decoder encoder =
      match in_f () with
         | Some char ->
              (match decoder char with
                  | R (r, decoder) -> (
		       match encoder r with
			  | R (r, f2) ->
			       out_f r;
			       fstream decoder encoder
			  | F encoder ->
			       fstream decoder encoder
		    )
				  (*
                       let f2 =List.fold_left 
                          (fun f2 a ->
                              match f2 a with
                                 | R (r, f2) ->
                                      out_f r;
                                      f2
                                 | F f2 ->
                                      f2
                          )
                          encoder r 
                       in
                          fstream f1 f2
				  *)
                  | F f ->
                       fstream f encoder
              )
         | None ->
	      ()
   in
      fstream decoder encoder

let recode_string ~in_enc ~out_enc string =
   let strm = Stream.of_string string in
   let buf = Buffer.create (String.length string) in
   let fdecoder () =
      try Some (Stream.next strm)
      with Stream.Failure -> None
   in
   let fencoder chs =
      List.iter (fun c -> Buffer.add_char buf c) chs
   in
      frecoder ~in_enc ~out_enc fdecoder fencoder;
      Buffer.contents buf

let () =
   Hashtbl.add fdecoders "UTF-8" Cs_utf8.decode_utf8;
   Hashtbl.add fencoders "UTF-8" Cs_utf8.encode_utf8;

   Hashtbl.add fdecoders "UTF-16" Cs_utf16.fun_decode_utf16;
   Hashtbl.add fencoders "UTF-16" Cs_utf16.fun_encode_utf16;

   Hashtbl.add fdecoders "UTF-16BE" Cs_utf16.decode_utf16be;
   Hashtbl.add fencoders "UTF-16BE" Cs_utf16.encode_utf16be;

   Hashtbl.add fdecoders "UTF-16LE" Cs_utf16.decode_utf16le;
   Hashtbl.add fencoders "UTF-16LE" Cs_utf16.encode_utf16le;

   Hashtbl.add fdecoders "UTF-32" Cs_utf32.fun_decode_utf32;
   Hashtbl.add fencoders "UTF-32" Cs_utf32.fun_encode_utf32;

   Hashtbl.add fdecoders "UTF-32BE" Cs_utf32.decode_utf32be;
   Hashtbl.add fencoders "UTF-32BE" Cs_utf32.encode_utf32be;

   Hashtbl.add fdecoders "UTF-32LE" Cs_utf32.decode_utf32le;
   Hashtbl.add fencoders "UTF-32LE" Cs_utf32.encode_utf32le;

   Hashtbl.add fdecoders "UCS-2" Cs_ucs2.decode_ucs2;
   Hashtbl.add fencoders "UCS-2" Cs_ucs2.encode_ucs2;

   Hashtbl.add fdecoders "UCS-4" Cs_ucs4.decode_ucs4;
   Hashtbl.add fencoders "UCS-4" Cs_ucs4.encode_ucs4;
   
   load_aliases Config.aliases_ini
