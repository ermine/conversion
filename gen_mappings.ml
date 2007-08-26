open Types

let decoder_dir = "./data/decoders"
let encoder_dir = "./data/encoders"

type ftype =
   | TPMAP
   | UPMAP
   | RPMAP

let re_tmap = Pcre.regexp "([0-9a-fA-F]+)(?:[ \t]+([0-9a-fA-F]+))?"

let is_empty_line line =
   line = "" || List.mem line.[0] ['*'; '#'; '\r'; '\n'; '\026'; '\127']

let convert_tpmap file cp size =
   let tin = open_in file in
   let data () =
      try let line = input_line tin in Some line
      with End_of_file -> close_in tin; None
   in
   let get_mapping line =
      try
	 let r = Pcre.exec ~rex:re_tmap line in
	 let enc_s = Pcre.get_substring r 1 in
	 let ucs_s = 
	    try Some (Pcre.get_substring r 2)
	    with Not_found -> None
	 in
	    match ucs_s with
	       | Some v -> Some (int_of_string ("0x" ^enc_s), 
				 int_of_string ("0x" ^ v))
	       | None -> None
      with Not_found ->
	 Printf.printf "* not matched: %s\n" line;
	 None
   in
   let a1 = Array.make 256 None in
   let a2 = Array.make 256 None in
   let rec helper nl =
      match data () with
	 | Some line ->
	      if is_empty_line line then
		 helper nl
	      else (
		 match get_mapping line with
		    | None -> helper nl
		    | Some (enc, ucs) ->
			 if enc > 0x10000 then
			    failwith "Too big enc";
			 let b1 = enc lsr 8 land 0xFF in
			 let b2 = enc land 0xFF in
			    match a1.(b1) with
			       | None ->
				    let a3 = Array.make 256 None in
				       a1.(b1) <- Some nl;
				       a2.(nl) <- Some a3;
				       a3.(b2) <- Some ucs;
				       helper (succ nl)
			       | Some l ->
				    match a2.(l) with
				       | Some a3 ->
					    a3.(b2) <- Some ucs;
					    helper nl
				       | None ->
					    failwith "Broken array"
	      )
	 | None ->
	      nl
   in
   let nl = helper 0 in
   let bin =  
      open_out_bin (Filename.concat decoder_dir ("IBM" ^ cp ^ ".dec")) in
   let mar =
      if nl = 1 then
	 match a2.(0) with
	    | Some a -> SBtoUCS4 a
	    | None ->
		 failwith "Broken sb array"
      else
	 MBtoUCS4 (a1, a2)
   in
      output_value bin mar;
	 flush bin;
      close_out bin


let convert_upmap file cp size =
   let outfile = Filename.concat encoder_dir ("IBM" ^ cp ^ ".enc") in
      Gen_unimap.process_file file outfile size

let specify_type file =
   try
      let cp1, cp2, t, size =
	 Scanf.sscanf (String.uppercase file) "%04X%04X.%5s%x"
	    (fun cp1 cp2 t size -> 
		let ftype = 
		   match t with
		      | "TPMAP" -> TPMAP
		      | "RPMAP" -> RPMAP
		      | "UPMAP" -> UPMAP
			   
		      | "TXMAP"
		      | "RXMAP" 
		      | "UXMAP"
		      | _ ->
			   failwith "Not interesting file"
		in
		   string_of_int cp1, string_of_int cp2, ftype, size)
      in
	 Some (cp1, cp2, t, size)
   with 
      | exn ->
	   None

let process_file file =
   let basename = Filename.basename file in
   let data = specify_type basename in
      match data with
	 | Some (cp1, "13488", ftype, size) -> (
	      Printf.printf "%s\n" cp1;
	      match ftype with
		 | TPMAP ->
		      convert_tpmap file cp1 size
		 | UPMAP ->
		      convert_upmap file cp1 size
		 | _ ->
		      ()
	   )
	 | _ ->
	      ()
			 
let _ =
   let path = Sys.argv.(1) in
   let files = Sys.readdir path in
      Array.iter (fun file ->
		     Printf.printf "Processing %s\n" file;
		     process_file (Filename.concat path file)
		 ) files
		     
