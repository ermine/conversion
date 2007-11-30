open Cs

let process_line line =
   let i = String.index line '\t' in
   let enc_s = String.sub line 0 i 
   and ucs_s = String.sub line (i+1) (String.length line - (i+1)) in
      int_of_string enc_s, int_of_string ucs_s

let make_decoder_map infile outfile =
   let tin = open_in infile in
   let data () =
      try Some (input_line tin)
      with End_of_file -> close_in tin; None
   in
   let a1 = Array.make 256 None in
   let a2 = Array.make 256 None in
   let rec aux_convert i =
      match data () with
	 | Some line -> (
	      let enc, ucs = process_line line in
	      let b1 = enc lsr 8 land 0xFF
	      and b2 = enc land 0xFF in
		 match a1.(b1) with
		    | Some l -> (
			 match a2.(l) with
			    | Some a3 ->
				 a3.(b2) <- Some ucs;
				 aux_convert i
			    | None ->
				 failwith "Malformed array"
		      )
		    | None ->
			 a1.(b1) <- Some i;
			 let a3 = Array.make 256 None in
			    a3.(b2) <- Some ucs;
			    a2.(i) <- Some a3;
			    aux_convert (i+1)
	   )
	 | None ->
	      i
   in
   let max_i = aux_convert 0 in
   let mar =
      if max_i = 1 then
	 match a2.(0) with
	    | Some a ->
		 SBtoUCS4 a
	    | None ->
		 failwith "malformed array"
      else
	 MBtoUCS4 (a1, a2)
   in
   let outf = open_out_bin outfile in
      output_value outf mar;
      flush outf;
      close_out outf

let _ =
   let src = Sys.argv.(1)
   and decoder = Sys.argv.(2)
   and _encoder = Sys.argv.(3) in

     (* Printf.printf "Processing file %s\n" src;*)
      make_decoder_map src decoder
