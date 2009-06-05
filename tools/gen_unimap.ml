open Cs

let is_empty_line line =
   line = "" || 
   List.mem line.[0] ['#'; '\r'; '\n'; '\026'; '\127']

let is_prefix str1 str2 =
   if String.length str1 >= String.length str2 &&
      String.sub str1 0 (String.length str2) = str2 then
	 true
   else
      false

let re_substr = Pcre.regexp "<[^>]+>[ \t]+\"([^\"]+)\""
let re_subdig = Pcre.regexp "<[^>]+>[ \t]+([0-9]+)"

let re_sb = Pcre.regexp "<U([0-9a-fA-F]+)> +\\\\x([0-9a-fA-F]+)"
let re_mb = Pcre.regexp "<U([0-9a-fA-F]+)> +(?:\\\\x([0-9a-fA-F]+))+"
let re_split = Pcre.regexp "\\\\x"

let get_str line =
   let r = Pcre.exec ~rex:re_substr line in
   let s = Pcre.get_substring r 1 in
      s

let get_digs line =
   let r = Pcre.exec ~rex:re_subdig line in
   let s = Pcre.get_substring r 1 in
      int_of_string s

let split_encs encs =
   let splitted = Pcre.split ~rex:re_split encs in
      List.map (fun x -> Char.chr (int_of_string ("0x" ^ x))) splitted
   

let process_file file outfile size =
   let tin = open_in file in
   let data () =
      try Some (input_line tin)
      with End_of_file -> close_in tin; None
   in
   let rec parse_map_mb umap =
      let save_umap umap =
	 let bin = open_out_bin outfile in
	    output_value bin (UCS4toMB umap);
	    flush bin;
	    close_out bin;
      in
      match data () with
	 | Some line ->
	      if is_empty_line line then
		 parse_map_mb umap
	      else if is_prefix line "END CHARMAP" then
		 save_umap umap
	      else
		 let r =
		    try Some (Pcre.exec ~rex:re_mb line)
		    with Not_found -> None
		 in 
		    (match r with
			| Some z ->
			     let ucs_s = Pcre.get_substring z 1 in
			     let encs_s = Pcre.get_substring z 2 in
			     let ucs = int_of_string ("0x" ^ ucs_s) in
			     let encs = split_encs encs_s in
			     let umap = UMap.add ucs encs umap in
				parse_map_mb umap
			| None ->
			     Printf.printf "* not matched: %s\n" line;
			     parse_map_mb umap
		    )
	 | None ->
	      save_umap umap
   in
   let rec parse_map_sb umap =
      let save_umap umap =
	 let bin = open_out_bin outfile in
	    output_value bin (UCS4toSB umap);
	    flush bin;
	    close_out bin;
      in
	 match data () with
	    | Some line ->
		 if is_empty_line line then
		    parse_map_sb umap
		 else if is_prefix line "END CHARMAP" then
		    save_umap umap
		 else
		    let r =
		       try Some (Pcre.exec ~rex:re_sb line)
		       with Not_found -> None
		    in 
		    (match r with
			| Some z ->
			     let ucs_s = Pcre.get_substring z 1 in
			     let enc_s = Pcre.get_substring z 2 in
			     let ucs = int_of_string ("0x" ^ ucs_s) in
			     let enc = int_of_string ("0x" ^ enc_s) in
			     let umap = UMap.add ucs (Char.chr enc) umap in
				parse_map_sb umap
			| None ->
			     Printf.printf "* not matched: %s\n" line;
			     parse_map_sb umap
		    )
	    | None ->
		 save_umap umap
   in
   let rec parse mb_min mb_max =
      match data () with
	 | None ->
	      Printf.printf "Prematured end of file"
	 | Some line ->
	      if is_empty_line line then
		 parse mb_min mb_max
	      else if is_prefix line "<code_set_name>" then
		 (* let code_set_name = get_str line *)
		 parse mb_min mb_max
	      else if is_prefix line "<mb_cur_min>" then
		 let mb_min = get_digs line in
		    parse mb_min mb_max
	      else if is_prefix line "<mb_cur_max>" then
		 let mb_max = get_digs line in
		    parse mb_min mb_max
	      else if is_prefix line "CHARMAP" then (
		 if mb_max > 1 then
		    parse_map_mb UMap.empty
		 else
		    parse_map_sb UMap.empty
	      )
	      else
		 parse mb_min mb_max
   in
      parse 1 1
