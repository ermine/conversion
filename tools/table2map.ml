open Cs

let process_line line =
  let i = String.index line '\t' in
  let enc_s = String.sub line 0 i 
  and ucs_s = String.sub line (i+1) (String.length line - (i+1)) in
    int_of_string enc_s, int_of_string ucs_s

let read_file infile =
  let data () =
    try Some (input_line infile)
    with End_of_file -> close_in infile; None
  in
  let rec aux_read acc =
    match data () with
      | Some line ->
          let enc, ucs4 = process_line line in
            aux_read ((enc, ucs4) :: acc)
      | None ->
          acc
  in
  let data = aux_read [] in
    List.rev data
      
let u2s_map data =
  let rec aux_umap xs umap =
    match xs with
      | [] -> umap
      | (enc, ucs4) :: tail ->
          let newumap =
            UMap.add ucs4 (Char.chr enc) umap
          in
            aux_umap tail newumap
  in
    aux_umap data (UMap.empty)

let u2m_map data =
  let rec bytes i =
    if i < 0x100 then
      [Char.chr i]
    else
      Char.chr (i land 0xFFF) :: bytes (i lsr 8) 
  in    
  let rec aux_umap xs umap =
    match xs with
      | [] -> umap
      | (enc, ucs4) :: tail ->
          let newumap = UMap.add ucs4 (bytes enc) umap in
            aux_umap tail newumap
  in
    aux_umap data UMap.empty

let sb2ucs4 data =
  let a = Array.make 256 None in
    List.iter (fun (enc, ucs4) -> a.(enc) <- Some ucs4) data;
    SBtoUCS4 a
      
let mb2ucs4 data =
  let a1 = Array.make 256 None in
  let a2 = Array.make 256 None in
  let rec aux_map j xs =
    match xs with
      | [] ->
          MBtoUCS4 (a1, a2)
      | (enc, ucs4) :: tail ->
	        let b1 = enc lsr 8 land 0xFF
	        and b2 = enc land 0xFF in
            if enc > 0xFFFF then
              failwith "enc is too big";
		        match a1.(b1) with
		          | Some l -> (
			            match a2.(l) with
			              | Some a3 ->
				                a3.(b2) <- Some ucs4;
				                aux_map j tail
			              | None ->
				                failwith "Malformed array"
		            )
		          | None ->
			            a1.(b1) <- Some j;
			            let a3 = Array.make 256 None in
			              a3.(b2) <- Some ucs4;
			              a2.(j) <- Some a3;
			              aux_map (succ j) tail
  in
    aux_map 0 data

let make_coder_maps infile decfile encfile =
  let tin = open_in infile in
  let data = read_file tin in
  let decmap, encmap =
    if List.length data <= 256 then
		  sb2ucs4 data, UCS4toSB (u2s_map data)
    else
      mb2ucs4 data, UCS4toMB (u2m_map data)
  in
  let outf = open_out_bin decfile in
  let () =
    output_value outf decmap;
    flush outf;
    close_out outf
  in
  let outf = open_out_bin encfile in
  let () =
    output_value outf encmap;
    flush outf;
    close_out outf
  in
    ()

let make_decoder_map src dst =
  let tin = open_in src in
  let data = read_file tin in
  let decmap =
    if List.length data <= 256 then
      sb2ucs4 data
    else
      mb2ucs4 data
  in
  let outf = open_out_bin dst in
  let () =
    output_value outf decmap;
    flush outf;
    close_out outf
  in
    ()

let make_encoder_map src dst =
  let tin = open_in src in
  let data = read_file tin in
  let encmap =
    if List.length data <= 256 then
		  UCS4toSB (u2s_map data)
    else
      UCS4toMB (u2m_map data)
  in
  let outf = open_out_bin dst in
  let () =
    output_value outf encmap;
    flush outf;
    close_out outf
  in
    ()
  
let _ =
  let opt = Sys.argv.(1)
  and src = Sys.argv.(2)
  and dst = Sys.argv.(3) in
    match opt with
      | "decoder" ->
          make_decoder_map src dst
      | "encoder" ->
          make_encoder_map src dst
      | _ ->
          failwith "Unknown option"
      
