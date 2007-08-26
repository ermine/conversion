(*
 * UTF-8
 * 
 * RFC 3629
 *)

open Fstream
open Cs

let utf8_len = [|        (* Char byte length according to first UTF-8 byte. *)
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
   2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
   3; 3; 3; 3; 3; 3; 4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 
|]

let rec decode_utf8 c0 =
   let b0 = Char.code c0 in
      match utf8_len.(b0) with
	 | 0 ->
	      raise Malformed
	 | 1 ->
	      R (b0, decode_utf8)
	 | 2 ->
	      F (fun c1 ->
		    let b1 = Char.code c1 in
                       if b1 lsr 6 != 0b10 then 
			  raise Malformed 
                       else
			  R (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F), 
			     decode_utf8)
		)
	 | 3 -> 
	      F (fun c1 ->
		    F (fun c2 ->
			  let b1 = Char.code c1
			  and b2 = Char.code c2 in
			     if (b1 lsr 6 != 0b10) || (b2 lsr 6 != 0b10) then 
				raise Malformed;
			     let b = 
				((b0 land 0x0f) lsl 12) lor 
				   ((b1 land 0x3f) lsl 6) lor (b2 land 0x3f)
			     in
				if (b >= 0xd800) && (b <= 0xdf00) then
				   raise Malformed;
				R (b, decode_utf8)
		      )
		)
	 | 4 ->
	      F (fun c1 ->
		    F (fun c2 ->
			  F (fun c3 ->
				let b1 = Char.code c1
				and b2 = Char.code c2
				and b3 = Char.code c3 in
				   if (b1 lsr 6 != 0b10) || 
				      (b2 lsr 6 != 0b10) || 
				      (b3 lsr 6 != 0b10) then
					 raise Malformed;
				   let b =
				      ((b0 land 0x07) lsl 18) lor 
					 ((b1 land 0x3f) lsl 12) 
				      lor ((b2 land 0x3f) lsl 6) lor 
					 (b3 land 0x3f)
				   in
				      R (b, decode_utf8)
			    )
		      )
		)
	 | _ -> assert false   
	   
let rec encode_utf8 ucs4 =
   let bytes = 
      if ucs4 < 0x80 then
	 [ucs4]
      else if ucs4 <= 0x7ff then
	 [(0xc0 lor (ucs4 lsr 6)); (0x80 lor (ucs4 land 0x3f))]
      else if ucs4 <= 0xffff then (
	 if (ucs4 >= 0xd800 & ucs4 < 0xe000) then 
	    raise Malformed;
	 [(0xe0 lor (ucs4 lsr 12));
	  (0x80 lor ((ucs4 lsr 6) land 0x3f));
	  (0x80 lor (ucs4 land 0x3f))
	 ]
      )
      else if ucs4 <= 0x10ffff then
	 [(0xf0 lor (ucs4 lsr 18));
	  (0x80 lor ((ucs4 lsr 12) land 0x3f));
	  (0x80 lor ((ucs4 lsr 6)  land 0x3f));
	  (0x80 lor (ucs4 land 0x3f))]
      else 
	 raise Malformed
   in
      R ((List.map Char.chr bytes), encode_utf8)
