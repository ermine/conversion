(*
 * UTF-16
 * 
 * RFC 2781
 *)

open Fstream
open Cs

let chars_of_bo bo =
   match bo with
      | BE -> ['\254'; '\255']
      | LE -> ['\255'; '\254']

let char_pair_of_ucs4 bo ucs4 = 
   match bo with
      | LE -> (Char.chr (ucs4 land 0xFF), Char.chr ((ucs4 lsr 8) land 0xFF ))
      | BE -> (Char.chr ((ucs4 lsr 8) land 0xFF), Char.chr (ucs4 land 0xFF))

let rec decode_utf16 bo =
   let f1 ucs4 =
      match ucs4 with
	 | 0xfeff -> F (decode_utf16 BE)
	 | 0xfffe -> F (decode_utf16 LE)
	 | _ ->
	      if ucs4 < 0xd800 || 0xdfff < ucs4 then 
		 R (ucs4, decode_utf16 bo)
	      else if ucs4 <= 0xdbff then
		 let f2 ucs42 =
		    if ucs42 < 0xdc00 || ucs42 > 0xdfff then
		       raise Malformed;
		    let upper10 = (ucs4 land 0x3ff) lsl 10
		    and lower10 = ucs42 land 0x3ff in
		       R (0x10000 + upper10 + lower10,
			  decode_utf16 bo)
		 in
		    F (read2 bo f2)
	      else 
		 raise Malformed
   in
      read2 bo f1
	
let rec encode_utf16 bo =
   fun ucs4 ->
      if ucs4 < 0x10000 then
	 let (c1, c2) = char_pair_of_ucs4 bo ucs4 in
	    R ([c1; c2], encode_utf16 bo)
      else
	 let u' = ucs4 - 0x10000 in
	 let w1 = 0xd800 + (u' lsr 10)
	 and w2 = 0xdc00 + (u' land 0x3ff) in
	 let (c1,c2) = char_pair_of_ucs4 bo w1
	 and (c3,c4) = char_pair_of_ucs4 bo w2 in
	    R ([c1; c2; c3; c4], encode_utf16 bo)

let rec decode_utf16be b1 =
   let f1 ucs4 =
      if ucs4 >= 0xd800 && ucs4 < 0xdc00 then
	 let f2 ucs42 =
	    if ucs42 < 0xdc00 || ucs42 > 0xdfff then
	       raise Malformed;
	    let upper10 = (ucs4 land 0x3ff) lsl 10
	    and lower10 = ucs42 land 0x3ff in
	       R (0x10000 + upper10 + lower10, decode_utf16be)
	 in
	    F (read2 BE f2)
      else if ucs4 >= 0xdc00 && ucs4 < 0xe000 then
	 raise Malformed
      else
	 R (ucs4, decode_utf16be)
   in
      read2 BE f1 b1

let rec encode_utf16be =
   fun ucs4 ->
      if not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	 if ucs4 < 0x10000 then
	    let (c1, c2) = char_pair_of_ucs4 BE ucs4 in
	       R ([c1; c2], encode_utf16be)
	 else
	    raise Malformed
      else if ucs4 < 0x110000 then
	 let u' = ucs4 - 0x10000 in
	 let w1 = 0xd800 + (u' lsr 10)
	 and w2 = 0xdc00 + (u' land 0x3ff) in
	 let (c1,c2) = char_pair_of_ucs4 BE w1
	 and (c3,c4) = char_pair_of_ucs4 BE w2 in
	    R ([c1; c2; c3; c4], encode_utf16be)
      else
	 raise Malformed
   
let rec decode_utf16le b1 =
   let f1 ucs4 =
      if ucs4 >= 0xd800 && ucs4 < 0xdc00 then
	 let f2 ucs42 =
	    if ucs42 < 0xdc00 || ucs42 > 0xdfff then
	       raise Malformed;
	    let upper10 = (ucs4 land 0x3ff) lsl 10
	    and lower10 = ucs42 land 0x3ff in
	       R (0x10000 + upper10 + lower10, decode_utf16le)
	 in
	    F (read2 LE f2)
      else if ucs4 >= 0xdc00 && ucs4 < 0xe000 then
	 raise Malformed
      else
	 R (ucs4, decode_utf16le)
   in
      read2 LE f1 b1

let rec encode_utf16le =
   fun ucs4 ->
      if not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	 if ucs4 < 0x10000 then
	    let (c1, c2) = char_pair_of_ucs4 LE ucs4 in
	       R ([c1; c2], encode_utf16le)
	 else
	    raise Malformed
      else if ucs4 < 0x110000 then
	 let u' = ucs4 - 0x10000 in
	 let w1 = 0xd800 + (u' lsr 10)
	 and w2 = 0xdc00 + (u' land 0x3ff) in
	 let (c1,c2) = char_pair_of_ucs4 LE w1
	 and (c3,c4) = char_pair_of_ucs4 LE w2 in
	    R ([c1; c2; c3; c4], encode_utf16le)
      else
	 raise Malformed


let fun_decode_utf16 = decode_utf16 BE

let fun_encode_utf16 =
   fun ucs4 ->
      match encode_utf16 BE ucs4 with
         | F f -> R (chars_of_bo BE, f)
         | R (r, f) -> R (chars_of_bo BE @ r, f)
