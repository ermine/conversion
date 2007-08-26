(*
  * UCS-2
 *)

open Fstream
open Cs

let rec decode_ucs2 b1 =
   let f ucs4 =
      if ucs4 >= 0xd800 && ucs4 < 0xe000 then
	 raise Malformed
      else
	 R (ucs4, decode_ucs2)
   in
      read2 BE f b1

let rec encode_ucs2 =
   fun ucs4 ->
      if ucs4 < 0x10000 && ucs4 != 0xfffe && 
	 not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	    let chars = [Char.chr ((ucs4 lsr 8) land 0xFF);
			 Char.chr (ucs4 land 0xFF)]
	    in
	       R (chars, encode_ucs2)
      else
	 raise Malformed
