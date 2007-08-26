(*
  * UCS-4
 *)

open Fstream
open Cs

let rec decode_ucs4 b1 =
   let f ucs4 =
      if ucs4 <= 0x7fffffff then
	 R (ucs4, decode_ucs4)
      else
	 raise Malformed
   in
      read4 BE f b1

let rec encode_ucs4 =
   fun ucs4 ->
      if ucs4 <= 0x7fffffff then
	 let c1 = ucs4 lsr 24
	 and c2 = (ucs4 lsr 16) land 0xFF
	 and c3 = (ucs4 lsr 8) land 0xFF
	 and c4 = ucs4 land 0xFF in
	    R ([Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4], encode_ucs4)
      else
	 raise Malformed
