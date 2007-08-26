(*
 * UTF-32, UTF32BE, UTF32LE
 * 
 * http://www.unicode.org/unicode/reports/tr19/
 * http://www.unicode.org/reports/tr19/tr19-9.html
 *)

open Fstream
open Cs

let be32 = Int32.of_string "0xfeff0000"

let chars_of_bo bo =
   match bo with
      | BE -> ['\000'; '\000'; '\254'; '\255']
      | LE -> ['\255'; '\254'; '\000'; '\000']

let chars4_of_ucs4 bo ucs4 =
   match bo with
      | BE ->
	   [Char.chr 0x00;
	    Char.chr ((ucs4 lsr 16) land 0xFF);
	    Char.chr ((ucs4 lsr 8) land 0xFF);
	    Char.chr (ucs4 land 0xFF)]
      | LE -> 
	   [Char.chr (ucs4 land 0xFF);
	    Char.chr ((ucs4 lsr 8) land 0xFF );
	    Char.chr ((ucs4 lsr 16) land 0xFF );
	    Char.chr 0x00]

let rec decode_utf32 bo =
   let f ucs4 =
      if ucs4 = 0x0000feff then
	 F (decode_utf32 BE)
      else if Int32.compare (Int32.of_int ucs4) be32 = 0 then
	 F (decode_utf32 LE)
      else if ucs4 < 0x110000 &&
	 not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	    R (ucs4, decode_utf32 bo)
      else
	 raise Malformed
   in read4 bo f

let rec encode_utf32 bo =
   fun ucs4 ->
      if ucs4 < 0x110000 && not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	 let chars = chars4_of_ucs4 bo ucs4 in
	    R (chars, encode_utf32 bo)
      else
	 raise Malformed

let rec decode_utf32be b1 =
   let f ucs4 =
      if ucs4 < 0x110000 && not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	 R (ucs4, decode_utf32be)
      else
	 raise Malformed
   in
      read4 BE f b1

let rec encode_utf32be =
   fun ucs4 ->
      if ucs4 < 0x110000 && not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	 let chars = chars4_of_ucs4 BE ucs4 in
	    R (chars, encode_utf32be)
      else
	 raise Malformed

let rec decode_utf32le b1 =
   let f ucs4 =
      if ucs4 < 0x110000 && not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	 R (ucs4, decode_utf32le)
      else
	 raise Malformed
   in
      read4 LE f b1

let rec encode_utf32le =
   fun ucs4 ->
      if ucs4 < 0x110000 && not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	 let chars = chars4_of_ucs4 LE ucs4 in
	    R (chars, encode_utf32le)
      else
	 raise Malformed

let fun_decode_utf32 = decode_utf32 BE

let fun_encode_utf32 = 
   fun ucs4 ->
      match encode_utf32 BE ucs4 with
         | F f -> R (chars_of_bo BE, f)
         | R (r, f) -> R( chars_of_bo BE @ r, f)

 
