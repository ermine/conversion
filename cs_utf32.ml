(*
 * UTF-32, UTF32BE, UTF32LE
 * 
 * http://www.unicode.org/unicode/reports/tr19/
 * http://www.unicode.org/reports/tr19/tr19-9.html
 *)

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

let decode_utf32 bo =
  let bo = ref bo in
    fun str i ->
      if i+3 < String.length str then
        let b1 = Char.code str.[i]
        and b2 = Char.code str.[i+1]
        and b3 = Char.code str.[1+2]
        and b4 = Char.code str.[i+3] in
        let ucs4 =
          match !bo with
            | BE -> (b1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4
            | LE -> b1 + (b2 lsl 8) + (b3 lsl 16) + (b4 lsl 24)
        in
          if ucs4 = 0x0000feff then (
            bo := BE;
            Shift (i+4)
          ) else if Int32.compare (Int32.of_int ucs4) be32 = 0 then (
            bo := LE;
            Shift (i+4)
          ) else if ucs4 < 0x110000 &&
	          not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
              Result (i+4, ucs4)
          else
            Invalid
      else
        TooFew

let encode_utf32 bo ucs4 =
  if ucs4 < 0x110000 && not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	  chars4_of_ucs4 bo ucs4
  else
	  raise Malformed

