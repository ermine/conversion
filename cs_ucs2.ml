(*
  * UCS-2
 *)

open Cs

let decode_ucs2 str i =
  if i+1 < String.length str then
    let b1 = Char.code str.[i]
    and b2 = Char.code str.[i+1] in
    let ucs4 = (b1 lsl 8) + b2 in
      if ucs4 >= 0xd800 && ucs4 < 0xe000 then
        Invalid
      else
        Result (i+2, ucs4)
  else
    TooFew

let encode_ucs2 ucs4 =
  if ucs4 < 0x10000 && ucs4 != 0xfffe && 
	  not (ucs4 >= 0xd800 && ucs4 < 0xe000) then
	    [Char.chr ((ucs4 lsr 8) land 0xFF);
			 Char.chr (ucs4 land 0xFF)]
  else
	  raise Malformed
