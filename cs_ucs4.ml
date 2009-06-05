(*
  * UCS-4
 *)

open Cs

let decode_ucs4 str i =
  if i+3 < String.length str then
    let b1 = Char.code str.[i]
    and b2 = Char.code str.[i+1]
    and b3 = Char.code str.[1+2]
    and b4 = Char.code str.[i+3] in
    let ucs4 = b1 lsl 24 + b2 lsl 16 + b3 lsl 8 + b4 in
      if ucs4 <= 0x7fffffff then
        Result (i+4, ucs4)
      else
        Invalid
  else
    TooFew
      
let encode_ucs4 ucs4 =
  if ucs4 <= 0x7fffffff then
    let c1 = ucs4 lsr 24
    and c2 = (ucs4 lsr 16) land 0xFF
    and c3 = (ucs4 lsr 8) land 0xFF
    and c4 = ucs4 land 0xFF in
      [Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4]
  else
    raise Illegal

let decode_ucs4be = decode_ucs4

let encode_ucs4be = encode_ucs4
  
let decode_ucs4le str i =
  if i+3 < String.length str then
    let b1 = Char.code str.[i]
    and b2 = Char.code str.[i+1]
    and b3 = Char.code str.[i+2]
    and b4 = Char.code str.[i+3] in
    let ucs4 = b4 lsl 24 + b3 lsl 16 + b2 lsl 8 + b1 in
      if ucs4 <= 0x7fffffff then
        Result (i+4, ucs4)
      else
        Invalid
  else
    TooFew
      
let encode_ucs4le ucs4 =
  if ucs4 <= 0x7fffffff then
    let c4 = ucs4 lsr 24
    and c3 = (ucs4 lsr 16) land 0xFF
    and c2 = (ucs4 lsr 8) land 0xFF
    and c1 = ucs4 land 0xFF in
      [Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4]
  else
    raise Illegal
