(*
 * UTF-16
 * 
 * RFC 2781
 *)

open Cs

let chars_of_bo bo =
   match bo with
      | BE -> ['\254'; '\255']
      | LE -> ['\255'; '\254']

let decode_utf16 bo =
  let bo = ref bo in
    fun str i ->
      if i+1 < String.length str then
        let b1 = Char.code str.[i]
        and b2 = Char.code str.[i+1] in
        let ucs4 =
          match !bo with
            | BE -> (b1 lsl 8) + b2
            | LE -> b1 + (b2 lsl 8)
        in
          match ucs4 with
            | 0xfeff -> bo := BE; Shift (i+2)
            | 0xfffe -> bo := LE; Shift (i+2)
            | _ -> 
                if ucs4 < 0xd800 || ucs4 > 0xdfff then
                  Result (i+2, ucs4)
                else if not (ucs4 >= 0xd800 && ucs4 <= 0xdbff) then
                  Invalid
                else
                  if i+3 < String.length str then
                    let b3 = Char.code str.[i+2]
                    and b4 = Char.code str.[i+3] in
                    let ucs42 =
                      match !bo with
                        | BE -> (b3 lsl 8) + b4
                        | LE -> b3 + (b4 lsl 8)
                    in
                      if ucs42 >= 0xdc00 && ucs42 <= 0xdfff then
                        let upper10 = (ucs4 land 0x3ff) lsl 10
                        and lower10 = ucs42 land 0x3ff in
                          Result (i+4, (0x10000 + upper10 + lower10))
                      else
                        Invalid
                  else
                    TooFew
      else
        TooFew

let char_pair_of_ucs4 bo ucs4 =
  match bo with
    | LE -> (Char.chr (ucs4 land 0xFF), Char.chr ((ucs4 lsr 8) land 0xFF ))
    | BE -> (Char.chr ((ucs4 lsr 8) land 0xFF), Char.chr (ucs4 land 0xFF))

let encode_utf16 bo ucs4 =
  if ucs4 < 0x10000 then
    let (c1, c2) = char_pair_of_ucs4 bo ucs4 in
      [c1; c2]
  else
    let u' = ucs4 - 0x10000 in
    let w1 = 0xd800 + (u' lsr 10)
    and w2 = 0xdc00 + (u' land 0x3ff) in
    let (c1,c2) = char_pair_of_ucs4 bo w1
    and (c3,c4) = char_pair_of_ucs4 bo w2 in
      [c1; c2; c3; c4]

