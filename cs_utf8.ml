(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

(*
 * UTF-8
 * 
 * RFC 3629
 *)

open Cs

let decode_utf8 str i =
  match str.[i] with
    | '\000'..'\127' as ch ->
        Result (i+1, Char.code ch)
    | '\192'..'\223' as c1 ->
        if i+1 < String.length str then
          let n1 = Char.code c1 in
          let n2 = Char.code str.[i+1] in
            if (n2 lsr 6 != 0b10) then Invalid
            else Result (i+2, ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f))
        else
          TooFew
    | '\224'..'\239' as c1 ->
        if i+2 < String.length str then
          let n1 = Char.code c1
          and n2 = Char.code str.[i+1]
          and n3 = Char.code str.[i+2] in
            if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then
              Invalid
            else
              let p = 
                ((n1 land 0x0f) lsl 12) lor
                  ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
              in
                if (p >= 0xd800) && (p <= 0xdf00) then Invalid
                else Result (i+3, p)
        else
          TooFew
    | '\240'..'\247' as c1 ->
        if i+3 < String.length str then
          let n1 = Char.code c1
          and n2 = Char.code str.[i+1]
          and n3 = Char.code str.[i+2]
          and n4 = Char.code str.[i+3] in
            if (n2 lsr 6 != 0b10) ||
              (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10) then
                Invalid
            else
              Result (i+4, 
                      (((n1 land 0x07) lsl 18) lor
                         ((n2 land 0x3f) lsl 12) lor
                         ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)))
        else
          TooFew
    | _ ->
        Invalid

let encode_utf8 ucs4 =
  let bytes = 
    if ucs4 < 0x80 then
      [ucs4]
    else if ucs4 <= 0x7ff then
      [(0xc0 lor (ucs4 lsr 6)); (0x80 lor (ucs4 land 0x3f))]
    else if ucs4 <= 0xffff then (
      if (ucs4 >= 0xd800 & ucs4 < 0xe000) then 
        raise Illegal;
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
      raise Illegal
  in
    List.map Char.chr bytes
