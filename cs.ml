open Fstream

exception Malformed
exception Illegal
exception UnknownEncoding of string

module UCS4 =
struct
   type t = int
   let compare = compare
end

module UMap = Map.Make(UCS4)

type decoder_table =
   | SBtoUCS4 of int option array
   | MBtoUCS4 of int option array * int option array option array

type encoder_table =
   | UCS4toSB of char UMap.t
   | UCS4toMB of char list UMap.t

type bom =
   | BE
   | LE

let read2 bo f =
   fun c1 ->
      F (fun c2 ->
	    let b1 = Char.code c1
	    and b2 = Char.code c2 in
            let ucs4 =
               match bo with
                  | BE -> (b1  lsl 8) + b2
                  | LE -> (b2 lsl 8) + b1
            in
               f ucs4
        )

let read4 bo f =
   fun c1 ->
      F (fun c2 ->
            F (fun c3 ->
                  F (fun c4 ->
			let b1 = Char.code c1
			and b2 = Char.code c2
			and b3 = Char.code c3
			and b4 = Char.code c4 in
                        let ucs4 =
                           match bo with
                              | BE -> (b1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4
                              | LE -> b1 + (b2 lsl 8) + (b3 lsl 16) + (b4 lsl 24
								      )
                        in
                           f ucs4
                    )
              )
        )
