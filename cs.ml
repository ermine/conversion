(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

exception Malformed
exception Illegal
exception UnknownEncoding of string

type ucs4 = int

type t =
  | TooFew
  | Shift of int
  | Invalid
  | Result of int * ucs4

module UCS4 =
struct
  type t = ucs4
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
