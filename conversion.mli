(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

val decoder_path : string ref
val encoder_path : string ref

val aliases : (string, string) Hashtbl.t

val normalize_name : string -> string

val add_alias : string -> string -> unit

val get_real_name : string -> string

val decoders : (string, Cs.decoder_table Weak.t) Hashtbl.t

val encoders : (string, Cs.encoder_table Weak.t) Hashtbl.t

val make_decoder : string -> (string -> int -> Cs.t)

val make_encoder : string -> (Cs.ucs4 -> char list)

val recode_string : in_enc:string -> out_enc:string -> string -> string

val init : decoder_dir:string -> encoder_dir:string -> unit -> unit
