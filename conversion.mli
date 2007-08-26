val aliases : (string, string) Hashtbl.t

val normalize_name : string -> string

val add_alias : string -> string -> unit

val get_real_name : string -> string

val load_aliases : string -> unit

val decoders : (string, Cs.decoder_table Weak.t) Hashtbl.t

val encoders : (string, Cs.encoder_table Weak.t) Hashtbl.t

val make_decoder : string -> char -> (char, int) Fstream.t

val make_encoder : string -> int -> (int, char list) Fstream.t

val frecoder : in_enc:string -> out_enc:string -> 
  (unit -> char option) -> (char list -> unit) -> unit

val recode_string : in_enc:string -> out_enc:string -> string -> string
