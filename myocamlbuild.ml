open Ocamlbuild_plugin
open Myocamlbuild_config

let conversion_decoder_dir = "../../share/conversion/decoders"
let conversion_encoder_dir = "../../share/conversion/encoders"
let conversion_aliases_ini = "../../conversion/aliases.ini"

let codec_maps =
  let srcdir = "data/tables" in
    List.filter (fun f -> not (Pathname.is_directory (srcdir / f)))
      (Array.to_list (Pathname.readdir srcdir))

let _ =  dispatch begin function
  | After_rules ->
      
      rule "generating decoding mapping"
        ~prod: "data/decoders/%.dec"
        ~deps: ["tools/table2map.native";
                "data/tables/%"]
        (fun env _build ->
           let table = env "data/tables/%"
           and dec = env "data/decoders/%.dec" in
             Seq [Cmd (S[A"mkdir"; A"-p"; P"data/decoders"]);
                  Cmd (S[Px"tools/table2map.native";
                         A"decoder";
                         P table;
                         P dec])
                 ]
        );

      rule "generating encoding mapping"
        ~prod:"data/encoders/%.enc"
        ~deps: ["tools/table2map.native";
                "data/tables/%"]
        (fun env _build ->
           let table = env "data/tables/%" 
           and enc = env "data/encoders/%.enc" in
             Seq [Cmd (S[A"mkdir"; A"-p"; P"data/encoders"]);
                  Cmd (S[Px"tools/table2map.native";
                         A"encoder";
                         P table;
                         P enc])
                 ]
        );

      rule "generating decoding mappings"
        ~prod:"decoders"
        ~deps: (List.map (fun f -> "data/decoders" / f -.- "dec") codec_maps)
        (fun _ _ -> Nop);

      rule "generating encoding mappings"
        ~prod:"encoders"
        ~deps: (List.map (fun f -> "data/encoders" / f -.- "enc") codec_maps)
        (fun _ _ -> Nop);

      install_lib "conversion" []

  | _ ->
      ()
end
