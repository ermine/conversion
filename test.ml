let _ =
   let in_enc = Sys.argv.(1)
   and out_enc = Sys.argv.(2) in

   let in_file = open_in Sys.argv.(3) in
   let out_file = open_out Sys.argv.(4) in

   let in_f () =
      try Some (input_char in_file) with End_of_file -> None in
   let out_f chs =
      List.iter (fun c -> output_char out_file c) chs;
      flush out_file
   in
      Conversion.frecoder ~in_enc ~out_enc in_f out_f;
      close_in in_file;
      flush out_file;
      close_out out_file
