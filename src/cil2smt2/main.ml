open Batteries
open Basic
open Smt2_cmd

(* initialize global variables *)
let _ = Global.set Cil2smt.debug false
let check_debug () = Global.get_exn Cil2smt.debug
let lb = ref ~-. infinity
let ub = ref infinity
let c_file = ref ""
let info_file = ref None
let check_file filename =
  if Sys.file_exists filename then ()
  else raise (Arg.Bad (filename^": No such file"))
let spec = [("-d", Arg.Unit (fun _ -> Global.set Cil2smt.debug true), "enable debugging");
            ("-l", Arg.Float (fun n -> lb := n), "lower bound");
            ("-u", Arg.Float (fun n -> ub := n), "upper bound");
            ("-i", Arg.String (fun f -> (check_file f;
                                         info_file := Some f;
                                        )), "info file");]
let usage = "Usage: cil2smt.native [<options>] <.c>\n<options> are: "
let out = IO.stdout
let run () =
  let info_entries = match !info_file with
    None -> []
  | Some f ->
    let lines = Enum.map String.trim (File.lines_of f) in
    let entries = List.of_enum (Enum.map Info.of_string lines) in
    begin
      if check_debug () then
        begin
          String.println out ("Info file " ^ f ^ " is read");
          List.print ~first:"" ~last:"\n" ~sep:"\n"
            Info.print out entries;
        end;
      entries
    end in
  let f = Cil2smt.translation !c_file !lb !ub info_entries in
  let vars = Set.to_list (collect_vars_in_formula f) in
  let var_decls =
    List.map
      (fun v ->
       try
         match Map.find v !Cil2smt.vtype_map with
         | Cil2smt.Real -> declare_realvar v
         | Cil2smt.Int -> declare_intvar v
       with Not_found ->
         Printf.printf "%s not found" v;
         declare_realvar v)
      vars
  in
  let logic_cmd = SetLogic QF_NRA in
  Smt2.print out
    (List.flatten
       [[logic_cmd];
        var_decls;
        [Assert f;
         CheckSAT;
         Exit]])

let _ =
  begin
    Arg.parse spec
      (fun x -> check_file x; c_file := x) usage;
    run ()
  end
