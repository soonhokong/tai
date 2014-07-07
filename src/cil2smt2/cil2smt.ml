(*
Author: Wei Chen      <weichen1@andrew.cmu.edu>
        Soonho Kong   <soonhok@cs.cmu.edu>
        Sicun Gao     <sicung@cs.cmu.edu>
        Edmund Clarke <emc@cs.cmu.edu>

dReal -- Copyright (C) 2013 - 2014, Soonho Kong, Wei Chen, Sicun Gao, and Edmund Clarke

dReal is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

dReal is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with dReal. If not, see <http://www.gnu.org/licenses/>.
*)
open Cil
open Batteries
open Smt2_cmd
open Basic
open Vcmap

type expr =
  | E of Basic.exp
  | F of Basic.formula

(* variable type  *)
type vtype =
  | Int
  | Real

let vtype_map:(string, vtype) Map.t ref = ref Map.empty

let extract_E_exn =
  function E e -> e
         | F f -> failwith "Expression expected, but has formula"

let extract_F_exn =
  function E e -> failwith "Formula expected, but has expression"
         | F f -> f

let debug : bool Global.t = Global.empty "debug"
let check_debug () = Global.get_exn debug
(* let _ = Errormsg.colorFlag := true *)
(* let _ = Errormsg.verboseFlag := true *)

let ignore_func_names = ["main"; "get_low_nbits"]

let is_exp e =
  match e with
  | E _ -> true
  | _ -> false

let extract_exp e =
  match e with
  | E e1 -> e1
  | _ -> failwith "error"

let is_formula e =
  not (is_exp e)

let extract_formula e =
  match e with
  | F f -> f
  | _ -> failwith "error"

let is_gfun e =
  match e with
  | GFun _ -> true
  | _ -> false

let is_array_access e =
  match e with
  | Lval (Var _, Index _) -> true
  | _ -> false

let extract_cases labels =
  List.map
    (fun l ->
       match l with
       | Case (e, _) -> e
       | _ -> failwith "not a case"
    )
    labels

let all f bs =
  List.length (List.filter f bs) != 0

let any f bs =
  not (all f bs)

class removeUnnecessaryCodeVisitor =
  object(self)
    inherit nopCilVisitor as super
    method vstmt stmt  =
      match stmt.skind with
     | Instr ins ->
        DoChildren
     | Block b ->
        DoChildren
     | If _ ->
        DoChildren
     | Switch _ ->
       DoChildren
     | Return _ ->
       ChangeTo Cil.invalidStmt
     | Break _ ->
       DoChildren
     | Goto _
     | Continue _
     | Loop _
     | TryFinally _
     | TryExcept _
     | ComputedGoto _ ->
        ChangeTo Cil.invalidStmt
  end

let (arr_init_map :  ( (string, (int, expr) Map.t) Map.t) ref) = ref Map.empty

let rec translation file_name lb ub info_entries =
  vtype_map := Map.empty;
  let cil_file = Frontc.parse file_name () in
  visitCilFile (new removeUnnecessaryCodeVisitor) cil_file;
  if (Global.get_exn debug) then dumpFile defaultCilPrinter Pervasives.stdout "codegen" cil_file;
  let globals = List.filter is_gfun cil_file.globals in
  let globals' =
    List.filter
      (function GFun (fndec, l) -> not (List.mem fndec.svar.vname ignore_func_names)
              | _ -> failwith "GFun only"
      )
      globals
  in
  let exprs = List.flatten (List.map (translate_function lb ub info_entries) globals') in
  match (all is_formula exprs) with
  | true ->
    Basic.make_and (List.map extract_formula exprs)
  | false -> failwith "not all are formula"

and translate_function lb ub info_entries f: expr list =
  match f with
  | GFun (fd, _) ->
    let f, vc = translate_blocks info_entries [fd.sbody] Vcmap.empty in
    let formals = fd.sformals in
    if List.length formals != 1 then
      failwith "The number of function argument has to be 1."
    else
      let formal = List.at formals 0 in
      let v = formal.vname in
      let bounds_on_formal = make_bounded_constraint (v ^ "0") lb ub in
      begin
        (F bounds_on_formal)::f
      end
  | _ -> failwith "should be function"

and translate_blocks info_entries blocks (vc : Vcmap.t): expr list * Vcmap.t =
  List.fold_left
    (fun accu b ->
       let (bs, vc) = accu in
       let stmts1, vc1 = translate_stmts info_entries b.bstmts vc in
       (bs @ stmts1, vc1)
    )
    ([], vc) blocks

and translate_stmts info_entries stmts (vc : Vcmap.t) : expr list * Vcmap.t =
  List.fold_left
    (fun accu s ->
       let (ss, vc) = accu in
       let (ss1, vc1) = translate_stmtkinds info_entries [s.skind] vc in
       (ss @ ss1, vc1)
    )
    ([], vc) stmts

and mk_var s i = s ^ (string_of_int i)

and gen_copy_formula diff_list =
  Basic.make_and
    (List.map
       (fun (s, a, b) ->
          Basic.Eq ((Basic.Var (mk_var s a)), (Basic.Var (mk_var s b)))
       )
       diff_list)

and translate_stmtkinds info_entries skinds (vc : Vcmap.t) : expr list * Vcmap.t =
  let translate_stmtkind sk vc : expr list * Vcmap.t =
    match sk with
    | Instr ins ->
      translate_instrs info_entries ins vc
    | Return _ -> failwith "not now return"
    | Goto _ -> failwith "not now goto"
    | Break _ -> failwith "not now break"
    | Continue _ -> failwith "not now continue"
    | If (be, e1, e2, _)->
      let be', vc0 = translate_exps [be] vc in
      let e1', vc1 = translate_blocks info_entries [e1] vc0 in
      let e2', vc2 = translate_blocks info_entries [e2] vc0 in
      begin
      match (all is_formula be'), (all is_formula (e1' @ e2')) with
      | true, true ->
         let be = extract_F_exn (List.hd be') in
         let es1 = List.map extract_formula e1' in
         let es2 = List.map extract_formula e2' in

         let vc3 = join vc1 vc2 in
         let diff_conseq = diff vc1 vc3 in
         let diff_alter = diff vc2 vc3 in
         let copy_formula_conseq = gen_copy_formula diff_conseq in
         let copy_formula_alter = gen_copy_formula diff_alter in
         [F (
             Basic.make_and
               [ Basic.Imply (be, Basic.make_and (es1 @ [copy_formula_conseq]));
                 Basic.Imply (Basic.Not be, Basic.make_and (es2 @ [copy_formula_alter]))]
           )], vc3
      | _ -> failwith "should all be formula"
      end
    | Switch (exp, block, stmts, _) ->
      translate_switch info_entries sk vc
    | Loop _ -> failwith "not now loop"
    | Block b ->
       translate_blocks info_entries [b] vc
    | TryFinally _ -> failwith "not now try"
    | TryExcept _ -> failwith "not now try"
    | ComputedGoto _ -> failwith "not now comput goto"
  in
  List.fold_left
    (fun accu b ->
       let (prev, vc) = accu in
       let ss, vc1 = translate_stmtkind b vc in
       (prev @ ss, vc1)
    )
    ([], vc) skinds

and translate_switch info_entries stmt vc : expr list * Vcmap.t =
  match stmt with
  | Switch (e, b, stmts, _) ->
    let exps, vc1 = translate_exps [e] vc in
    let e' = extract_E_exn (List.hd exps) in

    (* for each case statement, generate a formula and get new
       variable counting map *)
    let expr_vc_list =
      List.map
        (fun stmt ->
           let labels : Cil.exp list  = extract_cases stmt.labels in
           let processed_labels =
             List.map
               (fun e ->
                  (* abandon the vc here assume it's constant expression *)
                  let exps', _ = translate_exps [e] vc1 in
                  let e' = extract_E_exn (List.hd exps') in
                  e'
               )
               labels
           in

           (* formula for case labels *)
           let pred =
             Basic.make_or
               (List.map
                  (fun case_exp ->
                     Basic.Eq (case_exp, e')
                  )
                  processed_labels
               )
           in
           let conclude, vc2 = translate_stmtkinds info_entries [stmt.skind] vc1 in
           match all is_formula conclude with
           | true ->
             let conclude' = Basic.make_and (List.map extract_formula conclude) in
             Basic.Imply (pred, conclude'), vc2
           | false -> failwith "no all are forumula"
        )
        stmts
    in
    let vcs = List.map snd expr_vc_list in
    let full_vc = List.fold_left join vc1 vcs in
    let full_formula = Basic.make_and (
        List.map
          (fun expr_vc ->
             let (f, vc) = expr_vc in
             let diff_vc = diff vc full_vc in
             let copy_formua = gen_copy_formula diff_vc in
             Basic.make_and [f; copy_formua]
          )
          expr_vc_list
      )
    in
    [F full_formula], full_vc
  | _ -> failwith "not a switch"

and translate_exps exps (vc : Vcmap.t) : expr list * Vcmap.t =
  let translate_exp (e : Cil.exp) (vc : Vcmap.t) : expr * Vcmap.t =
    match e with
    | Const c -> E (translate_const c), vc
    | Lval lval ->
      let e', vc1 = translate_lval lval vc in
      E e', vc1
    | SizeOf _ -> failwith "not now sizeof"
    | SizeOfE _ -> failwith "not now sizeof"
    | SizeOfStr _ -> failwith "not now sizeof"
    | AlignOf _ -> failwith "not now align"
    | AlignOfE _ -> failwith "not now align"
    | AddrOfLabel _ -> failwith "not now align"
    | UnOp (uop, e, _) ->
      begin
        let es, vc1 = translate_exps [e] vc in
        let e' = List.hd es in
        match uop with
        | Neg ->
          begin
            match e' with
            | E e'' -> E (Basic.Neg e''), vc1
            | _ -> failwith "should be an expression"
          end
        | BNot -> failwith "todo BNot"
        | LNot ->
          begin
            match e' with
            | E e'' -> failwith "should be a formula"
            | F e'' -> F (Basic.Not e''), vc1
          end
      end
    | BinOp (bop, e1, e2, _) ->
      begin
        let es1, vc1 = translate_exps [e1] vc in
        let es2, vc2 = translate_exps [e2] vc in
        let e1' = List.hd es1 in
        let e2' = List.hd es2 in
        let vc3 = join vc1 vc2 in
          match bop with
          | PlusA ->
             begin
              match all is_exp [e1'; e2'] with
              | true ->
                let e1' = extract_E_exn e1' in
                let e2' = extract_E_exn e2' in
                E (Basic.Add [e1'; e2']), vc3
              | false -> failwith "not all expression"
            end
          | PlusPI  -> failwith "not now plus"
          | IndexPI -> failwith "not now index"
          | MinusA ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let e1' = extract_E_exn e1' in
                let e2' = extract_E_exn e2' in
                E (Basic.Sub [e1'; e2']), vc3
              | false -> failwith "not all expression"
            end
          | MinusPI -> failwith "not now minus"
          | MinusPP -> failwith "not now minus"
          | Mult ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let e1' = extract_E_exn e1' in
                let e2' = extract_E_exn e2' in
                E (Basic.Mul [e1'; e2']), vc3
              | false -> failwith "not all expression"
            end
          | Div ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let e1' = extract_E_exn e1' in
                let e2' = extract_E_exn e2' in
                E (Basic.Div (e1', e2')), vc3
              | false -> failwith "not all expression"
            end
          | Mod -> failwith "not now mod"
          | Shiftlt -> failwith "not now shift"
          | Shiftrt -> failwith "not now shift"
          | Lt ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let f1 = extract_E_exn e1' in
                let f2 = extract_E_exn e2' in
                F (Basic.Lt (f1, f2)), vc3
              | _ -> failwith "not all formula"
            end
          | Gt ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let f1 = extract_E_exn e1' in
                let f2 = extract_E_exn e2' in
                F (Basic.Gt (f1, f2)), vc3
              | _ -> failwith "not all formula"
            end
          | Le ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let f1 = extract_E_exn e1' in
                let f2 = extract_E_exn e2' in
                F (Basic.Le (f1, f2)), vc3
              | _ -> failwith "not all formula"
            end
          | Ge ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let f1 = extract_E_exn e1' in
                let f2 = extract_E_exn e2' in
                F (Basic.Ge (f1, f2)), vc3
              | _ -> failwith "not all formula"
            end
          | Eq ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let f1 = extract_E_exn e1' in
                let f2 = extract_E_exn e2' in
                F (Basic.Eq (f1, f2)), vc3
              | _ -> failwith "not all formula"
            end
          | Ne ->
            begin
              match all is_exp [e1'; e2'] with
              | true ->
                let f1 = extract_E_exn e1' in
                let f2 = extract_E_exn e2' in
                F (Basic.Eq (f1, f2)), vc3
              | _ -> failwith "not all formula"
            end
          | BAnd -> failwith ""
          | BXor -> failwith ""
          | BOr -> failwith ""
          | LAnd ->
            begin
              match all is_formula [e1'; e2'] with
              | true ->
                let f1 = extract_F_exn e1' in
                let f2 = extract_F_exn e2' in
                F (Basic.And [f1; f2]), vc3
              | _ -> failwith "not all formula"
            end
          | LOr ->
            begin
              match all is_formula [e1'; e2'] with
              | true ->
                let f1 = extract_F_exn e1' in
                let f2 = extract_F_exn e2' in
                F (Basic.Or [f1; f2]), vc3
              | _ -> failwith "not all formula"
            end
        end
    | CastE (_, e) ->
      begin
        let e, vc1 = translate_exps [e] vc in
        match all is_exp e with
        | true ->
          List.hd e, vc1
        | false -> failwith "not a expression for caste"
      end
    | AddrOf _ -> failwith "not now addrOf"
    | StartOf _ -> failwith "not now startOf"
    | Question _ -> failwith "not now question"
  in
  List.fold_left
    (fun accu b ->
       let (prev, vc) = accu in
       let ss, vc1 = translate_exp b vc in
       (prev @ [ss], vc1)
    )
    ([], vc) exps


and extract_index e =
  match e with
  | Const c ->
    begin
      match c with
      | CInt64 (i, _, _) -> Int64.to_int i
      | _ -> failwith "not support number"
    end
  | _ -> failwith "not support non-const index"

and extract_index_exp e =
  match e with
  | Lval (_, Index (i, _)) -> i
  | _ -> failwith "not a variable"

and extract_index_term e =
  match e with
  | Lval lval ->
    extract_var_name lval
  | _ -> failwith "not a lval"

and print_exp exp =
  let doc = Cil.printExp Cil.defaultCilPrinter () exp in
  Pretty.fprint Pervasives.stdout ~width:20 doc;
  print_newline();

and print_stmt stmt =
  let doc = Cil.printStmt Cil.defaultCilPrinter () stmt in
  Pretty.fprint Pervasives.stdout ~width:20 doc;
  print_newline();

and print_block block =
  let doc = Cil.printBlock Cil.defaultCilPrinter () block in
  Pretty.fprint Pervasives.stdout ~width:20 doc;
  print_newline();

and translate_instrs info_entries ins (vc : Vcmap.t) : expr list * Vcmap.t =
  let translate_inst ins (vc : Vcmap.t) : expr * Vcmap.t =
    match ins with
    | Set (lval, e, _) ->
      begin
        match lval with
        | (Var vi, NoOffset) ->
          (* normal variable*)
          begin
            match is_array_access e with
            | true ->
              let index_exp = extract_index_exp e in
              let exps, vc1 = translate_exps [index_exp] vc in
              let index_exp1 = extract_E_exn (List.hd exps) in
              let s = extract_index_term e in
              let vc2 = update s vc1 in
              let dest, vc3 = translate_lval lval vc2 in
              let imap : (int, expr) BatMap.t = Map.find s !arr_init_map in
              let fmap : (int, formula * formula) BatMap.t =
                Map.mapi
                  (*  array = { (index_i, v_i) }           *)
                  (* (index_exp = 0  ) => (dest = v_0)     *)
                  (* (index_exp = 1  ) => (dest = v_1)     *)
                  (*                  ...                  *)
                  (* (index_exp = n-1) => (dest = v_{n-1}) *)
                  (fun index v ->
                     let e = extract_E_exn v in
                     let open Basic in
                     (Eq (Num (float_of_int index), index_exp1),
                      Eq (dest, e))
                  )
                  imap
              in
              let values : (formula * formula) list = List.of_enum (Map.values fmap) in
              let implications = List.map (fun (f1, f2) -> Imply (f1, f2)) values in
              let index_conds = List.map (fun (f1, f2) -> f1) values in
              F (Basic.make_and ((Basic.make_or index_conds)::implications)), vc3
              (* F (Basic.make_and implications), vc3 *)
            | false ->
              let ty = typeOfLval lval in
              let exps, vc1 = translate_exps [e] vc in
              let e' = List.hd exps in
              let s = extract_var_name lval in
              let vc2 = update s vc1 in
              let lval, vc3 = translate_lval lval vc2 in
              begin
                match (e', ty) with
                | (E e', TInt (IInt, _)) ->
                  (F (Basic.Eq (lval, e')), vc3)
                | (E e', TFloat (FDouble, _)) -> (F (Basic.Eq (lval, e')), vc3)
                | (E e', _) -> failwith "Set: only support an assignment to int or double type."
                | (F _, _) -> failwith "should be an expression"
              end
          end
        | (Var vi, Field _) ->
          (* ignore *)
          Errormsg.error
            "%a: var + field is not supported, yet" d_lval lval;
          failwith "todo"
        | (Var vi, Index (exp, _) ) ->
          (* array assignment *)
          let index = extract_index exp in
          let var = extract_var_name lval in
          let imap =
            begin
              match Map.mem var !arr_init_map with
              | true -> Map.find var !arr_init_map
              | false -> Map.empty
            end
          in
          let exps, vc1 = translate_exps [e] vc in
          let e' = List.hd exps in
          let imap' = Map.add index e' imap in
          arr_init_map := Map.add var imap' !arr_init_map;
          F (Basic.True), vc1
        | _ -> failwith "todo _"
      end;
    | Call (lv_opt, f, arg_list, l) ->
      begin match f with
          Lval f' -> handle_call info_entries lv_opt f' arg_list l vc (* lv_opt = f' (arg_list) *)
        | _ -> failwith "Call has to be in the form, x = f (arg_list)"
      end
    | Asm _ -> failwith "not now asm"
  in
  List.fold_left
    (fun accu b ->
       let (prev, vc) = accu in
       let ss, vc1 = translate_inst b vc in
       (prev @ [ss], vc1)
    )
    ([], vc) ins

and get_type =
  function
  | TInt _ -> Int
  | TFloat _ -> Real
  | _ -> failwith "can not convert to our type"

and track_typ vi c =
  if (Global.get_exn debug) then Printf.printf "add %s%d\n" vi.vname c else ();
  vtype_map := Map.add (vi.vname ^ (string_of_int c)) (get_type vi.vtype) !vtype_map;

and translate_lval l (vc : Vcmap.t) : Basic.exp * Vcmap.t =
  let (lhost, _) = l in
  match lhost with
  | Var vi ->
    let c, vc1 = lookup vi.vname vc in
    track_typ vi c;
    Basic.Var (vi.vname ^ (string_of_int c)), vc1
  | _ -> failwith "not now (translate_lval)"


and extract_var_name l  =
  let (lhost, _) = l in
  match lhost with
  | Var vi ->
    vi.vname
  | _ -> failwith "error var name"

and translate_const (c : Cil.constant) =
  match c with
  | CInt64 (i, _, _) -> Basic.Int (Int64.to_int i)
  | CStr _ -> failwith "not now string"
  | CWStr _ -> failwith "not now CWStr"
  | CChr _ -> failwith "not now char"
  | CReal (f, _, _) -> Basic.Num f
  | CEnum _ -> failwith "not now enum"

and handle_call (info_entries : Info.t list) (lv_opt : lval option) ((flhost, foffset) : lval) (arg_list : Cil.exp list) (loc : Cil.location) (vc : Vcmap.t)
  : expr * Vcmap.t
  = match (flhost, foffset) with
    (Var fvinfo, NoOffset) ->
    let fname = fvinfo.vname in
    begin
      match fname with
        "GET_LOW_NBITS" ->
        begin
          let open Info in
          try
            let info_entry = List.find
                (fun i -> i.funcname = fname &&
                          i.lineno   = loc.line &&
                          i.filename = loc.file)
                info_entries
            in
            (* String.println IO.stdout ("We do handle " ^ fname ^ " function..."); *)
            let args = info_entry.args in
            let info = info_entry.info in
            let ret  = info_entry.ret in
            let y_name = List.at args 0 in (* u *)
            let n = Int.of_string (List.at args 1) in (* 32 *)
            let y_val = Float.of_string (List.at info 0) in (* 52776558133248.5 *)
            let y_sign = (List.at info 1) = "+" in
            let significant_high = Int64.of_string ("0x" ^ (List.at info 2)) in
            let significant_low  = Int64.of_string ("0x" ^ (List.at info 3)) in
            let exponent         = Int.of_string (List.at info 4) in
            let xh0 = if n <= 32 then
                Int64.add
                  (Int64.shift_right_logical significant_low n)
                  (Int64.shift_left significant_high (32 - n))
              else
                Int64.shift_right_logical significant_low (32 - n)
            in
            let tmp1 = Float.pow 2.0 (Float.of_int exponent) in (* 2 ^ e0 *)
            let tmp2 = (Int64.to_float xh0) *.
                       (Float.pow 2.0 (Float.of_int (-52 + n + exponent))) in (* x_h * 2 ^ (-52 + n + e) *)
            let tmp3 = tmp1 +. tmp2 in (* 2^e0 + x_h * 2 ^ (-52 + n + e) *)
            let tmp4 = Float.pow 2.0 (Float.of_int (-52 + exponent)) in (* 2 ^ (-52 + e0) *)
            let xl = match lv_opt with
              | Some (Var xvinfo, NoOffset) ->
                let xname = xvinfo.vname in
                let (subscript, vc') = Vcmap.lookup xname vc in
                (xname ^ (String.of_int subscript))
              | _ -> failwith "x must be Var + NoOffset"
            in
            (* f1 = (tmp3 + xl * tmp4 <= y) *)
            let f1 = Le (Add [Num tmp3; Mul [Var xl; Num tmp4]], Var (y_name ^ "0")) in
            (* f2 = (y <= (tmp3 + (xl + 1) * tmp4 *)
            let f2 = Le (Var (y_name ^ "0"), Add [Num tmp3; Mul [Add [Var xl; Num 1.0]; Num tmp4]]) in
            let f3 = Basic.Le (Num 0.0, Var xl) in
            begin
              (F (make_and [f1; f2; f3]), vc)
            end
          with Not_found ->
            begin
              String.println IO.stderr ("We don't handle " ^ fname ^ " function...");
              (F Basic.True, vc)
            end
        end
      | "assert" ->
        let (fs, vc') = translate_exps arg_list vc in
        let f_0 = List.at fs 0 in
        let f_formula = extract_F_exn f_0 in
        let f_formula_not = Not f_formula in
        (F f_formula_not, vc')
      | _ -> failwith ("handle_call: " ^ fname ^ " not yet.")
    end
  | (Var vinfo, _) -> failwith "handle_call only support (Var _, NoOffset) at this time"
  | (Mem _, _) -> failwith "handle_call only support (Var _, NoOffset) at this time"
