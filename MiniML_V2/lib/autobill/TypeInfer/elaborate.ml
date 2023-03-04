open Constraint
open Constructors
open Types
open Vars
open Ast
open Prelude
open FullAst


module type Prelude = sig
  val it : prelude
end

module Make (Prelude : Prelude) = struct

  module Params = Constraints_params.Params(Prelude)
  include Constraint.Make (Params)

  let elab_typ : uvar -> typ elaboration = fun u typ ->
    let v, fvs = of_rank1_typ ~sort:(get_sort u) typ in
    exists fvs (eq u v) >>> fun env -> env.u v

  let of_eqns eqns =
    let of_eqn = function
      | FirstOrder.FullFOL.Eq (a,b,sort) ->
        let a,fvsa = of_rank1_typ ~sort a in
        let b,fvsb = of_rank1_typ ~sort b in
        Eq (a, b, sort), fvsa @ fvsb
      | Rel (rel, args) ->
        let aux = List.map2 (fun sort t -> of_rank1_typ ~sort t) (sort_of_rel rel) args in
        let args, fvss = List.split aux in
        let fvs = List.concat fvss in
        Rel (rel, args), fvs in
    List.fold_left_map
      (fun fvs eqn -> let eqn, fvs' = of_eqn eqn in (fvs@fvs',eqn))
      []
      eqns

  let rec elab_cmd : uvar -> command elaboration = fun u cmd ->
    let Command {pol; valu; stk; mid_typ; loc} = cmd in
    let v = fresh_u (Base pol) in
    let cvalu, gvalu = elab_metaval v valu in
    let cstk, gstk = elab_metastack v u stk in
    let cmid, gmid = elab_typ v mid_typ in
    CLoc (loc, exists [v] (cvalu @+ cstk @+ cmid))
    >>> fun env -> Command {pol ; loc;
                            valu = gvalu env;
                            stk = gstk env;
                            mid_typ = gmid env
                           }


  and elab_metaval : uvar -> meta_value elaboration = fun u mval ->
    let MetaVal {node; val_typ; loc} = mval in
    let cnode, gnode = elab_val u node in
    let ctyp, gtyp = elab_typ u val_typ in
    CLoc (loc, cnode @+ ctyp )
    >>> fun env ->
    MetaVal {node = gnode env; val_typ = gtyp env; loc}


  and elab_metastack : uvar -> uvar -> meta_stack elaboration =
    fun ucont ufinal mstk ->
    let MetaStack {node; cont_typ; loc} = mstk in
    let cnode, gnode = elab_stack ucont ufinal node in
    let ccont, gcont = elab_typ ucont cont_typ in
    CLoc (loc, cnode @+ ccont) >>> fun env ->
    MetaStack {node = gnode env;
               cont_typ = gcont env;
               loc}

  and elab_var u var =
    let con = cvar (Var.to_int var) u in
    (* let con = *)
    (*   match Var.Env.find var !(Prelude.it).var_multiplicities with *)
    (*   | MulZero | MulMany -> *)
    (*     let v = fresh_u (Base Negative) in *)
    (*     let u' = shallow ~sort:(Base Positive) (Shallow (Box Exponential, [v])) in *)
    (*     exists [v;u'] (eq u u' @+ con) *)
    (*   | MulOne -> con *)
    (* in *)
    con, fun _ -> var

  and elab_covar u var =
    let con = cvar (CoVar.to_int var) u in
    con, fun _ -> var

  and elab_val : uvar -> pre_value elaboration =
    fun u valu -> match valu with

      | Var x ->
        (* TODO especialize here *)
        let con, gvar = elab_var u x in
        con, fun env -> Var (gvar env)

      | CoTop ->
        let v,fvs = of_rank1_typ ~sort:(Base Negative) top in
        exists fvs (eq u v) >>> fun _ -> CoTop

      | Bindcc { bind=(a,t); pol; cmd } ->
        (* TODO generalize here *)
        let ct, gt = elab_typ u t in
        let ccmd, gcmd = elab_cmd u cmd in
        ct @+ CDef (CoVar.to_int a, CoVar.to_string a, u, ccmd)
        >>> fun env -> Bindcc {
          pol;
          bind = (a, gt env);
          cmd = gcmd env
        }

      | Box { kind; bind=(a,t); cmd } ->
        let v = fresh_u (Base Negative) in
        let q = shallow ~sort:Qualifier (Shallow (Cons (Qual kind), [])) in
        let u' = shallow ~sort:(Base Positive) (Shallow (Cons Closure, [q;v])) in
        let cbind, gbind = elab_typ v t in
        let ccmd, gcmd = elab_cmd v cmd in
        exists [q;v;u'] (CDef (CoVar.to_int a, CoVar.to_string a, v, cbind @+ ccmd @+ eq u u'))
        >>> fun env -> Box {
          kind;
          bind = (a, gbind env);
          cmd = gcmd env
        }

      | Fix {self=(x,t); cmd; cont=(a,t')} ->
        let w = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Negative) (Shallow (Cons Fix, [w])) in
        let q = shallow ~sort:Qualifier (Shallow (Cons (Qual Exponential), [])) in
        let v = shallow ~sort:(Base Positive) (Shallow (Cons Closure, [q;u'])) in
        let ccmd, gcmd = elab_cmd w cmd in
        let cbind, gbind = elab_typ v t in
        let ccont, gcont = elab_typ w t' in
        exists [q;u';v;w] (CDef (Var.to_int x, Var.to_string x, v,
                                 CDef (CoVar.to_int a, CoVar.to_string a, w,
                                       eq u u' @+ cbind @+ ccont @+ ccmd)))
        >>> fun env ->
        Fix { self = (x, gbind env);
              cmd = gcmd env;
              cont = (a, gcont env)}

      | Cons cons ->
        let ccons, gcons = elab_cons u cons in
        ccons >>> fun env -> Cons (gcons env)

      | Destr {default; cases} ->
        let ccases, gcases = List.split @@ List.map (elab_copatt u) cases in
        let cdefault, gdefault = match default with
          | None -> CTrue, fun _ -> None
          | Some ((a,t), cmd) ->
            let ct, gt = elab_typ u t in
            let ccmd, gcmd = elab_cmd u cmd in
            ct @+ CDef (CoVar.to_int a, CoVar.to_string a, u, ccmd)
            >>> fun env -> Some ((a, gt env), gcmd env)
        in
        cdefault @+ CAnd ccases
        >>> fun env -> Destr {cases = List.map (fun f -> f env) gcases;
                              default = gdefault env}

  and elab_stack : uvar -> uvar -> pre_stack elaboration =
    fun ucont ufinal stk -> match stk with

      (* TODO spectialize here *)
      | Ret a ->
        let con, gvar = elab_covar ucont a in
        con @+ eq ucont ufinal
        >>> fun env -> Ret (gvar env)

      | CoZero ->
        let v,fvs = of_rank1_typ ~sort:(Base Positive) zero in
        exists fvs (eq ucont v) >>> fun _ -> CoZero

      (* TODO generalize here *)
      | CoBind { bind=(x,t); pol; cmd } ->
        let ccmd, gcmd = elab_cmd ufinal cmd in
        let cbind, gbind = elab_typ ucont t in
        CDef (Var.to_int x, Var.to_string x, ucont, cbind @+ ccmd)
        >>> fun env -> CoBind {
          bind = (x, gbind env);
          cmd = gcmd env;
          pol
        }

      | CoBox { kind; stk } ->
        let v = fresh_u (Base Negative) in
        let q = shallow ~sort:Qualifier (Shallow (Cons (Qual kind), [])) in
        let u' = shallow ~sort:(Base Positive) (Shallow (Cons Closure, [q;v])) in
        let cstk, gstk = elab_metastack v ufinal stk in
        exists [q;v;u'] (eq ucont u' @+ cstk)
        >>> fun env -> CoBox {
          stk = gstk env;
          kind
        }

      | CoFix stk ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Negative) (Shallow (Cons Fix, [v])) in
        let cstk, gstk = elab_metastack v ufinal stk in
        exists [v;u'] (eq ucont u' @+ cstk)
        >>> fun env -> CoFix (gstk env)

      | CoDestr destr ->
        let cdestr, gdestr = elab_destr ucont ufinal destr in
        cdestr >>> fun env -> CoDestr (gdestr env)

      | CoCons {cases; default} ->
        let ccases, gcases = List.split @@ List.map (elab_patt ucont ufinal) cases in
        let cdefault, gdefault = match default with
          | None -> CTrue, fun _ -> None
          | Some ((x,t), cmd) ->
            let ct, gt = elab_typ ucont t in
            let ccmd, gcmd = elab_cmd ufinal cmd in
            ct @+ CDef (Var.to_int x, Var.to_string x, ucont, ccmd)
            >>> fun env -> Some ((x, gt env), gcmd env)
        in
        cdefault @+ CAnd ccases
        >>> fun env ->
        CoCons {cases = List.map (fun f -> f env) gcases;
                default = gdefault env}

  and elab_cons  u (Raw_Cons cons) =

    let Consdef { resulting_type; typ_args; equations;
                  constructor = Raw_Cons def} =
      def_of_cons Prelude.it cons.tag in
    let n = List.length def.args in
    let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
    let cargs, gargs = List.split @@ List.map2 elab_metaval vs cons.args in

    let typ_args_u = of_tvars typ_args in
    let idxs_u = of_tvars def.idxs in
    let fve, equations = of_eqns equations in
    let cidxs, gidxs = List.map2 elab_typ idxs_u cons.idxs |> List.split in
    let so = match cons.tag with Thunk -> sort_negtype | _ -> sort_postype in
    let u', fvs = of_rank1_typ ~sort:so resulting_type in
    let args, fvss = List.split
        (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
    let fvs = List.concat (typ_args_u :: idxs_u :: fve :: fvs :: vs :: fvss) in

    exists ~st:equations fvs
      (eq u u' @+ CAnd (List.map2 eq vs args) @+ CAnd cargs @+  CAnd cidxs)

    >>> fun env -> Raw_Cons {
      tag = def.tag;
      idxs = List.map (fun f -> f env) gidxs;
      args = List.map (fun f -> f env) gargs
    }


  and elab_destr ucont ufinal (Raw_Destr destr) =


    let Destrdef {resulting_type; typ_args; equations;
                  destructor = Raw_Destr def} =
      def_of_destr Prelude.it destr.tag in

    let n = List.length def.args in
    let args_u = List.init n (fun _ -> fresh_u (Base Positive)) in
    let w = fresh_u (Base Negative) in

    let typ_args_u = of_tvars typ_args in
    let idxs_u = of_tvars def.idxs in
    let eq_fvs, equations = of_eqns equations in
    let cidxs, gidxs = List.split (List.map2 elab_typ idxs_u destr.idxs) in
    let cret, gret = elab_metastack w ufinal destr.cont in
    let cargs, gargs = List.split (List.map2 elab_metaval args_u destr.args) in

    let so = match destr.tag with Closure _ -> sort_postype | _ -> sort_negtype in
    let u', fvs = of_rank1_typ ~sort:so resulting_type in
    let vs', fvss = List.split (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
    let w', fvs' = of_rank1_typ ~sort:(Base Negative) def.cont in

    exists ~st:equations
      (List.concat (typ_args_u ::idxs_u :: fvs :: fvs' :: args_u :: eq_fvs :: fvss))
      (eq ucont u'
       @+ eq w w'
       @+ CAnd (List.map2 eq args_u vs')
       @+ CAnd cargs
       @+ CAnd cidxs
       @+ cret)
    >>> fun env -> Raw_Destr {
      tag = def.tag;
      idxs = List.map (fun f -> f env) gidxs;
      args = List.map (fun f -> f env) gargs;
      cont = gret env
    }


  and elab_patt ucont ufinal (Raw_Cons patt, cmd) =


    let Consdef { typ_args; resulting_type; equations; constructor = Raw_Cons def}
      = def_of_cons Prelude.it patt.tag in

    if def.idxs = [] && equations = [] then begin

      let typ_args_u = of_tvars typ_args in
      let args_u = List.init (List.length patt.args) (fun _ -> fresh_u (Base Positive)) in
      let def_args_u, fvss = List.split
          (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let so = match def.tag with Thunk -> sort_negtype | _ -> sort_postype in
      let u', fvs = of_rank1_typ ~sort:so resulting_type in

      let fvs = List.concat (typ_args_u :: args_u :: fvs :: fvss) in
      let c = exists fvs ( CAnd (List.map2 eq args_u def_args_u)
                           @+ ccmd
                           @+ eq ucont u') in
      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_int x, Var.to_string x, v, c)) in
      let c = List.fold_left2 go c def_args_u patt.args in

      c >>> fun env ->
      (Raw_Cons {
          tag = def.tag;
          idxs = [];
          args = List.map2 (fun (x,_) v -> x, env.u v) patt.args args_u;
        }, gcmd env)

    end else begin

      let typ_args_u = of_tvars typ_args in

      enter ();

      let idxs_u = of_tvars patt.idxs in
      let def_idxs_u = of_tvars def.idxs in
      let eq_fvs, equations = of_eqns equations in

      let args_u = List.init (List.length patt.args) (fun _ -> fresh_u (Base Positive)) in
      let def_args_u, fvss = List.split
          (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let so = match def.tag with Thunk -> sort_negtype | _ -> sort_postype in
      let u', fvs = of_rank1_typ ~sort:so resulting_type in

      let fvs = List.concat (idxs_u :: def_idxs_u :: args_u :: fvs :: fvss) in
      let c = exists fvs ( CAnd (List.map2 eq args_u def_args_u)
                           @+ ccmd
                           @+ eq ucont u') in
      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_int x, Var.to_string x, v, c)) in
      let c = List.fold_left2 go c def_args_u patt.args in

      leave ();

      let rec mk_model_eqns = function
        | x::xs, y::ys, (_,so)::sos -> Eq (x,y,so) :: mk_model_eqns (xs, ys, sos)
        | [],[],[] -> []
        | _ -> assert false in

      exists typ_args_u (CGoal {
        typs = args_u;
        accumulated = idxs_u @ eq_fvs;
        existentials = [];
        exist_eqns = [];
        univ_eqns = equations @ mk_model_eqns (idxs_u, def_idxs_u, def.idxs);
        inner = c;
        outer = CTrue;
        quantification_duty = def_idxs_u;
      })
      >>> fun env ->
      (Raw_Cons {
          tag = def.tag;
          idxs = List.map (fun v -> (env.get v, get_sort v)) idxs_u;
          args = List.map2 (fun (x,_) v -> x, env.u v) patt.args args_u;
        }, gcmd env)

    end

  and elab_copatt ucont (Raw_Destr copatt, cmd) =

    let Destrdef { typ_args; resulting_type; equations; destructor = Raw_Destr def}
      = def_of_destr Prelude.it copatt.tag in

    if def.idxs = [] && equations = [] then begin

      let ufinal = fresh_u (Base Negative) in
      let typ_args_u = of_tvars typ_args in
      let args_u = List.init (List.length copatt.args) (fun _ -> fresh_u (Base Positive)) in
      let def_args_u, fvss = List.split
          (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let so = match def.tag with Closure _ -> sort_postype | _ -> sort_negtype in
      let u', fvs = of_rank1_typ ~sort:so resulting_type in
      let a, ret = copatt.cont in
      let def_cont_u, fvs' = of_rank1_typ ~sort:sort_negtype def.cont in
      let cont_u, fvs'' = of_rank1_typ ~sort:sort_negtype ret in
      let fvs = List.concat (args_u :: fvs :: fvs' :: fvs'' :: fvss) in
      let c = exists fvs ( CAnd (List.map2 eq args_u def_args_u)
                           @+ ccmd
                           @+ eq ucont u' @+ eq cont_u def_cont_u) in
      let c = CDef (CoVar.to_int a, CoVar.to_string a, cont_u, c ) in
      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_int x, Var.to_string x, v, c)) in
      let fvs = ufinal :: typ_args_u in
      let c = exists fvs (List.fold_left2 go c def_args_u copatt.args) in

      c >>> fun env ->
      (Raw_Destr {
          tag = def.tag;
          idxs = [];
          args = List.map2 (fun (x,_) v -> x, env.u v) copatt.args args_u;
          cont = a, env.u cont_u
        }, gcmd env)

    end else begin

      enter ();

      let ufinal = fresh_u (Base Negative) in
      let idxs_u = of_tvars copatt.idxs in
      let def_idxs_u = of_tvars def.idxs in
      let eq_fvs, equations = of_eqns equations in
      let typ_args_u = of_tvars typ_args in
      let args_u = List.init (List.length copatt.args) (fun _ -> fresh_u (Base Positive)) in
      let def_args_u, fvss = List.split
          (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let so = match def.tag with Closure _ -> sort_postype | _ -> sort_negtype in
      let u', fvs = of_rank1_typ ~sort:so resulting_type in
      let a, ret = copatt.cont in
      let def_cont_u, fvs' = of_rank1_typ ~sort:sort_negtype def.cont in
      let cont_u, fvs'' = of_rank1_typ ~sort:sort_negtype ret in
      let fvs = List.concat (idxs_u :: fvs' :: fvs''
                             :: def_idxs_u :: eq_fvs :: args_u :: fvs :: fvss) in
      let c = exists fvs ( ccmd
                           @+ CAnd (List.map2 eq def_idxs_u idxs_u)
                           @+ eq ucont u' @+ eq cont_u def_cont_u) in
      let c = CDef (CoVar.to_int a, CoVar.to_string a, cont_u, c ) in
      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_int x, Var.to_string x, v, c)) in
      let c = exists typ_args_u (List.fold_left2 go c def_args_u copatt.args) in

      leave ();


      let rec mk_model_eqns = function
        | x::xs, y::ys, (_,so)::sos -> Eq (x,y,so) :: mk_model_eqns (xs, ys, sos)
        | [],[],[] -> []
        | _ -> assert false in

      exists typ_args_u (CGoal {
        typs = args_u;
        accumulated = idxs_u @ eq_fvs;
        existentials = [];
        exist_eqns = [];
        univ_eqns = equations @ mk_model_eqns (idxs_u, def_idxs_u, def.idxs);
        inner = c;
        outer = CTrue;
        quantification_duty = def_idxs_u;
      })
      >>> fun env ->
      (Raw_Destr {
          tag = def.tag;
          idxs = List.map (fun v -> (env.get v, get_sort v)) idxs_u;
          args = List.map2 (fun (x,_) v -> x, env.u v) copatt.args args_u;
          cont = a, env.u cont_u
        }, gcmd env)

    end

  let elab_prog_items items =

    let go (con, gen) item = match item with

      | Value_declaration {bind = (name, typ); pol; loc} ->
        let u,fvs = of_rank1_typ ~sort:(Base pol) typ in
        exists fvs (CDef (Var.to_int name, Var.to_string name, u, con))
        >>> fun env ->
        Value_declaration {bind = (name, env.u u); pol; loc;} :: gen env

      | Value_definition {bind = (name, typ); pol; loc; content} ->
        let u, fvs = of_rank1_typ ~sort:(Base pol) typ in
        let cc, cgen = elab_metaval u content in
        exists fvs (cc @+ CDef (Var.to_int name, Var.to_string name, u, con))
        >>> fun env ->
        Value_definition {
          bind = (name, env.u u);
          pol; loc;
          content = cgen env} :: gen env

      | Command_execution {name; pol; cont; conttyp; loc; content} ->
        let u, fvs = of_rank1_typ ~sort:(Base pol) conttyp in
        let cc, cgen = elab_cmd u content in
        exists fvs (CDef (CoVar.to_int cont, CoVar.to_string cont, u, cc @+ con))
        >>> fun env ->
        Command_execution {
          name; pol; loc;
          cont = cont;
          conttyp = env.u u;
          content = cgen env} :: gen env in

    List.fold_left go (CTrue, fun _ -> []) (List.rev items)

  let go ~trace:trace items =
    let item, post = solve ~trace elab_prog_items items in
    item, Obj.magic post
end
