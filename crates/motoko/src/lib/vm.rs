use crate::ast::{
    BinOp, Cases, Dec, DecField, Dec_, Delim, Exp, ExpField, ExpField_, Exp_, Id, Id_, Inst,
    Literal, Mut, Pat, Pat_, PrimType, Prog, ProjIndex, RelOp, Source, Stab_, ToId, Type, UnOp,
    Vis_,
};
//use crate::ast_traversal::ToNode;
use crate::shared::{FastClone, Share};
use crate::value::{
    ActorId, ActorMethod, Closed, ClosedFunction, CollectionFunction, FastRandIter,
    FastRandIterFunction, HashMapFunction, PrimFunction, Value, ValueError, Value_,
};
use crate::vm_types::{
    def::{
        Actor as ActorDef, Ctx, CtxId, Def, Defs, Field as FieldDef, Function as FunctionDef,
        Module as ModuleDef, Var as VarDef,
    },
    stack::{FieldContext, FieldValue, Frame, FrameCont},
    Activation, Active, ActiveBorrow, Actor, Actors, Agent, Breakpoint, Cont, Core, CoreSource,
    Counts, DebugPrintLine, Env, Interruption, Limit, Limits, LocalPointer, ModuleFile,
    ModuleFileInit, ModuleFileState, ModuleFiles, ModulePath, NamedPointer, Pointer, Response,
    ScheduleChoice, Stack, Step, SyntaxError,
};
use crate::vm_types::{EvalInitError, Store};
use im_rc::{HashMap, Vector};
use num_bigint::{BigUint, ToBigInt};
use num_traits::ToPrimitive;
use std::vec::Vec;

use crate::{type_mismatch, type_mismatch_};

macro_rules! nyi {
    ($line:expr) => {
        Err(Interruption::NotYetImplemented(CoreSource {
            name: None,
            description: Some("Not yet implemented in current VM logic".to_string()),
            file: file!().to_string(),
            line: $line,
        }, None))
    };
    ($line:expr, $($mesg:tt)+) => {
        Err(Interruption::NotYetImplemented(CoreSource {
            name: None,
            description: Some("Not yet implemented in current VM logic".to_string()),
            file: file!().to_string(),
            line: $line,
        }, Some(format!($($mesg)+)) ))
    };
}

impl Interruption {
    pub fn is_recoverable(&self) -> bool {
        match self {
            Interruption::Send(..) => true,
            Interruption::Response(..) => true,
            Interruption::Breakpoint(..) => true,
            Interruption::Limit(..) => true,
            _ => false,
        }
    }
}

/*
trait InterruptionLike {
    fn interruption<'a>(&'a self) -> Option<&'a Interruption>;
}

impl InterruptionLike for Interruption {
    fn interruption<'a>(&'a self) -> Option<&'a Interruption> {
        Some(self)
    }

    }
}
*/

impl From<()> for Interruption {
    // try to avoid this conversion, except in temp code.
    fn from(_x: ()) -> Interruption {
        Interruption::Unknown
    }
}

impl Def {
    pub fn source(&self) -> Source {
        // to do
        Source::Evaluation
    }
}

impl crate::vm_types::def::Field {
    pub fn source(&self) -> Source {
        self.def.source()
    }
}

impl CtxId {
    pub fn get_field<'a, A: ActiveBorrow>(&self, a: &'a A, id: &Id) -> Option<&'a FieldDef> {
        a.defs().map.get(self).unwrap().fields.get(id)
    }
}

impl Defs {
    fn new() -> Self {
        let mut map = HashMap::new();
        let root = Ctx {
            parent: None,
            fields: HashMap::new(),
        };
        map.insert(CtxId(0), root);
        Defs {
            map,
            active_ctx: CtxId(0),
            next_ctx_id: 1,
        }
    }
    fn active_context(&self) -> CtxId {
        self.active_ctx.clone()
    }
    fn enter_context(&mut self, is_a_root: bool) -> (CtxId, CtxId) {
        let x = self.next_ctx_id;
        self.next_ctx_id = self
            .next_ctx_id
            .checked_add(1)
            .expect("Out of def-context ids.");
        let ctx = Ctx {
            parent: if is_a_root {
                None
            } else {
                Some(self.active_ctx.clone())
            },
            fields: HashMap::new(),
        };
        let prev = self.map.insert(CtxId(x), ctx);
        assert_eq!(prev, None);
        let saved = self.active_ctx.clone();
        self.active_ctx = CtxId(x);
        (saved, CtxId(x))
    }
    fn reenter_context(&mut self, x: &CtxId) -> (CtxId, CtxId, Ctx) {
        let old_ctx = self.map.get(x).unwrap().clone();
        let new_ctx = Ctx {
            parent: old_ctx.parent.clone(),
            fields: HashMap::new(),
        };
        let saved = self.active_ctx.clone();
        self.active_ctx = x.clone();
        self.map.insert(x.clone(), new_ctx);
        (saved, x.clone(), old_ctx)
    }
    fn insert_field(
        &mut self,
        i: &Id,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        def: Def,
    ) -> Result<(), Interruption> {
        let s = source.clone();
        let a = self.active_ctx.clone();
        let y = self.map.get_mut(&a).unwrap().fields.insert(
            i.clone(),
            crate::vm_types::def::Field {
                source,
                stab,
                vis,
                def,
            },
        );
        if let Some(y) = y {
            // to do -- both source infos for the error.
            Err(Interruption::AmbiguousIdentifer(
                i.clone(),
                s,
                y.source().clone(),
            ))
        } else {
            Ok(())
        }
    }
    // special-case: for named modules and actors whose new defs over-write prior ones.
    fn reinsert_field(
        &mut self,
        i: &Id,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        def: Def,
    ) -> Result<(), Interruption> {
        let a = self.active_ctx.clone();
        let y = self.map.get_mut(&a).unwrap().fields.insert(
            i.clone(),
            crate::vm_types::def::Field {
                source,
                stab,
                vis,
                def,
            },
        );
        if let Some(_) = y {
            Ok(())
        } else {
            unreachable!()
        }
    }

    fn leave_context(&mut self, saved: CtxId, sanity_check_active: &CtxId) {
        assert_eq!(&self.active_ctx, sanity_check_active);
        if let Some(parent) = self
            .map
            .get(&self.active_ctx)
            .expect("leave context")
            .parent
            .as_ref()
        {
            assert_eq!(parent, &saved)
        }
        self.active_ctx = saved;
    }
    fn report_diff(&self, _old_ctx: &Ctx) {
        // to do
        //
        // compare entries in current context to that of given old context param.
        // - for entries in both, the current context is the upgraded version, or unchanged version.
        // - (detecting change vs upgrade requires comparing but ingoring source locations, which can shift).
        // - for entries in only old context, the new context is deleting them.
        // - for entries in only the new context, the new context is adding it.
    }
    fn releave_context(&mut self, saved: CtxId, sanity_check_active: &CtxId, old_ctx: &Ctx) {
        self.report_diff(old_ctx);
        self.leave_context(saved, sanity_check_active)
    }
}

impl Active for Core {
    fn ctx_id<'a>(&'a mut self) -> &'a mut CtxId {
        &mut self.defs.active_ctx
    }
    fn defs<'a>(&'a mut self) -> &'a mut Defs {
        &mut self.defs
    }
    fn module_files<'a>(&'a mut self) -> &'a mut ModuleFiles {
        &mut self.module_files
    }
    //fn schedule_choice<'a>(&'a self) -> &'a ScheduleChoice {
    //&self.schedule_choice
    //}
    fn cont<'a>(&'a mut self) -> &'a mut Cont {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.cont,
            Actor(ref n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .cont
            }
        }
    }
    fn package<'a>(&'a mut self) -> &'a mut Option<String> {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.package,
            Actor(ref n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .package
            }
        }
    }
    fn cont_source<'a>(&'a mut self) -> &'a mut Source {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.cont_source,
            Actor(ref n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .cont_source
            }
        }
    }
    fn cont_prim_type<'a>(&'a mut self) -> &'a mut Option<PrimType> {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.cont_prim_type,
            Actor(ref n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .cont_prim_type
            }
        }
    }
    fn env<'a>(&'a mut self) -> &'a mut Env {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.env,
            Actor(ref n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .env
            }
        }
    }
    fn stack<'a>(&'a mut self) -> &'a mut Stack {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.stack,
            Actor(ref n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .stack
            }
        }
    }
    fn store<'a>(&'a mut self) -> &'a mut Store {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.store,
            Actor(ref n) => &mut self.actors.map.get_mut(n).unwrap().store,
        }
    }
    fn debug_print_out<'a>(&'a mut self) -> &'a mut Vector<DebugPrintLine> {
        &mut self.debug_print_out
    }
    fn counts<'a>(&'a mut self) -> &'a mut Counts {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.counts,
            Actor(ref n) => &mut self.actors.map.get_mut(n).unwrap().counts,
        }
    }

    fn create(
        &mut self,
        path: String,
        name: ActorId,
        def: ActorDef,
    ) -> Result<Value_, Interruption> {
        let v = Value::Actor(crate::value::Actor {
            def: Some(def.clone()),
            id: name.clone(),
        });
        //let def = self.defs().map.get(&CtxId(0)).unwrap().fields.get(name).unwrap().def.clone();
        let mut store = Store::new(ScheduleChoice::Actor(name.clone()));
        let mut env = self.env().clone();
        let ctx = self.defs().map.get(&def.fields).unwrap();
        for (i, field) in ctx.fields.iter() {
            match &field.def {
                Def::Var(v) => {
                    store.alloc_named(i.clone(), v.init.fast_clone());
                    let owner = ScheduleChoice::Actor(name.clone());
                    env.insert(
                        i.clone(),
                        Value::Pointer(Pointer {
                            owner,
                            local: LocalPointer::Named(NamedPointer(i.clone())),
                        })
                        .share(),
                    );
                }
                Def::Func(f) => {
                    env.insert(i.clone(), f.rec_value.fast_clone());
                }
                _ => return nyi!(line!()),
            }
        }
        let a = Actor {
            path,
            def,
            env,
            store,
            counts: Counts::default(),
            active: None,
            awaiting: HashMap::new(),
        };
        let a0 = self.actors.map.insert(name, a);
        if let Some(_a0) = a0 {
            todo!("upgrade")
        };
        Ok(v.share())
    }

    fn upgrade(
        &mut self,
        path: String,
        name: ActorId,
        def: ActorDef,
    ) -> Result<Value_, Interruption> {
        let v = Value::Actor(crate::value::Actor {
            def: Some(def.clone()),
            id: name.clone(),
        });
        let mut env = HashMap::new();
        let mut store = self.actors.map.get(&name).unwrap().store.clone();
        let counts = self.actors.map.get(&name).unwrap().counts.clone();
        let ctx = self.defs().map.get(&def.fields).unwrap();
        for (i, field) in ctx.fields.iter() {
            match &field.def {
                Def::Var(v) => {
                    match store.get(&LocalPointer::Named(NamedPointer(i.clone()))) {
                        None => {
                            let p = store.alloc_named(i.clone(), v.init.fast_clone());
                            let pv = Value::Pointer(p).share();
                            env.insert(i.clone(), pv);
                        }
                        Some(_) => {
                            let p = Pointer {
                                owner: ScheduleChoice::Actor(name.clone()),
                                local: LocalPointer::Named(NamedPointer(i.clone())),
                            };
                            let pv = Value::Pointer(p).share();
                            // keep store's current value.
                            // (even if not stable.)
                            env.insert(i.clone(), pv);
                        }
                    }
                }
                Def::Func(..) => {
                    // to do
                }
                _ => todo!(),
            }
        }
        let a = Actor {
            path,
            def,
            env,
            store,
            counts,
            active: None,
            awaiting: HashMap::new(),
        };
        self.actors.map.insert(name, a);
        Ok(v.share())
    }

    fn create_module(
        &mut self,
        _path: ModulePath,
        _id: Option<Id>,
        module: ModuleDef,
    ) -> Result<Value_, Interruption> {
        Ok(crate::value::Value::Module(module).share())
    }

    fn upgrade_module(
        &mut self,
        _path: ModulePath,
        _id: Option<Id>,
        module: ModuleDef,
    ) -> Result<Value_, Interruption> {
        Ok(crate::value::Value::Module(module).share())
    }
}

impl ActiveBorrow for Core {
    fn ctx_id<'a>(&'a self) -> &'a CtxId {
        &self.defs.active_ctx
    }
    fn defs<'a>(&'a self) -> &'a Defs {
        &self.defs
    }
    fn schedule_choice<'a>(&'a self) -> &'a ScheduleChoice {
        &self.schedule_choice
    }
    fn cont<'a>(&'a self) -> &'a Cont {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.cont,
            Actor(ref n) => {
                &self
                    .actors
                    .map
                    .get(n)
                    .unwrap()
                    .active
                    .as_ref()
                    .unwrap()
                    .cont
            }
        }
    }
    fn cont_source<'a>(&'a self) -> &'a Source {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.cont_source,
            Actor(ref n) => {
                &self
                    .actors
                    .map
                    .get(n)
                    .unwrap()
                    .active
                    .as_ref()
                    .unwrap()
                    .cont_source
            }
        }
    }
    fn cont_prim_type<'a>(&'a self) -> &'a Option<PrimType> {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.cont_prim_type,
            Actor(ref n) => {
                &self
                    .actors
                    .map
                    .get(n)
                    .unwrap()
                    .active
                    .as_ref()
                    .unwrap()
                    .cont_prim_type
            }
        }
    }
    fn env<'a>(&'a self) -> &'a Env {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.env,
            Actor(ref n) => &self.actors.map.get(n).unwrap().active.as_ref().unwrap().env,
        }
    }
    fn stack<'a>(&'a self) -> &'a Stack {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.stack,
            Actor(ref n) => {
                &self
                    .actors
                    .map
                    .get(n)
                    .unwrap()
                    .active
                    .as_ref()
                    .unwrap()
                    .stack
            }
        }
    }
    fn store<'a>(&'a self) -> &'a Store {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.store,
            Actor(ref n) => &self.actors.map.get(n).unwrap().store,
        }
    }
    fn debug_print_out<'a>(&'a self) -> &'a Vector<DebugPrintLine> {
        &self.debug_print_out
    }
    fn counts<'a>(&'a self) -> &'a Counts {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.counts,
            Actor(ref n) => &self.actors.map.get(n).unwrap().counts,
        }
    }
}

impl Limits {
    pub fn default() -> Self {
        Self::none()
    }

    /// No limits.
    pub fn none() -> Limits {
        Limits {
            breakpoints: vec![],
            step: None,
            redex: None,
            send: None,
        }
    }
    /// Set step limit.
    pub fn step(mut self, s: usize) -> Self {
        self.step = Some(s);
        self
    }
    /// Set redex limit.
    pub fn redex(mut self, s: usize) -> Self {
        self.redex = Some(s);
        self
    }
}

fn module_project(
    defs: &Defs,
    m: &ModuleDef,
    pattern: &Pat,
) -> Result<Vec<(Id_, Def)>, Interruption> {
    match pattern {
        Pat::Var(x) => Ok(vec![(x.clone(), Def::Module(m.clone()))]),
        Pat::Object(pat_fields) => {
            let mut r = vec![];
            for f in pat_fields.vec.iter() {
                match f.0.pat.clone() {
                    None => {
                        let fd = resolve_def(defs, &m.fields, true, &f.0.id.0)?;
                        r.push((f.0.id.clone(), fd.def.clone()))
                    }
                    Some(Pat::Var(x)) => {
                        let fd = resolve_def(defs, &m.fields, true, &f.0.id.0)?;
                        r.push((x.clone(), fd.def.clone()))
                    }
                    p => return nyi!(line!(), "module_project object-field pattern {:?}", p),
                }
            }
            Ok(r)
        }
        pattern => nyi!(line!(), "module_project ({:?}, {:?})", m, pattern),
    }
}

mod def {
    use super::*;
    use crate::ast::{DecField, DecFields};

    pub fn import<A: Active>(active: &mut A, path: &str) -> Result<ModuleDef, Interruption> {
        let path0 = path.clone(); // for log.
        let path = format!("{}", &path[1..path.len() - 1]);
        let (package_name, local_path) = if path == "mo:⛔" || path == "mo:prim" {
            (Some("⛔".to_string()), "lib".to_string())
        } else if path.starts_with("mo:") {
            let path = format!("{}", &path[3..path.len()]);
            let mut sep_parts = path.split("/");
            if let Some(package_name) = sep_parts.next() {
                if let Some(_) = sep_parts.next() {
                    let local_path = format!("{}", &path[package_name.len() + 1..path.len()]);
                    (Some(package_name.to_string()), local_path)
                } else {
                    (Some(package_name.to_string()), "lib".to_string())
                }
            } else {
                return nyi!(line!(), "import {}", path);
            }
        } else {
            (active.package().clone(), path)
        };
        let path = crate::vm_types::ModulePath {
            package_name: package_name.clone(),
            local_path,
        };
        log::debug!(
            "`import {}` resolves as `import {:?}`.  Attemping to import...",
            path0,
            path
        );
        let mf = active.module_files().map.get(&path).map(|x| x.clone());
        let mf = match mf {
            None => return Err(Interruption::ModuleFileNotFound(path)),
            Some(ModuleFileState::Defined(mf)) => mf,
            Some(ModuleFileState::Init(init)) => {
                // if a module imports itself, directly or indirectly, we we diverge without cycle detection.
                // so, detect a cycle by tracking import paths.
                let contains_this_path = active.module_files().import_stack.contains(&path);
                if contains_this_path {
                    let mut stack = active.module_files().import_stack.clone();
                    stack.push_back(path.clone());
                    return Err(Interruption::ImportCycle(stack));
                } else {
                    active.module_files().import_stack.push_back(path.clone());
                };
                let importing_package = active.package().clone();
                *active.package() = package_name;
                let (saved, ctxid) = active.defs().enter_context(true);
                for dec in init.outer_decs.iter() {
                    let dec = dec.clone();
                    let df = crate::ast::DecField {
                        vis: None,
                        stab: None,
                        dec,
                    };
                    def::insert_static_field(active, &df.dec.1, &df)?;
                }
                let do_promote_fields_to_public_vis =
                // if the package_name is ⛔, then "promote" everything to be public.
                    active.package().as_ref().map_or(false, |n| n.as_str() == "⛔");
                let fields = // promote.
                    if do_promote_fields_to_public_vis {
                        Delim{
                            vec:
                            init.fields.vec.iter().map(
                                |f|
                                crate::ast::NodeData(
                                    DecField{
                                        vis:Some(
                                            crate::ast::NodeData(crate::ast::Vis::Public(None),
                                                                 crate::ast::Source::ImportPrim).share()),
                                        .. f.0.clone()},
                                    f.1.clone()).share()).collect(),
                            .. init.fields.clone()
                        }
                    } else {
                        init.fields.clone()
                    };
                let v = def::module(
                    active,
                    path.clone(),
                    &init.id,
                    Source::CoreSetModule,
                    None,
                    None,
                    &fields,
                    None,
                )?;
                active.defs().leave_context(saved, &ctxid);
                *active.package() = importing_package;
                if let Some(top_path) = active.module_files().import_stack.pop_back() {
                    assert_eq!(top_path, path)
                } else {
                    unreachable!()
                };
                if let Value::Module(m) = &*v {
                    let mf = ModuleFile {
                        file_content: init.file_content.clone(),
                        context: m.context.clone(),
                        module: m.fields.clone(),
                    };
                    active
                        .module_files()
                        .map
                        .insert(path.clone(), ModuleFileState::Defined(mf.clone()));
                    mf
                } else {
                    unreachable!()
                }
            }
        };
        log::debug!("`import {}` Success.", path0);
        Ok(mf.def())
    }

    pub fn insert_static_field<A: Active>(
        active: &mut A,
        source: &Source,
        df: &DecField,
    ) -> Result<(), Interruption> {
        //println!("{:?} -- {:?} ", source, df);
        match &df.dec.0 {
            Dec::LetModule(id, _, dfs) => {
                let v = module(
                    active,
                    ModulePath {
                        package_name: None,
                        local_path: format!("<anonymous@{}>", &df.dec.1),
                    },
                    id,
                    df.dec.1.clone(),
                    df.vis.clone(),
                    df.stab.clone(),
                    dfs,
                    None,
                )?;
                if let Some(id) = id {
                    if let Value::Module(m) = &*v {
                        active.defs().insert_field(
                            &id.0,
                            source.clone(),
                            df.vis.clone(),
                            df.stab.clone(),
                            Def::Module(m.clone()),
                        )?;
                    };
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            Dec::Func(f) => {
                if let Some(name) = f.name.clone() {
                    let f = FunctionDef {
                        context: active.defs().active_ctx.clone(),
                        function: f.clone(),
                        rec_value: Value::Function(ClosedFunction(Closed {
                            ctx: active.defs().active_ctx.clone(),
                            env: active.env().fast_clone(),
                            content: f.clone(),
                        }))
                        .share(),
                    };
                    active.defs().insert_field(
                        &name.0,
                        source.clone(),
                        df.vis.clone(),
                        df.stab.clone(),
                        Def::Func(f),
                    )?;
                    Ok(())
                } else {
                    nyi!(line!())
                }
            }
            Dec::Var(_p, _e) => Err(Interruption::ModuleNotStatic(
                df.dec.1.clone(),
                Some("var-decl".to_string()),
            )),
            Dec::Exp(e) => {
                if exp_is_static(&e.0) {
                    // ignore pure expression with no name.
                    Ok(())
                } else {
                    Err(Interruption::ModuleNotStatic(
                        df.dec.1.clone(),
                        Some(format!("non-static-exp({:?})", e)),
                    ))
                }
            }
            Dec::Let(p, e) => {
                if let Pat::Wild = p.0 {
                    if exp_is_static(&e.0) {
                        // ignore pure expression with no name.
                        Ok(())
                    } else {
                        Err(Interruption::ModuleNotStatic(
                            df.dec.1.clone(),
                            Some(format!("non-static-let({:?})", e)),
                        ))
                    }
                } else {
                    match get_pat_var(&p.0) {
                        Some(x) => {
                            let ctx_id = active.defs().active_ctx.clone();
                            if exp_is_static(&e.0) {
                                active.defs().insert_field(
                                    &x.0,
                                    source.clone(),
                                    df.vis.clone(),
                                    df.stab.clone(),
                                    Def::StaticValue(ctx_id, e.clone()),
                                )?;
                                Ok(())
                            } else {
                                Err(Interruption::ModuleNotStatic(
                                    source.clone(),
                                    Some(format!("non-static-let({:?})", e)),
                                ))
                            }
                        }
                        None => {
                            nyi!(line!())
                        }
                    }
                }
            }
            Dec::LetImport(pattern, _, path) => {
                let m = import(active, path)?;
                let fields = module_project(active.defs(), &m, &pattern.0)?;
                for (x, def) in fields {
                    active
                        .defs()
                        .insert_field(&x.0, x.1.clone(), None, None, def.clone())?;
                }
                Ok(())
            }
            Dec::Type(_id, _typ_binds, _typ) => Ok(()),
            Dec::LetActor(_i, _, _dfs) => {
                nyi!(line!())
            }
            Dec::LetObject(_i, _, _dfs) => {
                nyi!(line!())
            }
            Dec::Class(_class) => {
                // 20221209-1047 to do.
                // nyi!(line!())
                Ok(())
            }
        }
    }

    fn insert_owned_field<A: Active>(
        active: &mut A,
        def_owner: &ScheduleChoice,
        source: &Source,
        df: &DecField,
    ) -> Result<(), Interruption> {
        //println!("{:?} -- {:?} ", source, df);
        match &df.dec.0 {
            Dec::Func(f) => {
                if let Some(name) = f.name.clone() {
                    let f = FunctionDef {
                        context: active.defs().active_ctx.clone(),
                        function: f.clone(),
                        rec_value: Value::Function(ClosedFunction(Closed {
                            ctx: active.defs().active_ctx.clone(),
                            env: active.env().fast_clone(),
                            content: f.clone(),
                        }))
                        .share(),
                    };
                    active.defs().insert_field(
                        &name.0,
                        source.clone(),
                        df.vis.clone(),
                        df.stab.clone(),
                        Def::Func(f),
                    )?;
                    Ok(())
                } else {
                    nyi!(line!())
                }
            }
            Dec::Var(p, e) => {
                let v = match &e.0 {
                    Exp::Literal(l) => Value::from_literal(l)?,
                    _ => return Err(Interruption::NonLiteralInit(e.1.clone())),
                };
                let mut pat = &p.0;
                if let Pat::AnnotPat(ref p, _) = pat {
                    // Temporary
                    pat = &p.0;
                }
                match pat {
                    Pat::Var(ref x) => {
                        let var_def = Def::Var(VarDef {
                            owner: def_owner.clone(),
                            name: x.0.clone(),
                            init: v.share(),
                        });
                        active.defs().insert_field(
                            &x.0,
                            source.clone(),
                            df.vis.clone(),
                            df.stab.clone(),
                            var_def,
                        )?;
                        //Ok(Value::Pointer(Pointer::NamedPointer(x.0.clone())))
                        Ok(())
                    }
                    _ => nyi!(line!()),
                }
            }
            _ => nyi!(line!()),
        }
    }

    pub fn actor<A: Active>(
        active: &mut A,
        path: String,
        id: &ActorId,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        dfs: &DecFields,
    ) -> Result<Value_, Interruption> {
        let (parent, fields) = active.defs().enter_context(false);
        for df in dfs.vec.iter() {
            insert_owned_field(active, &ScheduleChoice::Actor(id.clone()), &df.1, &df.0)?;
        }
        active.defs().leave_context(parent, &fields);
        let context = active.defs().active_context();
        let actor = crate::vm_types::def::Actor { context, fields };
        match id {
            ActorId::Alias(_x) => {
                /* 20221103
                active
                    .defs()
                    .insert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
                */
            }
            ActorId::Local(x) => {
                active
                    .defs()
                    .insert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
            }
        }
        active.create(path, id.clone(), actor)
    }

    pub fn actor_upgrade<A: Active>(
        active: &mut A,
        path: String,
        id: &ActorId,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        dfs: &DecFields,
        old_def: &ActorDef,
    ) -> Result<Value_, Interruption> {
        let (saved, fields, old_ctx) = active.defs().reenter_context(&old_def.fields);
        for df in dfs.vec.iter() {
            insert_owned_field(active, &ScheduleChoice::Actor(id.clone()), &df.1, &df.0)?;
        }
        active.defs().releave_context(saved, &fields, &old_ctx);
        let context = active.defs().active_context();
        let actor = crate::vm_types::def::Actor { context, fields };
        match id {
            ActorId::Alias(_x) => {
                /*
                active
                    .defs()
                    .reinsert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
                */
            }
            ActorId::Local(x) => {
                active
                    .defs()
                    .reinsert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
            }
        }
        active.upgrade(path, id.clone(), actor)
    }

    pub fn module<A: Active>(
        active: &mut A,
        path: ModulePath,
        id: &Option<Id_>,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        dfs: &DecFields,
        // Some(_) means the module is being upgraded.
        ctx_id: Option<CtxId>,
    ) -> Result<Value_, Interruption> {
        if let Some(ctx_id) = ctx_id {
            let (saved, fields, old_ctx) = active.defs().reenter_context(&ctx_id);
            for df in dfs.vec.iter() {
                insert_static_field(active, &df.1, &df.0)?;
            }
            active.defs().releave_context(saved, &fields, &old_ctx);
            let context = active.defs().active_context();
            let module = crate::vm_types::def::Module { context, fields };
            match id {
                None => (),
                Some(x) => active.defs().reinsert_field(
                    &x.0,
                    source,
                    vis,
                    stab,
                    Def::Module(module.clone()),
                )?,
            }
            active.upgrade_module(path, id.as_ref().map(|x| x.0.clone()), module)
        } else {
            let (parent, fields) = active.defs().enter_context(false);
            for df in dfs.vec.iter() {
                insert_static_field(active, &df.1, &df.0)?;
            }
            active.defs().leave_context(parent, &fields);
            let context = active.defs().active_context();
            let module = crate::vm_types::def::Module { context, fields };
            /*
                    active
                        .defs()
                        .insert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
            */
            active.create_module(path, id.as_ref().map(|x| x.0.clone()), module)
        }
    }
}

fn exp_field_is_static(e: &ExpField) -> bool {
    if let Some(e) = &e.exp {
        exp_is_static(&e.0)
    } else {
        true
    }
}

fn object_is_static(d: &crate::ast::Delim<ExpField_>) -> bool {
    for ef in d.vec.iter() {
        if !exp_field_is_static(&ef.0) {
            return false;
        }
    }
    true
}

fn delim_is_static(d: &crate::ast::Delim<Exp_>) -> bool {
    for e in d.vec.iter() {
        if !exp_is_static(&e.0) {
            return false;
        }
    }
    true
}

// Conservative check if an expression is "static enough".
// No allocation, looping is permitted in a static expression.
// (So no Mut::Var fields, and no objects, etc.)
fn exp_is_static(e: &Exp) -> bool {
    match e {
        Exp::ActorUrl(_) => true,
        Exp::Object(_, efs) => {
            if let Some(efs) = efs {
                object_is_static(efs)
            } else {
                true
            }
        }
        Exp::Literal(_) => true,
        Exp::Function(_) => true,
        Exp::Block(decs) => {
            if decs.vec.len() > 1 {
                return false;
            };
            decs.vec.iter().all(|d| dec_is_static(&d.0))
        }
        Exp::Var(_) => true,  // variables reference _values_ in the env.
        Exp::Prim(_) => true, // primitives are constant/built-in.
        Exp::Un(_, e1) => exp_is_static(&e1.0),
        Exp::Bin(e1, _, e2) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::Rel(e1, _, e2) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::Opt(e) => exp_is_static(&e.0),
        Exp::Variant(_, None) => true,
        Exp::Variant(_, Some(e)) => exp_is_static(&e.0),
        Exp::Array(Mut::Const, d) => delim_is_static(d),
        Exp::Tuple(d) => delim_is_static(d),
        Exp::Not(e) => exp_is_static(&e.0),
        Exp::And(e1, e2) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::Or(e1, e2) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::If(e1, e2, None) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::If(e1, e2, Some(e3)) => {
            exp_is_static(&e1.0) & exp_is_static(&e2.0) & exp_is_static(&e3.0)
        }
        // Switch -- to do
        Exp::Ignore(e) => exp_is_static(&e.0),
        Exp::Annot(_, e, _) => exp_is_static(&e.0),
        Exp::Paren(e) => exp_is_static(&e.0),
        Exp::Dot(e1, _e2) => exp_is_static(&e1.0) & exp_is_static(&e1.0),
        _ => {
            log::warn!("Safety check failed: exp_is_static({:?})", e);
            false
        }
    }
}

fn dec_is_static(d: &Dec) -> bool {
    match d {
        Dec::Exp(e) => exp_is_static(&e.0),
        Dec::Func(_) => true,
        _ => todo!("dec_is_static({:?})", d),
    }
}

fn agent_init(prog: Prog) -> Agent {
    let mut a = Agent {
        store: Store::new(ScheduleChoice::Agent),
        //debug_print_out: Vector::new(),
        counts: Counts::default(),
        active: Activation::new(),
    };
    a.active.cont = Cont::Decs(prog.vec);
    a
}

fn unop(un: UnOp, v: Value_) -> Result<Value, Interruption> {
    match (un, &*v) {
        (UnOp::Neg, Value::Nat(n)) => Ok(Value::Int(-n.to_bigint().unwrap())),
        _ => nyi!(line!()),
    }
}

fn binop(
    cont_prim_type: &Option<PrimType>,
    binop: BinOp,
    v1: Value_,
    v2: Value_,
) -> Result<Value, Interruption> {
    use BinOp::*;
    use Value::*;
    if let Unit = &*v1 {
        type_mismatch!(file!(), line!());
    };
    if let Unit = &*v2 {
        type_mismatch!(file!(), line!());
    };
    match binop {
        Add => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 + n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 + i2)),
            // _ => nyi!(line!()),
            (v1, v2) => unimplemented!("{:?} + {:?}", v1, v2),
        },
        Sub => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => {
                if n2 > n1 {
                    Ok(Int(n1.to_bigint().unwrap() - n2.to_bigint().unwrap()))
                } else {
                    Ok(Nat(n1 - n2))
                }
            }
            (Int(i1), Int(i2)) => Ok(Int(i1 - i2)),
            (Int(i1), Nat(n2)) => Ok(Int(i1 - n2.to_bigint().unwrap())),
            (Nat(n1), Int(i2)) => Ok(Int(n1.to_bigint().unwrap() - i2)),
            // _ => nyi!(line!()),
            (v1, v2) => unimplemented!("{:?} - {:?}", v1, v2),
        },
        Mul => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 * n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 * i2)),
            // _ => nyi!(line!()),
            (v1, v2) => unimplemented!("{:?} * {:?}", v1, v2),
        },
        WAdd => match (cont_prim_type, &*v1, &*v2) {
            (None, _, _) => Err(Interruption::AmbiguousOperation),
            (Some(t), Value::Nat(n1), Value::Nat(n2)) => match t {
                PrimType::Nat => Ok(Value::Nat(n1 + n2)),
                PrimType::Nat8 => Ok(Value::Nat(
                    (n1 + n2) % BigUint::parse_bytes(b"256", 10).unwrap(),
                )),
                _ => nyi!(line!()),
            },
            _ => nyi!(line!()),
        },
        _ => nyi!(line!(), "binop({:?}. {:?}, {:?})", binop, v1, v2),
    }
}

fn relop(
    _cont_prim_xotype: &Option<PrimType>,
    relop: RelOp,
    v1: Value_,
    v2: Value_,
) -> Result<Value, Interruption> {
    use RelOp::*;
    use Value::*;
    Ok(Bool(match relop {
        Eq => match (&*v1, &*v2) {
            (Unit, Unit) => true,
            (Bool(b1), Bool(b2)) => b1 == b2,
            (Text(t1), Text(t2)) => t1 == t2,
            (Nat(n1), Nat(n2)) => n1 == n2,
            (Int(i1), Int(i2)) => i1 == i2,
            (v1, v2) => v1 == v2, //            _ => nyi!(line!(), "{:?} == {:?}", v1, v2)?,
        },
        Neq => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Bool(b1), Bool(b2)) => b1 != b2,
            (Text(t1), Text(t2)) => t1 == t2,
            (Nat(n1), Nat(n2)) => n1 != n2,
            (Int(i1), Int(i2)) => i1 != i2,
            (v1, v2) => v1 != v2, //            _ => nyi!(line!(), "{:?} == {:?}", v1, v2)?,
        },
        _ => nyi!(line!(), "relop({:?}, {:?}, {:?})", relop, v1, v2)?,
    }))
}

fn exp_conts_<A: Active>(
    active: &mut A,
    source: Source,
    frame_cont: FrameCont,
    cont: Cont,
    cont_source: Source,
) -> Result<Step, Interruption> {
    let env = active.env().fast_clone();
    let cont_prim_type = active.cont_prim_type().clone();
    let context = active.defs().active_ctx.clone();
    active.stack().push_front(Frame {
        context,
        env,
        cont: frame_cont,
        cont_prim_type,
        source,
    });
    *active.cont() = cont;
    *active.cont_source() = cont_source;
    Ok(Step {})
}

/* continuation separates into stack frame cont and immediate cont. */
fn exp_conts<A: Active>(
    active: &mut A,
    frame_cont: FrameCont,
    cont: &Exp_,
) -> Result<Step, Interruption> {
    let cont_source = cont.1.clone();
    exp_conts_(
        active,
        cont_source.clone(),
        frame_cont,
        Cont::Exp_(cont.fast_clone(), Vector::new()),
        cont_source,
    )
}

/* continuation uses same stack frame. */
fn exp_cont<A: Active>(active: &mut A, cont: &Exp_) -> Result<Step, Interruption> {
    *active.cont_source() = cont.1.clone();
    *active.cont() = Cont::Exp_(cont.fast_clone(), Vector::new());
    Ok(Step {})
}

fn string_from_value(v: &Value) -> Result<String, Interruption> {
    //Ok(crate::format::format_one_line(v))
    Ok(format!("{:?}", v))
}

fn opaque_iter_next<A: Active>(
    active: &mut A,
    p: &Pointer,
) -> Result<Option<Value_>, Interruption> {
    use crate::value::Collection;
    let iter_value = active.deref(p)?;
    // dispatch based on iterator value (as opposed to primitive function being given in source).
    // one case for each inbuilt iterator value.
    // to do -- integrate "dynamic" iterators too
    match &*iter_value {
        Value::Collection(Collection::FastRandIter(fri)) => {
            let mut fri = fri.clone();
            let n = fri.next();
            active.store().mutate(
                p.clone(),
                Value::Collection(Collection::FastRandIter(fri)).share(),
            )?;
            Ok(n.map(|v| v.share()))
        }
        _ => type_mismatch!(file!(), line!()),
    }
}

fn cont_for_call_dot_next<A: Active>(
    active: &mut A,
    p: Pat_,
    v: Value_,
    body: Exp_,
) -> Result<Step, Interruption> {
    let deref_v = active.deref_value(v.fast_clone())?; // Only used for `Dynamic` case
    match &*deref_v {
        Value::Dynamic(d) => {
            let env = active.env().fast_clone();
            let source = active.cont_source().clone();
            let context = active.defs().active_ctx.clone();
            active.stack().push_front(Frame {
                context,
                env,
                cont: FrameCont::For2(p, v, body),
                cont_prim_type: None,
                source,
            });
            *active.cont() = Cont::Value_(d.dynamic_mut().iter_next(active.store())?);
            Ok(Step {})
        }
        _ => {
            let v_next_func = v.get_field_or("next", type_mismatch_!(file!(), line!()))?;
            let env = active.env().fast_clone();
            let source = active.cont_source().clone();
            let context = active.defs().active_ctx.clone();
            active.stack().push_front(Frame {
                context,
                env,
                cont: FrameCont::For2(p, v, body),
                cont_prim_type: None,
                source,
            });
            call_cont(active, v_next_func, None, Value::Unit.share())
        }
    }
}

fn call_prim_function<A: Active>(
    active: &mut A,
    pf: &PrimFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use PrimFunction::*;
    match pf {
        AtSignVar(v) => nyi!(line!(), "call_prim_function({})", v),
        DebugPrint => match &*args {
            Value::Text(s) => {
                let schedule_choice = active.schedule_choice().clone();
                log::info!(
                    "DebugPrint: {:?}, {}: {:?}",
                    schedule_choice,
                    active.cont_source(),
                    s
                );

                active.debug_print_out().push_back(DebugPrintLine {
                    text: s.clone(),
                    schedule_choice,
                });
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            v => {
                let txt = string_from_value(v)?;
                let schedule_choice = active.schedule_choice().clone();
                log::info!(
                    "DebugPrint: {:?}: {}: {:?}",
                    schedule_choice,
                    active.cont_source(),
                    txt
                );
                let schedule_choice = active.schedule_choice().clone();
                active.debug_print_out().push_back(DebugPrintLine {
                    text: crate::value::Text::from(txt),
                    schedule_choice,
                });
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
        },
        NatToText => match &*args {
            Value::Nat(n) => {
                *active.cont() = cont_value(Value::Text(format!("{}", n).into()));
                Ok(Step {})
            }
            v => {
                *active.cont() = cont_value(Value::Text(format!("{:?}", v).into()));
                Ok(Step {})
            }
        },
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "value-reflection")]
        ReifyValue => {
            use crate::value::ToMotoko;
            *active.cont() = cont_value(args.to_motoko().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "value-reflection")]
        ReflectValue => {
            // active.cont = cont_value(args.to_rust::<Value>().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "active-reflection")]
        ReifyActive => {
            use crate::value::ToMotoko;
            *active.cont() = cont_value(active.to_motoko().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "active-reflection")]
        ReflectActive => {
            *active = args.to_rust::<Active>().map_err(Interruption::ValueError)?;
            Ok(Step {})
        }
        Collection(cf) => call_collection_function(active, cf, targs, args),
    }
}

fn call_collection_function<A: Active>(
    active: &mut A,
    cf: &CollectionFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use CollectionFunction::*;
    match cf {
        HashMap(hmf) => call_hashmap_function(active, hmf, targs, args),
        FastRandIter(frif) => call_fastranditer_function(active, frif, targs, args),
    }
}

fn call_fastranditer_function<A: Active>(
    active: &mut A,
    frif: &FastRandIterFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use FastRandIterFunction::*;
    match frif {
        New => collection::fastranditer::new(active, targs, args),
        Next => collection::fastranditer::next(active, args),
    }
}

fn call_hashmap_function<A: Active>(
    active: &mut A,
    hmf: &HashMapFunction,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use HashMapFunction::*;
    match hmf {
        New => collection::hashmap::new(active, args),
        Put => collection::hashmap::put(active, args),
        Get => collection::hashmap::get(active, args),
        Remove => collection::hashmap::remove(active, args),
    }
}

fn call_function_def<A: Active>(
    active: &mut A,
    actor_env: Env,
    fndef: &FunctionDef,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    if let Some(env_) = pattern_matches(actor_env, &fndef.function.input.0, args) {
        let source = active.cont_source().clone();
        let env_saved = active.env().fast_clone();
        *active.env() = env_;
        fndef.function.name.fast_clone().map(|f| {
            active
                .env()
                .insert(f.0.clone(), fndef.rec_value.fast_clone())
        });
        active.defs().active_ctx = fndef.context.clone();
        *active.cont() = Cont::Exp_(fndef.function.exp.fast_clone(), Vector::new());
        let context = active.defs().active_ctx.clone();
        active.stack().push_front(Frame {
            context,
            source,
            env: env_saved,
            cont: FrameCont::Call3,
            cont_prim_type: None, /* to do */
        }); // to match with Return, if any.
        Ok(Step {})
    } else {
        type_mismatch!(file!(), line!())
    }
}

fn call_function<A: Active>(
    active: &mut A,
    value: Value_,
    cf: &ClosedFunction,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    if let Some(env_) = pattern_matches(cf.0.env.fast_clone(), &cf.0.content.input.0, args) {
        let source = active.cont_source().clone();
        let env_saved = active.env().fast_clone();
        *active.env() = env_;
        cf.0.content
            .name
            .fast_clone()
            .map(|f| active.env().insert(f.0.clone(), value));
        *active.cont() = Cont::Exp_(cf.0.content.exp.fast_clone(), Vector::new());
        let context = active.defs().active_ctx.clone();
        active.defs().active_ctx = cf.0.ctx.clone();
        active.stack().push_front(Frame {
            context,
            source,
            env: env_saved,
            cont: FrameCont::Call3,
            cont_prim_type: None, /* to do */
        }); // to match with Return, if any.
        Ok(Step {})
    } else {
        type_mismatch!(file!(), line!())
    }
}

fn call_cont<A: Active>(
    active: &mut A,
    func_value: Value_,
    inst: Option<Inst>,
    args_value: Value_,
) -> Result<Step, Interruption> {
    match &*func_value {
        Value::Function(cf) => call_function(active, func_value.fast_clone(), cf, inst, args_value),
        Value::PrimFunction(pf) => call_prim_function(active, pf, inst, args_value),
        _ => {
            let func_value = active.deref_value(func_value)?; // Account for dynamic value pointers
            match &*func_value {
                Value::Dynamic(d) => {
                    let result =
                        d.dynamic_mut()
                            .call(active.store(), &inst, args_value.fast_clone())?;
                    *active.cont() = Cont::Value_(result);
                    Ok(Step {})
                }
                Value::ActorMethod(am) => Err(Interruption::Send(am.clone(), inst, args_value)),
                _ => type_mismatch!(file!(), line!()),
            }
        }
    }
}

mod pattern {
    use super::*;
    use crate::ast::{Delim, NodeData};

    pub fn temps(num: u16) -> Pat {
        let mut vars = vec![];
        for i in 0..num {
            vars.push(NodeData::new(Pat::TempVar(i as u16), Source::Evaluation).share())
        }
        Pat::Tuple(Delim::from(vars))
    }
}

fn assert_value_is_optional<'a>(v: &'a Value) -> Result<Option<&'a Value_>, Interruption> {
    match v {
        Value::Option(v) => Ok(Some(v)),
        Value::Null => Ok(None),
        _ => type_mismatch!(file!(), line!()),
    }
}

fn assert_value_is_u32<'a>(v: &'a Value) -> Result<u32, Interruption> {
    v.to_rust().map_err(Interruption::ValueError)
}

fn assert_value_is_option_u32<'a>(v: &'a Value) -> Result<Option<u32>, Interruption> {
    match assert_value_is_optional(v)? {
        None => Ok(None),
        Some(v) => Ok(Some(assert_value_is_u32(v)?)),
    }
}

fn assert_value_is_opaque_pointer(v: &Value) -> Result<Pointer, Interruption> {
    match v {
        Value::Opaque(p) => Ok(p.clone()),
        _ => type_mismatch!(file!(), line!()),
    }
}

mod collection {
    pub mod fastranditer {
        use super::super::*;
        use crate::{shared::Share, value::Collection};

        pub fn new<A: Active>(
            active: &mut A,
            _targs: Option<Inst>,
            v: Value_,
        ) -> Result<Step, Interruption> {
            if let Some(args) = pattern_matches_temps(&pattern::temps(2), v) {
                let size: Option<u32> = assert_value_is_option_u32(&args[0])?;
                let seed: u32 = assert_value_is_u32(&args[1])?;
                let ptr = active.alloc(
                    Value::Collection(Collection::FastRandIter(FastRandIter::new(size, seed)))
                        .share(),
                );
                *active.cont() = cont_value(Value::Opaque(ptr));
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }

        pub fn next<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            let ptr = assert_value_is_opaque_pointer(&v)?;
            match &*active.deref(&ptr)? {
                Value::Collection(Collection::FastRandIter(fri)) => {
                    let mut fri = fri.clone();
                    let n = match fri.next() {
                        Some(n) => Value::Option(n.share()),
                        None => Value::Null,
                    };
                    let i = Value::Collection(Collection::FastRandIter(fri));
                    active.store().mutate(ptr, i.share())?;
                    *active.cont() = cont_value(n);
                    Ok(Step {})
                }
                _ => type_mismatch!(file!(), line!()),
            }
        }
    }

    pub mod hashmap {
        use super::super::*;
        use crate::value::Collection;
        use im_rc::vector;

        pub fn new<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(_) = pattern_matches_temps(&Pat::Literal(Literal::Unit), v) {
                *active.cont() = cont_value(Value::Collection(Collection::HashMap(HashMap::new())));
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn put<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) = pattern_matches_temps(&pattern::temps(3), v) {
                let hm = &args[0];
                let k = &args[1];
                let v = &args[2];
                let (hm, old) = {
                    if let Value::Collection(Collection::HashMap(mut hm)) = hm.get() {
                        match hm.insert(k.fast_clone(), v.fast_clone()) {
                            None => (hm, Value::Null),
                            Some(old) => (hm, Value::Option(old)),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                // Note for later: immutable map updates are adding extra overhead here
                // We could probably just tolerate this and use `Dynamic` values for performance-critical situations
                let hm = Value::Collection(Collection::HashMap(hm));
                let ret = Value::Tuple(vector![hm.share(), old.share()]);
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn get<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) = pattern_matches_temps(&pattern::temps(2), v) {
                let hm = &args[0];
                let k = &args[1];
                let ret = {
                    if let Value::Collection(Collection::HashMap(hm)) = hm.get() {
                        match hm.get(k) {
                            None => Value::Null,
                            Some(v) => Value::Option(v.fast_clone()),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn remove<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) = pattern_matches_temps(&pattern::temps(2), v) {
                let hm = &args[0];
                let k = &args[1];
                let (hm, old) = {
                    if let Value::Collection(Collection::HashMap(mut hm)) = hm.get() {
                        match hm.remove(k) {
                            None => (hm, Value::Null),
                            Some(v) => (hm, Value::Option(v)),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                let hm = Value::Collection(Collection::HashMap(hm));
                let ret = Value::Tuple(vector![hm.share(), old.share()]);
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
    }
}

fn def_as_value(defs: &Defs, i: &Id, def: &Def) -> Result<Value_, Interruption> {
    match def {
        Def::Actor(a) => Ok(Value::Actor(crate::value::Actor {
            def: Some(a.clone()),
            id: ActorId::Local(i.clone()),
        })
        .share()),
        Def::Func(f) => Ok(f.rec_value.fast_clone()),
        Def::StaticValue(ctx_id, e) => {
            let mut c = Core::empty();
            c.defs = defs.clone();
            c.defs.active_ctx = ctx_id.clone();
            c.agent.active.cont = Cont::Exp_(e.fast_clone(), Vector::new());
            let v = c.run(&Limits::none())?;
            Ok(v)
        }
        Def::Var(v) => Ok(crate::value::Value::Pointer(Pointer {
            owner: v.owner.clone(),
            local: LocalPointer::Named(NamedPointer(v.name.clone())),
        })
        .share()),
        Def::Module(m) => Ok(Value::Module(m.clone()).share()),
    }
}

fn resolve_def(
    defs: &Defs,
    ctx_id: &CtxId,
    is_public_projection: bool,
    x: &Id,
) -> Result<FieldDef, Interruption> {
    let ctx = defs.map.get(&ctx_id).unwrap();
    match ctx.fields.get(x) {
        Some(d) => {
            let f_is_public = match &d.vis {
                Some(x) => x.0.is_public(),
                None => false,
            };
            if is_public_projection && !f_is_public {
                return Err(Interruption::ModuleFieldNotPublic(x.clone()));
            };
            Ok(d.clone())
        }
        None => {
            if is_public_projection {
                Err(Interruption::UnboundIdentifer(x.clone()))
            } else {
                match &ctx.parent {
                    Some(p) => resolve_def(defs, p, false, x),
                    None => Err(Interruption::UnboundIdentifer(x.clone())),
                }
            }
        }
    }
}

fn exp_step<A: Active>(active: &mut A, exp: Exp_) -> Result<Step, Interruption> {
    use Exp::*;
    let source = exp.1.clone();
    match &exp.0 {
        Literal(l) => {
            // TODO: partial evaluation would now be highly efficient due to value sharing
            *active.cont() = cont_value(Value::from_literal(l).map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        Function(f) => {
            let env = active.env().fast_clone();
            *active.cont() = cont_value(Value::Function(ClosedFunction(Closed {
                ctx: active.defs().active_ctx.clone(),
                env,
                content: f.clone(), // TODO: `Shared<Function>`?
            })));
            Ok(Step {})
        }
        Call(e1, inst, e2) => {
            exp_conts(active, FrameCont::Call1(inst.clone(), e2.fast_clone()), e1)
        }
        Return(None) => return_(active, Value::Unit.share()),
        Return(Some(e)) => exp_conts(active, FrameCont::Return, e),
        Var(x) => match active.env().get(x) {
            None => {
                if x.string.starts_with("@") {
                    let f = crate::value::PrimFunction::AtSignVar(x.to_string());
                    let v = Value::PrimFunction(f).share();
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                } else {
                    let ctx = active.defs().active_ctx.clone();
                    let fd = resolve_def(active.defs(), &ctx, false, x)?;
                    let v = def_as_value(active.defs(), x, &fd.def)?;
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                }
            }
            Some(v) => {
                *active.cont() = Cont::Value_(v.fast_clone());
                Ok(Step {})
            }
        },
        Bin(e1, binop, e2) => exp_conts(
            active,
            FrameCont::BinOp1(binop.clone(), e2.fast_clone()),
            e1,
        ),
        Un(un, e) => exp_conts(active, FrameCont::UnOp(un.clone()), e),
        Paren(e) => exp_conts(active, FrameCont::Paren, e),
        Variant(id, None) => {
            // TODO: cache and share variants?
            *active.cont() = cont_value(Value::Variant(id.0.clone(), None));
            Ok(Step {})
        }
        Variant(id, Some(e)) => exp_conts(active, FrameCont::Variant(id.fast_clone()), e),
        Switch(e1, cases) => exp_conts(active, FrameCont::Switch(cases.clone()), e1),
        Block(decs) => exp_conts_(
            active,
            source.clone(),
            FrameCont::Block,
            Cont::Decs(decs.vec.clone()),
            source,
        ),
        Do(e) => exp_conts(active, FrameCont::Do, e),
        Assert(e) => exp_conts(active, FrameCont::Assert, e),
        Object(bases, fields) => {
            if let Some(_bases) = bases {
                return nyi!(line!());
            };
            if let Some(fields) = fields {
                let mut fs: Vector<_> = fields.vec.fast_clone();
                match fs.pop_front() {
                    None => {
                        *active.cont() = cont_value(Value::Object(HashMap::new()));
                        Ok(Step {})
                    }
                    Some(f1) => {
                        let fc = FieldContext {
                            mut_: f1.0.mut_.clone(),
                            id: f1.0.id.fast_clone(),
                            typ: f1.0.typ.fast_clone(),
                        };
                        exp_conts(
                            active,
                            FrameCont::Object(Vector::new(), fc, fs),
                            &f1.0.exp_(),
                        )
                    }
                }
            } else {
                *active.cont() = cont_value(Value::Object(HashMap::new()));
                Ok(Step {})
            }
        }
        Tuple(es) => {
            let mut es: Vector<_> = es.vec.fast_clone();
            match es.pop_front() {
                None => {
                    // TODO: globally share (), true, false, null, etc.
                    *active.cont() = cont_value(Value::Unit);
                    Ok(Step {})
                }
                Some(e1) => exp_conts(active, FrameCont::Tuple(Vector::new(), es), &e1),
            }
        }
        Array(mut_, es) => {
            let mut es: Vector<_> = es.vec.fast_clone();
            match es.pop_front() {
                None => {
                    *active.cont() = cont_value(Value::Array(mut_.clone(), Vector::new()));
                    Ok(Step {})
                }
                Some(e1) => exp_conts(
                    active,
                    FrameCont::Array(mut_.clone(), Vector::new(), es),
                    &e1,
                ),
            }
        }
        Index(e1, e2) => exp_conts(active, FrameCont::Idx1(e2.fast_clone()), e1),
        Annot(_, e, t) => {
            match &t.0 {
                Type::Prim(pt) => *active.cont_prim_type() = Some(pt.clone()),
                _ => {}
            };
            exp_conts(active, FrameCont::Annot(t.fast_clone()), e)
        }
        Assign(e1, e2) => exp_conts(active, FrameCont::Assign1(e2.fast_clone()), e1),
        BinAssign(e1, b, e2) => exp_conts(
            active,
            FrameCont::BinAssign1(b.clone(), e2.fast_clone()),
            e1,
        ),
        Proj(e1, i) => exp_conts(active, FrameCont::Proj(i.clone()), e1),
        Dot(e1, f) => exp_conts(active, FrameCont::Dot(f.fast_clone()), e1),
        If(e1, e2, e3) => exp_conts(active, FrameCont::If(e2.fast_clone(), e3.fast_clone()), e1),
        Rel(e1, relop, e2) => exp_conts(
            active,
            FrameCont::RelOp1(relop.clone(), e2.fast_clone()),
            e1,
        ),
        While(e1, e2) => exp_conts(
            active,
            FrameCont::While1(e1.fast_clone(), e2.fast_clone()),
            e1,
        ),
        For(p, e1, e2) => exp_conts(active, FrameCont::For1(p.fast_clone(), e2.fast_clone()), e1),
        And(e1, e2) => exp_conts(active, FrameCont::And1(e2.fast_clone()), e1),
        Or(e1, e2) => exp_conts(active, FrameCont::Or1(e2.fast_clone()), e1),
        Not(e) => exp_conts(active, FrameCont::Not, e),
        Opt(e) => exp_conts(active, FrameCont::Opt, e),
        DoOpt(e) => exp_conts(active, FrameCont::DoOpt, e),
        Bang(e) => exp_conts(active, FrameCont::Bang, e),
        Ignore(e) => exp_conts(active, FrameCont::Ignore, e),
        Debug(e) => exp_conts(active, FrameCont::Debug, e),
        Prim(p) => {
            *active.cont() = cont_value(Value::PrimFunction(
                p.clone()
                    .map_err(|s| Interruption::UnrecognizedPrim(s.to_string()))?,
            ));
            Ok(Step {})
        }
        e => nyi!(line!(), "{:?}", e),
    }
}

fn pattern_matches_temps(pat: &Pat, v: Value_) -> Option<Vec<Value_>> {
    pattern_matches_temps_(pat, v, vec![])
}

// TODO: see whether it's possible to return something like `&'a Option<Env>` to reduce cloning
// (since this has more of a performance impact than `fast_clone()`)
fn pattern_matches_temps_(pat: &Pat, v: Value_, mut out: Vec<Value_>) -> Option<Vec<Value_>> {
    match (pat, &*v) {
        (Pat::Wild, _) => Some(out),
        (Pat::Annot(_), _) => Some(out),
        (Pat::Literal(Literal::Unit), Value::Unit) => Some(out),
        (Pat::Paren(p), _) => pattern_matches_temps_(&p.0, v, out),
        (Pat::AnnotPat(p, _), _) => pattern_matches_temps_(&p.0, v, out),
        (Pat::Var(_x), _) => {
            unreachable!()
        }
        (Pat::TempVar(n), _) => {
            assert_eq!(out.len() as u16, *n);
            out.push(v.fast_clone());
            Some(out)
        }
        (Pat::Variant(id1, None), Value::Variant(id2, None)) => {
            if &id1.0 != id2 {
                return None;
            };
            Some(out)
        }
        (Pat::Variant(id1, Some(pat_)), Value::Variant(id2, Some(v_))) => {
            if &id1.0 != id2 {
                return None;
            };
            pattern_matches_temps_(&pat_.0, v_.fast_clone(), out)
        }
        (Pat::Tuple(ps), Value::Tuple(vs)) => {
            if ps.vec.len() != vs.len() {
                None
            } else {
                let mut out = out;
                for i in 0..ps.vec.len() {
                    if let Some(out_) = pattern_matches_temps_(
                        &ps.vec.get(i).unwrap().0,
                        vs.get(i).unwrap().fast_clone(),
                        out,
                    ) {
                        out = out_
                    } else {
                        return None;
                    }
                }
                Some(out)
            }
        }
        _ => None,
    }
}

fn get_pat_var(p: &Pat) -> Option<Id_> {
    match p {
        Pat::Var(x) => Some(x.clone()),
        Pat::AnnotPat(p, _) => get_pat_var(&p.0),
        Pat::Paren(p) => get_pat_var(&p.0),
        _ => None,
    }
}

// TODO: see whether it's possible to return something like `&'a Option<Env>` to reduce cloning
// (since this has more of a performance impact than `fast_clone()`)
fn pattern_matches(env: Env, pat: &Pat, v: Value_) -> Option<Env> {
    match (pat, &*v) {
        (Pat::Wild, _) => Some(env),
        (Pat::Literal(Literal::Unit), Value::Unit) => Some(env),
        (Pat::Paren(p), _) => pattern_matches(env, &p.0, v),
        (Pat::Annot(_), _) => Some(env),
        (Pat::AnnotPat(p, _), _) => pattern_matches(env, &p.0, v),
        (Pat::Var(x), _) => {
            let mut env = env;
            env.insert(x.0.clone(), v);
            Some(env)
        }
        (Pat::Variant(id1, None), Value::Variant(id2, None)) => {
            if &id1.0 != id2 {
                return None;
            };
            Some(env)
        }
        (Pat::Variant(id1, Some(pat_)), Value::Variant(id2, Some(v_))) => {
            if &id1.0 != id2 {
                return None;
            };
            pattern_matches(env, &pat_.0, v_.fast_clone())
        }
        (Pat::Tuple(ps), Value::Tuple(vs)) => {
            if ps.vec.len() != vs.len() {
                None
            } else {
                let mut env = env;
                for i in 0..ps.vec.len() {
                    if let Some(env_) = pattern_matches(
                        env,
                        &ps.vec.get(i).unwrap().0,
                        vs.get(i).unwrap().fast_clone(),
                    ) {
                        env = env_
                    } else {
                        return None;
                    }
                }
                Some(env)
            }
        }
        _ => None,
    }
}

fn switch<A: Active>(active: &mut A, v: Value_, cases: Cases) -> Result<Step, Interruption> {
    for case in cases.vec.into_iter() {
        if let Some(env) = pattern_matches(active.env().fast_clone(), &case.0.pat.0, v.fast_clone())
        {
            *active.env() = env;
            *active.cont_source() = case.0.exp.1.clone();
            *active.cont() = Cont::Exp_(case.0.exp.fast_clone(), Vector::new());
            return Ok(Step {});
        }
    }
    Err(Interruption::NoMatchingCase)
}

fn bang_null<A: Active>(active: &mut A) -> Result<Step, Interruption> {
    let mut stack = active.stack().clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::DoOpt => {
                    *active.stack() = stack;
                    *active.cont() = cont_value(Value::Null);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::NoDoQuestBangNull);
        }
    }
}

fn return_<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    let mut stack = active.stack().fast_clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::Call3 => {
                    active.defs().active_ctx = fr.context;
                    *active.env() = fr.env;
                    *active.stack() = stack;
                    *active.cont() = Cont::Value_(v);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::MisplacedReturn);
        }
    }
}

fn source_from_decs(decs: &Vector<Dec_>) -> Source {
    if decs.is_empty() {
        Source::Unknown
    } else {
        let first = decs.front().unwrap().1.clone();
        match decs.back() {
            None => first,
            Some(back) => first.expand(&back.1),
        }
    }
}

fn source_from_cont(cont: &Cont) -> Source {
    use Cont::*;
    match cont {
        Taken => {
            unreachable!("no source for Taken continuation. This signals a VM bug.  Please report.")
        }
        Decs(decs) => source_from_decs(decs),
        Exp_(exp_, decs) => {
            if decs.is_empty() {
                exp_.1.clone()
            } else {
                exp_.1.expand(&decs.back().unwrap().1)
            }
        }
        LetVarRet(s, _) => s.clone(),
        Value_(_v) => Source::Evaluation,
    }
}

#[inline]
fn usize_from_biguint(n: &BigUint) -> Result<usize, Interruption> {
    n.to_usize()
        .ok_or(Interruption::ValueError(ValueError::BigInt))
}

fn stack_cont_has_redex<A: ActiveBorrow>(active: &A, v: &Value) -> Result<bool, Interruption> {
    if active.stack().is_empty() {
        Ok(false)
    } else {
        use FrameCont::*;
        let frame = active.stack().front().unwrap();
        let r = match &frame.cont {
            Respond(_) => true,
            UnOp(_) => true,
            RelOp1(_, _) => false,
            RelOp2(_, _) => true,
            BinOp1(_, _) => false,
            BinOp2(_, _) => true,
            Assign1(_) => false,
            Assign2(_) => true,
            BinAssign1(..) => false,
            BinAssign2(..) => true,
            Idx1(_) => false,
            Idx2(_) => true,
            Let(_, _) => true,
            Var(_, _) => true,
            Paren => false,
            Variant(_) => false,
            Switch(_) => true,
            Block => false,
            Decs(_) => false,
            Do => true,
            Assert => true,
            Ignore => true,
            Tuple(_, _) => false,
            Array(..) => false,
            Object(..) => false,
            Annot(..) => false,
            Proj(..) => true,
            Dot(..) => true,
            Debug => false,
            If(_, _) => true,
            While1(_, _) => true,
            While2(_, _) => false,
            For1(_, _) => false,
            For2(_, _, _) => true,
            For3(_, _, _) => false,
            ForOpaqueIter(_, _, _) => true,
            And1(_) => false,
            And2 => true,
            Or1(_) => match v {
                Value::Bool(b) => *b,
                _ => true,
            },
            Or2 => true,
            Not => true,
            Opt => false,
            DoOpt => false,
            Bang => true,
            Call1(..) => false,
            Call2(..) => true,
            Call3 => false,
            Return => true,
            //_ => return nyi!(line!()),
        };
        Ok(r)
    }
}

// continue execution using the top-most stack frame, if any.
fn stack_cont<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    if active.stack().is_empty() {
        *active.cont() = Cont::Value_(v.fast_clone());
        Err(Interruption::Done(v))
    } else if let Some(&Frame {
        cont: FrameCont::ForOpaqueIter(ref pat, ref ptr, ref body),
        ..
    }) = active.stack().front()
    {
        let pat = pat.fast_clone();
        let ptr = ptr.fast_clone();
        let body = body.fast_clone();
        let env = active.stack().front().unwrap().env.fast_clone();
        /* fast-path: avoid popping top stack frame. */
        match &*v {
            Value::Unit => (),
            _ => type_mismatch!(file!(), line!()),
        };
        match opaque_iter_next(active, &ptr)? {
            None => {
                active.stack().pop_front();
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            Some(v_) => {
                if let Some(env) = pattern_matches(env, &pat.0, v_.fast_clone()) {
                    *active.env() = env;
                    exp_cont(active, &body)
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
        }
    } else {
        // common cases: need to pop top stack frame, then pattern-match it.
        nonempty_stack_cont(active, v)
    }
}

fn nonempty_stack_cont<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    use FrameCont::*;
    let frame = active.stack().pop_front().unwrap();
    match &frame.cont {
        Decs(_) => { /* decs in same block share an environment. */ }
        _ => {
            *active.env() = frame.env;
        }
    }
    active.defs().active_ctx = frame.context;
    *active.cont_prim_type() = frame.cont_prim_type;
    *active.cont_source() = frame.source;
    match frame.cont {
        ForOpaqueIter(..) => unreachable!(),
        Respond(target) => Err(Interruption::Response(Response { target, value: v })),
        UnOp(un) => {
            *active.cont() = cont_value(unop(un, v)?);
            Ok(Step {})
        }
        RelOp1(relop, e2) => exp_conts(active, RelOp2(v, relop), &e2),
        RelOp2(v1, rel) => {
            let v = relop(&active.cont_prim_type(), rel, v1, v)?;
            *active.cont() = cont_value(v);
            Ok(Step {})
        }
        BinOp1(binop, e2) => exp_conts(active, BinOp2(v, binop), &e2),
        BinOp2(v1, bop) => {
            let v = binop(&active.cont_prim_type(), bop, v1, v)?;
            *active.cont() = cont_value(v);
            Ok(Step {})
        }
        Assign1(e2) => exp_conts(active, Assign2(v), &e2),
        BinAssign1(b, e2) => exp_conts(active, BinAssign2(v, b), &e2),
        Assign2(v1) => match &*v1 {
            Value::Pointer(p) => {
                active.store().mutate(p.clone(), v)?;
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            Value::Index(p, i) => {
                active.store().mutate_index(p.clone(), i.fast_clone(), v)?;
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        BinAssign2(v1, bop) => {
            let v1d = match &*v1 {
                Value::Pointer(p) => active.deref(p)?,
                x => {
                    return nyi!(
                        line!(),
                        "BinAssign2: expected Value::Pointer, but got {:?}",
                        x
                    )
                }
            };
            let v3 = binop(&active.cont_prim_type(), bop, v1d.clone(), v.clone())?;
            match &*v1 {
                Value::Pointer(p) => {
                    active.store().mutate(p.clone(), v3.share())?;
                    *active.cont() = cont_value(Value::Unit);
                    Ok(Step {})
                }
                _ => return nyi!(line!()),
            }
        }
        Idx1(e2) => exp_conts(active, Idx2(v), &e2),
        Idx2(v1) => {
            if let Some(Frame {
                cont: FrameCont::Assign1(_), // still need to evaluate RHS of assignment.
                ..
            }) = active.stack().get(0)
            {
                match &*v1 {
                    Value::Pointer(p) => {
                        // save array pointer and offset until after RHS is evaluated.
                        *active.cont() = cont_value(Value::Index(p.clone(), v.fast_clone()));
                        Ok(Step {})
                    }
                    Value::Dynamic(_) => Err(Interruption::Other(
                        "Dynamic Rust value without a pointer".to_string(),
                    )),
                    _ => type_mismatch!(file!(), line!()),
                }
            } else {
                let v1 = active.deref_value(v1)?;
                match (&*v1, &*v) {
                    (Value::Array(_mut, a), Value::Nat(i)) => {
                        let i = usize_from_biguint(i)?;
                        *active.cont() =
                            cont_value((**a.get(i).ok_or(Interruption::IndexOutOfBounds)?).clone());
                        Ok(Step {})
                    }
                    (Value::Dynamic(d), _) => {
                        *active.cont() =
                            cont_value((*d.dynamic().get_index(active.store(), v)?).clone());
                        Ok(Step {})
                    }
                    _ => type_mismatch!(file!(), line!()),
                }
            }
        }
        Let(p, cont) => {
            if let Some(env) = pattern_matches(active.env().clone(), p.as_ref().data_ref(), v) {
                *active.env() = env;
                *active.cont_source() = source_from_cont(&cont);
                *active.cont() = cont;
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        Var(x, cont) => {
            let ptr = active.alloc(v);
            active
                .env()
                .insert(x.as_ref().data_ref().clone(), Value::Pointer(ptr).share());
            *active.cont_source() = source_from_cont(&cont);
            *active.cont() = cont;
            Ok(Step {})
        }
        Paren => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Variant(i) => {
            *active.cont() = cont_value(Value::Variant(i.0.clone(), Some(v)));
            Ok(Step {})
        }
        Switch(cases) => switch(active, v, cases),
        Block => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Decs(decs) => {
            match decs.front() {
                None => {
                    // return final value from block.
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                }
                Some(_) => {
                    *active.cont() = Cont::Decs(decs);
                    Ok(Step {})
                }
            }
        }
        Do => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Assert => match &*v {
            Value::Bool(true) => {
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            Value::Bool(false) => Err(Interruption::AssertionFailure),
            _ => type_mismatch!(file!(), line!()),
        },
        Ignore => {
            *active.cont() = cont_value(Value::Unit);
            Ok(Step {})
        }
        Tuple(mut done, mut rest) => {
            done.push_back(v);
            match rest.pop_front() {
                None => {
                    *active.cont() = cont_value(Value::Tuple(done));
                    Ok(Step {})
                }
                Some(next) => exp_conts(active, Tuple(done, rest), &next),
            }
        }
        Array(mut_, mut done, mut rest) => {
            done.push_back(v);
            match rest.pop_front() {
                None => {
                    if let Mut::Const = mut_ {
                        *active.cont() = cont_value(Value::Array(mut_, done));
                        Ok(Step {})
                    } else {
                        let arr = Value::Array(mut_, done);
                        let ptr = active.alloc(arr.share());
                        *active.cont() = cont_value(Value::Pointer(ptr));
                        Ok(Step {})
                    }
                }
                Some(next) => exp_conts(active, Array(mut_, done, rest), &next),
            }
        }
        Object(mut done, ctx, mut rest) => {
            done.push_back(FieldValue {
                mut_: ctx.mut_,
                id: ctx.id,
                typ: ctx.typ,
                val: v,
            });
            match rest.pop_front() {
                None => {
                    let mut hm = HashMap::new();
                    for f in done.into_iter() {
                        let id = f.id.0.clone();
                        let val = match f.mut_ {
                            Mut::Const => f.val,
                            Mut::Var => Value::Pointer(active.alloc(f.val)).share(),
                        };
                        hm.insert(id, crate::value::FieldValue { mut_: f.mut_, val });
                    }
                    *active.cont() = cont_value(Value::Object(hm));
                    Ok(Step {})
                }
                Some(next) => exp_conts(
                    active,
                    Object(
                        done,
                        FieldContext {
                            mut_: next.0.mut_.clone(),
                            id: next.0.id.fast_clone(),
                            typ: next.0.typ.fast_clone(),
                        },
                        rest,
                    ),
                    &next.0.exp_(),
                ),
            }
        }
        Annot(_t) => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Proj(i) => match &*v {
            Value::Tuple(vs) => {
                if let ProjIndex::Usize(i) = i {
                    if let Some(vi) = vs.get(i) {
                        *active.cont() = Cont::Value_(vi.fast_clone());
                        Ok(Step {})
                    } else {
                        type_mismatch!(file!(), line!())
                    }
                } else {
                    nyi!(line!())
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Dot(f) => match &*v {
            Value::Object(fs) => {
                if let Some(f) = fs.get(&f.0) {
                    *active.cont() = Cont::Value_(f.val.fast_clone());
                    Ok(Step {})
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
            Value::Module(m) => {
                let fd = resolve_def(active.defs(), &m.fields, true, &f.0)?;
                let v = def_as_value(active.defs(), &f.0, &fd.def)?;
                *active.cont() = Cont::Value_(v);
                Ok(Step {})
            }
            Value::Dynamic(d) => {
                let f = d.dynamic().get_field(active.store(), f.0.as_str())?;
                *active.cont() = Cont::Value_(f);
                Ok(Step {})
            }
            Value::Actor(a) => {
                // to do -- get defs from actor n
                // look up definition for f
                // is it a public function?
                // if not public, give error.
                // if not available, type mismatch.
                *active.cont() = Cont::Value_(
                    Value::ActorMethod(ActorMethod {
                        actor: a.id.clone(),
                        method: f.0.clone(),
                    })
                    .share(),
                );
                // do projection, representing function with special value.
                Ok(Step {})
            }
            v => Err(type_mismatch_!(
                file!(),
                line!(),
                "dot-operator-is-matching-operand",
                format!("{:?} @ {}", v, active.cont_source())
            )),
        },
        Debug => match &*v {
            Value::Unit => {
                *active.cont() = Cont::Value_(v);
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        If(e2, e3) => match &*v {
            Value::Bool(b) => {
                *active.cont() = if *b {
                    Cont::Exp_(e2, Vector::new())
                } else {
                    match e3 {
                        Some(e3) => Cont::Exp_(e3, Vector::new()),
                        None => cont_value(Value::Unit),
                    }
                };
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        While1(e1, e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    exp_conts(active, FrameCont::While2(e1, e2.fast_clone()), &e2)
                } else {
                    *active.cont() = cont_value(Value::Unit);
                    Ok(Step {})
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        While2(e1, e2) => match &*v {
            Value::Unit => exp_conts(active, FrameCont::While1(e1.fast_clone(), e2), &e1),
            _ => type_mismatch!(file!(), line!()),
        },
        For1(p, body) => {
            /* for-loop state: iterator object is value v */
            match &*v {
                Value::Opaque(ptr) => {
                    /* enter ForOpaqueIter loop (a "fast path"):
                    no need to evaluate general Motoko code for iterator. */
                    let env = active.env().fast_clone();
                    let source = active.cont_source().clone();
                    let context = active.defs().active_ctx.clone();

                    active.stack().push_front(Frame {
                        context,
                        env,
                        cont: FrameCont::ForOpaqueIter(p, ptr.clone(), body),
                        cont_prim_type: None,
                        source,
                    });
                    *active.cont() = cont_value(Value::Unit);
                    Ok(Step {})
                }
                _ => cont_for_call_dot_next(active, p, v, body),
            }
        }
        For2(p, v_iter, body) => match &*v {
            Value::Null => {
                *active.cont() = cont_value(Value::Unit);
                Ok(Step {})
            }
            Value::Option(v_) => {
                if let Some(env) = pattern_matches(active.env().fast_clone(), &p.0, v_.fast_clone())
                {
                    *active.env() = env;
                    exp_conts(active, FrameCont::For3(p, v_iter, body.fast_clone()), &body)
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        For3(p, v_iter, body) => match &*v {
            Value::Unit => cont_for_call_dot_next(active, p, v_iter, body),
            _ => type_mismatch!(file!(), line!()),
        },
        And1(e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    exp_conts(active, FrameCont::And2, &e2)
                } else {
                    *active.cont() = cont_value(Value::Bool(false));
                    Ok(Step {})
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        And2 => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(*b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Or1(e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    *active.cont() = cont_value(Value::Bool(true));
                    Ok(Step {})
                } else {
                    exp_conts(active, FrameCont::Or2, &e2)
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Or2 => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(*b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Not => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(!b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Opt => {
            *active.cont() = cont_value(Value::Option(v));
            Ok(Step {})
        }
        DoOpt => {
            *active.cont() = cont_value(Value::Option(v));
            Ok(Step {})
        }
        Bang => match &*v {
            Value::Option(v) => {
                *active.cont() = Cont::Value_(v.fast_clone());
                Ok(Step {})
            }
            Value::Null => bang_null(active),
            _ => type_mismatch!(file!(), line!()),
        },
        Call1(inst, e2) => exp_conts(active, FrameCont::Call2(v, inst), &e2),
        Call2(f, inst) => call_cont(active, f, inst, v),
        Call3 => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Return => return_(active, v),
    }
}

// Returns `Some(span)` if the limits include the breakpoint.
fn check_for_breakpoint<A: ActiveBorrow>(active: &A, limits: &Limits) -> Option<Breakpoint> {
    let cont_span = &active.cont_source().span();
    if let Some(span) = cont_span {
        if limits.breakpoints.contains(span) {
            Some(span.clone())
        } else {
            None
        }
    } else {
        None
    }
}

fn check_for_redex<A: ActiveBorrow>(active: &A, limits: &Limits) -> Result<usize, Interruption> {
    let mut redex_bump = 0;
    if let Cont::Value_(ref v) = active.cont() {
        if stack_cont_has_redex(active, v)? {
            redex_bump = 1;
            if let Some(redex_limit) = limits.redex {
                if active.counts().redex >= redex_limit {
                    // if =, adding 1 will exceed limit, so do not.
                    return Err(Interruption::Limit(Limit::Redex));
                }
            }
        }
    }
    Ok(redex_bump)
}
fn active_step<A: Active>(active: &mut A, limits: &Limits) -> Result<Step, Interruption> {
    /* to do -- check for pending send.

        if self.check_for_send_limit(limits) {
            return Err(Interruption::Limit(Limit::Send));
        };
        self.counts()
            .send
            .checked_add(1)
            .expect("Cannot count sends.");

    fn check_for_send_limit(&self, limits: &Limits) -> bool {
        if let Some(s) = limits.send {
            self.counts().send >= s
        } else {
            false
        }
    }

    */
    if let Some(break_span) = check_for_breakpoint(active, limits) {
        return Err(Interruption::Breakpoint(break_span));
    }
    if let Some(step_limit) = limits.step {
        if active.counts().step >= step_limit {
            return Err(Interruption::Limit(Limit::Step));
        }
    }
    let redex_bump = check_for_redex(active, limits)?;
    let ret = active_step_(active)?;
    active.counts().step += 1;
    active.counts().redex += redex_bump;
    Ok(ret)
}

fn active_trace<A: ActiveBorrow>(active: &A) {
    use log::trace;
    trace!(
        "# {:?} step {} (redex {})",
        active.schedule_choice(),
        active.counts().step,
        active.counts().redex
    );
    trace!(" - cont = {:?}", active.cont());
    trace!("   - cont_source = {:?}", active.cont_source());
    trace!("   - env = {:?}", active.env());
    trace!(" - stack = {:#?}", active.stack());
    trace!(" - store = {:#?}", active.store());
    trace!(" - defs  = {:#?}", active.defs());
}

// To advance the active Motoko state by a single step, after all limits are checked.
fn active_step_<A: Active>(active: &mut A) -> Result<Step, Interruption> {
    active_trace(active);
    let mut cont = Cont::Taken;
    std::mem::swap(active.cont(), &mut cont);
    match cont {
        Cont::Taken => unreachable!("The VM's logic currently has an internal issue."),
        Cont::Exp_(e, decs) => {
            if decs.is_empty() {
                exp_step(active, e)
            } else {
                let source = source_from_decs(&decs);
                let env = active.env().fast_clone();
                let context = active.defs().active_ctx.clone();

                active.stack().push_front(Frame {
                    context,
                    env,
                    cont: FrameCont::Decs(decs),
                    source,
                    cont_prim_type: None,
                });
                exp_step(active, e)
            }
        }
        Cont::LetVarRet(_, i) => {
            match i {
                Some(i) => {
                    *active.cont() = Cont::Value_(
                        active
                            .env()
                            .get(&i.0)
                            .ok_or(Interruption::Impossible)?
                            .fast_clone(),
                    )
                }
                None => *active.cont() = cont_value(Value::Unit),
            };
            Ok(Step {})
        }
        Cont::Value_(v) => {
            match &*v {
                Value::Pointer(p) => {
                    // Are we assigning to this pointer?
                    // If not, we are implicitly dereferencing it here.
                    match &active.stack().front() {
                        // Case: Let-binding the pointer.
                        Some(Frame {
                            cont: FrameCont::Let(_, _),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Assignment to a pointer.
                        Some(Frame {
                            cont: FrameCont::Assign1(_),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Binary-op + Assignment to a pointer.
                        Some(Frame {
                            cont: FrameCont::BinAssign1(..),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Array-indexing with a pointer.
                        Some(Frame {
                            cont: FrameCont::Idx1(_),
                            ..
                        }) => return stack_cont(active, v),
                        _ => (),
                    };
                    // Final case: Implicit dereferencing of pointer:
                    let v = active.deref(p)?;
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                }
                _ => stack_cont(active, v),
            }
        }
        Cont::Decs(mut decs) => {
            if decs.is_empty() {
                *active.cont() = cont_value(Value::Unit);
                *active.cont_source() = Source::Evaluation;
                Ok(Step {})
            } else {
                let dec_ = decs.pop_front().unwrap();
                match &dec_.0 {
                    Dec::Type(..) => {
                        *active.cont() = Cont::Decs(decs);
                        Ok(Step {})
                    }
                    Dec::Exp(e) => {
                        *active.cont_source() = dec_.1.clone();
                        *active.cont() = Cont::Exp_(e.fast_clone(), decs);
                        Ok(Step {})
                    }
                    Dec::Let(p, e) => {
                        if decs.is_empty() {
                            let i = match &p.0 {
                                Pat::Var(i) => Some(i.fast_clone()),
                                _ => None,
                            };
                            let source = active.cont_source().clone();
                            exp_conts(
                                active,
                                FrameCont::Let(p.fast_clone(), Cont::LetVarRet(source, i)),
                                e,
                            )
                        } else {
                            exp_conts(active, FrameCont::Let(p.fast_clone(), Cont::Decs(decs)), e)
                        }
                    }
                    Dec::LetActor(i, _, dfs) => {
                        let v = match i {
                            /* Are we upgrading a local Actor? */
                            None => todo!(),
                            Some(local_name) => {
                                let ctx_id = active.defs().active_ctx.clone();
                                let old_def =
                                    ctx_id.get_field(active, &local_name.0).map(|x| x.clone());

                                let id = ActorId::Local(local_name.0.clone());
                                match old_def {
                                    None => def::actor(
                                        active,
                                        format!("<anonymous@{}>", dec_.1),
                                        &id,
                                        dec_.1.clone(),
                                        None,
                                        None,
                                        dfs,
                                    )?,
                                    Some(FieldDef {
                                        def: Def::Actor(old_def),
                                        ..
                                    }) => def::actor_upgrade(
                                        active,
                                        format!("<anonymous@{}>", dec_.1),
                                        &id,
                                        dec_.1.clone(),
                                        None,
                                        None,
                                        dfs,
                                        &old_def,
                                    )?,
                                    _ => unreachable!(),
                                }
                            }
                        };
                        match i {
                            None => (),
                            Some(i) => {
                                active.env().insert(i.0.clone(), v);
                            }
                        };
                        *active.cont() = Cont::Decs(decs);
                        Ok(Step {})
                    }
                    Dec::LetObject(_id, _, _dfs) => {
                        nyi!(line!())
                    }
                    Dec::LetModule(id, _, dfs) => {
                        let v = def::module(
                            active,
                            ModulePath {
                                package_name: None,
                                local_path: format!("<anonymous@{}>", dec_.1),
                            },
                            &id,
                            dec_.1.clone(),
                            None,
                            None,
                            &dfs,
                            None,
                        )?;
                        match id {
                            None => (),
                            Some(i) => {
                                active.env().insert(i.0.clone(), v);
                            }
                        };
                        *active.cont() = Cont::Decs(decs);
                        Ok(Step {})
                    }
                    Dec::LetImport(pattern, _, path) => {
                        let m = def::import(active, path)?;
                        let fields = module_project(active.defs(), &m, &pattern.0)?;
                        for (x, def) in fields {
                            let val = def_as_value(active.defs(), &x.0, &def)?;
                            active.env().insert(x.0.clone(), val);
                        }
                        *active.cont() = Cont::Decs(decs);
                        Ok(Step {})
                    }
                    Dec::Var(p, e) => {
                        if let Some(x) = get_pat_var(&p.0) {
                            exp_conts(active, FrameCont::Var(x.fast_clone(), Cont::Decs(decs)), e)
                        } else {
                            nyi!(line!(), "Dec::Var({:?}, _)", p)
                        }
                    }
                    Dec::Func(f) => {
                        let id = f.name.clone();
                        let v = Value::Function(ClosedFunction(Closed {
                            ctx: active.defs().active_ctx.clone(),
                            env: active.env().fast_clone(),
                            content: f.clone(),
                        }))
                        .share();
                        if decs.is_empty() {
                            *active.cont() = Cont::Value_(v);
                            Ok(Step {})
                        } else {
                            if let Some(i) = id {
                                active.env().insert(i.as_ref().data_ref().clone(), v);
                            };
                            *active.cont() = Cont::Decs(decs);
                            Ok(Step {})
                        }
                    }
                    d => nyi!(line!(), "{:?}", d),
                }
            }
        } //_ => unimplemented!(),
    }
}

impl Core {
    /// New VM for a given program.
    pub fn new(prog: Prog) -> Self {
        Core {
            defs: Defs::new(),
            schedule_choice: ScheduleChoice::Agent,
            agent: agent_init(prog),
            actors: Actors {
                map: HashMap::new(),
            },
            module_files: ModuleFiles {
                map: HashMap::new(),
                import_stack: Vector::new(),
            },
            next_resp_id: 0,
            debug_print_out: Vector::new(),
        }
    }

    /// Load `base` library into an existing Core.
    pub fn load_base(&mut self) -> Result<(), Interruption> {
        use crate::package::{get_base_library, get_prim_library};
        let prim = get_prim_library();
        for (path, file) in prim.files.into_iter() {
            // remove '.mo' from suffix of the filename to produce the path
            let path = format!("{}", &path[0..path.len() - 3]);
            self.set_module(Some("⛔".to_string()), path.clone(), &file.content)?;
        }
        let base = get_base_library();
        for (path, file) in base.files.into_iter() {
            // remove '.mo' from suffix of the filename to produce the path
            let path = format!("{}", &path[0..path.len() - 3]);
            self.set_module(Some("base".to_string()), path.clone(), &file.content)?
        }
        Ok(())
    }

    /// New VM without any program.
    pub fn empty() -> Self {
        let mut c = Self::new(crate::ast::Delim::new());
        c.run(&Limits::none()).expect("empty");
        c
    }

    /// New VM from a given program string, to be parsed as the Agent program.
    #[cfg(feature = "parser")]
    pub fn parse(s: &str) -> Result<Self, crate::parser_types::SyntaxError> {
        Ok(Self::new(crate::check::parse(s)?))
    }

    fn assert_actor_def(
        local_path: String,
        s: &str,
    ) -> Result<(Vector<Dec_>, Option<Id_>, crate::ast::DecFields), Interruption> {
        let p = match crate::check::parse(s) {
            Err(code) => {
                return Err(Interruption::SyntaxError(SyntaxError {
                    package_name: None,
                    local_path,
                    code,
                }))
            }
            Ok(r) => r,
        };
        if p.vec.is_empty() {
            return Err(Interruption::MissingActorDefinition);
        };
        let mut vec = p.vec.clone();
        let last = vec.pop_back();
        match last {
            Some(d) => match &d.0 {
                Dec::LetActor(id, _, dfs) => Ok((vec, id.clone(), dfs.clone())),
                _ => Err(Interruption::NotAnActorDefinition),
            },
            None => unreachable!(),
        }
    }

    /// Test if the string is a syntatically-valid Motoko module.
    #[cfg(feature = "parser")]
    pub fn is_module_def(s: &str) -> bool {
        // we ignore the syntax error messages, if any; so this path doesn't matter.
        let path = ModulePath {
            package_name: None,
            local_path: "".to_string(),
        };
        Self::assert_module_def(path, s).is_ok()
    }

    /// path is only used to form SyntaxError Interruptions, if they are needed.
    fn assert_module_def(path: ModulePath, s: &str) -> Result<ModuleFileInit, Interruption> {
        let p = match crate::check::parse(s) {
            Err(code) => {
                return Err(Interruption::SyntaxError(SyntaxError {
                    package_name: path.package_name,
                    local_path: path.local_path,
                    code,
                }))
            }
            Ok(r) => r,
        };
        if p.vec.is_empty() {
            return Err(Interruption::MissingModuleDefinition);
        };
        let mut vec = p.vec.clone();
        let last = vec.pop_back();
        match last {
            Some(d) => match &d.0 {
                Dec::LetModule(id, _, dfs) => Ok(ModuleFileInit {
                    file_content: s.to_string(),
                    outer_decs: vec,
                    id: id.clone(),
                    fields: dfs.clone(),
                }),
                _ => Err(Interruption::NotAModuleDefinition),
            },
            None => unreachable!(),
        }
    }

    /// Set the actor `id` to the given `definition`, regardless of whether `id` is defined already or not.
    /// If not defined, this is the same as `create_actor`.
    /// Otherwise, it is the same as `update_actor`.
    pub fn set_actor(&mut self, path: String, id: ActorId, def: &str) -> Result<(), Interruption> {
        if self.actors.map.get(&id).is_none() {
            self.create_actor(path, id, def)
        } else {
            self.upgrade_actor(path, id, def)
        }
    }

    /// Set the path's file content (initially), or re-set it, when it changes.
    ///
    /// Optionally, the file is part of a named package, and will be distinct from paths from other packages.
    ///
    /// The content must be a module.  For actors, see `set_actor` instead.
    pub fn set_module(
        &mut self,
        package_name: Option<String>,
        local_path: String,
        file_content: &str,
    ) -> Result<(), Interruption> {
        let path = crate::vm_types::ModulePath {
            package_name,
            local_path,
        };
        let init = Self::assert_module_def(path.clone(), file_content)?;
        let old = self.module_files.map.get(&path).map(|x| x.clone());
        if let Some(ModuleFileState::Defined(old)) = old {
            let (saved, ctxid, old_ctx) = self.defs().reenter_context(&old.context);
            for dec in init.outer_decs.iter() {
                let dec = dec.clone();
                let df = DecField {
                    vis: None,
                    stab: None,
                    dec,
                };
                def::insert_static_field(self, &df.dec.1, &df)?;
            }
            def::module(
                self,
                path,
                &init.id,
                Source::CoreSetModule,
                None,
                None,
                &init.fields,
                Some(old.module.clone()),
            )?;
            self.defs().releave_context(saved, &ctxid, &old_ctx);
        } else {
            self.module_files
                .map
                .insert(path.clone(), ModuleFileState::Init(init));
        };
        Ok(())
    }

    /// Call an actor method.
    pub fn call(
        &mut self,
        actor: &ActorId,
        method: &Id,
        arg: Value_,
        limits: &Limits,
    ) -> Result<Value_, Interruption> {
        self.assert_idle_agent()?;
        let fn_v = Value::ActorMethod(ActorMethod {
            actor: actor.clone(),
            method: method.clone(),
        })
        .share();
        let context = self.defs().active_ctx.clone();

        self.stack().push_front(Frame {
            context,
            env: HashMap::new(),
            cont: FrameCont::Call2(fn_v, None),
            source: Source::CoreCall,
            cont_prim_type: None,
        });
        *self.cont() = Cont::Value_(arg);
        *self.cont_source() = Source::CoreCall;
        self.run(limits)
    }

    /// Create a new actor with the given (unused) `id`, and the definition `def`.
    pub fn create_actor(
        &mut self,
        path: String,
        id: ActorId,
        def: &str,
    ) -> Result<(), Interruption> {
        if let Some(_) = self.actors.map.get(&id) {
            return Err(Interruption::AmbiguousActorId(id));
        };
        let (decs, _id, dfs) = Self::assert_actor_def(path.clone(), def)?;
        let (saved, new_root) = self.defs().enter_context(true);
        for dec in decs.iter() {
            let dec = dec.clone();
            let df = crate::ast::DecField {
                vis: None,
                stab: None,
                dec,
            };
            def::insert_static_field(self, &df.dec.1, &df)?;
        }
        def::actor(self, path, &id, Source::CoreCreateActor, None, None, &dfs)?;
        self.defs().leave_context(saved, &new_root);
        Ok(())
    }

    /// Upgrade an existing actor with the given `id`, with new definition `def`.
    pub fn upgrade_actor(
        &mut self,
        path: String,
        id: ActorId,
        def: &str,
    ) -> Result<(), Interruption> {
        let old_def = if let Some(old) = self.actors.map.get(&id) {
            old.def.clone()
        } else {
            return Err(Interruption::ActorIdNotFound(id));
        };
        let (decs, _id, dfs) = Self::assert_actor_def(path.clone(), def)?;
        let (saved, ctxid, old_ctx) = self.defs().reenter_context(&old_def.context);
        for dec in decs.iter() {
            let dec = dec.clone();
            let df = crate::ast::DecField {
                vis: None,
                stab: None,
                dec,
            };
            def::insert_static_field(self, &df.dec.1, &df)?;
        }
        def::actor_upgrade(
            self,
            path,
            &id,
            Source::CoreUpgradeActor,
            None,
            None,
            &dfs,
            &old_def,
        )?;
        self.defs().releave_context(saved, &ctxid, &old_ctx);
        Ok(())
    }

    /// Attempt a single-step of VM, under some limits.
    pub fn step(&mut self, limits: &Limits) -> Result<Step, Interruption> {
        match active_step(self, limits) {
            Ok(Step {}) => Ok(Step {}),
            Err(Interruption::Send(am, inst, v)) => self.send(limits, am, inst, v),
            Err(Interruption::Response(r)) => self.response(limits, r),
            Err(other_interruption) => return Err(other_interruption),
        }
    }

    fn get_public_actor_field(&self, a: &ActorId, m: &Id) -> Result<FieldDef, Interruption> {
        let actor = match self.actors.map.get(a) {
            Some(a) => a,
            None => return Err(Interruption::ActorIdNotFound(a.clone())),
        };
        let f = match actor.def.fields.get_field(self, &m) {
            None => return Err(Interruption::ActorFieldNotFound(a.clone(), m.clone())),
            Some(f) => f,
        };
        let f_is_public = match &f.vis {
            Some(x) => x.0.is_public(),
            None => false,
        };
        if !f_is_public {
            return Err(Interruption::ActorFieldNotPublic(a.clone(), m.clone()));
        };
        Ok(f.clone())
    }

    fn send(
        &mut self,
        _limits: &Limits,
        am: ActorMethod,
        inst: Option<Inst>,
        v: Value_,
    ) -> Result<Step, Interruption> {
        let context = self.defs().active_ctx.clone();
        let resp_target = self.schedule_choice.clone();
        self.schedule_choice = ScheduleChoice::Actor(am.actor.clone());
        let actor = self.actors.map.get(&am.actor).unwrap();
        let actor_env = actor.env.fast_clone();
        let f = {
            let f = self.get_public_actor_field(&am.actor, &am.method)?;
            match &f.def {
                Def::Func(f) => f.clone(),
                _ => type_mismatch!(file!(), line!()),
            }
        };
        assert!(actor.active.is_none());
        let mut activation = Activation::new();
        activation.stack.push_front(Frame {
            context,
            source: Source::Evaluation,
            cont_prim_type: None,
            env: actor.env.fast_clone(),
            cont: FrameCont::Respond(resp_target),
        });
        let actor = self.actors.map.get_mut(&am.actor).unwrap();
        actor.active = Some(activation);
        call_function_def(self, actor_env, &f, inst, v)
    }

    fn response(&mut self, _limits: &Limits, r: Response) -> Result<Step, Interruption> {
        match self.schedule_choice {
            ScheduleChoice::Actor(ref i) => {
                let actor = self.actors.map.get_mut(i).unwrap();
                actor.active = None;
            }
            _ => unreachable!(),
        };
        self.schedule_choice = r.target;
        *self.cont() = Cont::Value_(r.value);
        Ok(Step {})
    }

    /// Run multiple steps of VM, with given limits.
    /// `Ok(value)` means that the Agent is idle.
    pub fn run(&mut self, limits: &Limits) -> Result<Value_, Interruption> {
        loop {
            match self.step(limits) {
                Ok(Step {}) => {}
                Err(Interruption::Done(v)) => return Ok(v),
                Err(i) => return Err(i),
            }
        }
    }

    /// Assert that the Agent is idle.
    pub fn assert_idle_agent(&self) -> Result<(), EvalInitError> {
        if self.schedule_choice != ScheduleChoice::Agent {
            return Err(EvalInitError::AgentNotScheduled);
        }
        if !self.agent.active.stack.is_empty() {
            return Err(EvalInitError::NonEmptyStack);
        }
        match self.agent.active.cont {
            Cont::Value_(_) => {}
            _ => return Err(EvalInitError::NonValueCont),
        };
        Ok(())
    }

    /// For running snippets of code as if they were within a package.
    /// (They import that package's modules as if they are all local).
    pub fn set_ambient_package_name(
        &mut self,
        package: Option<String>,
    ) -> Result<(), Interruption> {
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        self.agent.active.package = package;
        Ok(())
    }

    /// Evaluate a new program fragment, assuming agent is idle.
    #[cfg(feature = "parser")]
    pub fn eval(&mut self, new_prog_frag: &str) -> Result<Value_, Interruption> {
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        let local_path = "<anonymous>".to_string();
        let package_name = None;
        let p = crate::check::parse(new_prog_frag).map_err(|code| {
            Interruption::SyntaxError(SyntaxError {
                code,
                local_path,
                package_name,
            })
        })?;
        self.agent.active.cont = Cont::Decs(p.vec);
        self.run(&Limits::none())
    }

    /// Evaluate a new program fragment, assuming agent is idle.
    ///
    /// The block may refer to variables
    /// bound as arguments, and then forgotten after evaluation.
    pub fn eval_open_block(
        &mut self,
        value_bindings: Vec<(&str, impl Into<Value_>)>,
        prog: Prog,
    ) -> Result<Value_, Interruption> {
        let source = self.agent.active.cont_source.clone(); // to do -- use prog source
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        exp_conts_(
            self,
            source.clone(),
            FrameCont::Block,
            Cont::Decs(prog.vec),
            source,
        )?;
        for (x, v) in value_bindings.into_iter() {
            let _ = self.agent.active.env.insert(x.to_id(), v.into());
        }
        self.run(&Limits::none())
    }

    /// Evaluate a new program fragment, assuming agent is idle.
    pub fn eval_prog(&mut self, prog: Prog) -> Result<Value_, Interruption> {
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        self.agent.active.cont = Cont::Decs(prog.vec);
        self.run(&Limits::none())
    }

    #[inline]
    pub fn dealloc(&mut self, pointer: &Pointer) -> Option<Value_> {
        self.store().dealloc(&pointer.local)
    }

    // to do -- rename this to "define" or "bind" ("assign" connotes mutation).
    #[inline]
    pub fn assign(&mut self, id: impl ToId, value: impl Into<Value_>) {
        let value = value.into();
        self.env().insert(id.to_id(), value);
    }

    #[inline]
    pub fn assign_alloc(&mut self, id: impl ToId, value: impl Into<Value_>) -> Pointer {
        let pointer = self.alloc(value);
        self.assign(id, Value::Pointer(pointer.fast_clone()).share());
        pointer
    }
}

#[cfg(feature = "parser")]
/// Used for tests in check module.
pub fn eval_limit(prog: &str, limits: &Limits) -> Result<Value_, Interruption> {
    info!("eval_limit:");
    info!("  - prog = {}", prog);
    info!("  - limits = {:#?}", limits);
    //use crate::vm_types::Interruption::SyntaxError;
    let package_name = None;
    let local_path = "<anonymous>".to_string();
    let p = crate::check::parse(prog).map_err(|code| {
        Interruption::SyntaxError(SyntaxError {
            code,
            local_path,
            package_name,
        })
    })?;
    info!("eval_limit: parsed.");
    let mut c = Core::new(p);
    let r = c.run(limits);
    use log::info;
    info!("eval_limit: result: {:#?}", r);
    r
}

/// Used for tests in check module.
#[cfg(feature = "parser")]
pub fn eval(prog: &str) -> Result<Value_, Interruption> {
    eval_limit(prog, &Limits::none())
}

#[cfg(feature = "parser")]
pub fn eval_into<T: serde::de::DeserializeOwned>(prog: &str) -> Result<T, Interruption> {
    eval(prog)?.to_rust().map_err(Interruption::ValueError)
}

// TODO: possibly refactor to `Cont::Value(Value)` and `Cont::Value_(Value_)`
#[inline(always)]
fn cont_value(value: Value) -> Cont {
    // TODO: memoize (), true, false, null, variants, etc.
    Cont::Value_(value.share())
}
