use crate::ast::{Dec, DecField, Dec_, Exp_, Id, Id_, Inst, Prog, Source, ToId};
//use crate::ast_traversal::ToNode;
use crate::shared::{FastClone, Share};
use crate::value::{ActorId, ActorMethod, Value, Value_};
use crate::vm_types::{
    def::{Def, Defs, Field as FieldDef},
    stack::{Frame, FrameCont},
    Activation, Active, Actors, Agent, Cont, Core, Counts, Interruption, Limits, ModuleFileState,
    ModuleFiles, ModulePath, Pointer, Response, ScheduleChoice, Step, SyntaxError,
};
use crate::vm_types::{EvalInitError, Store};
use im_rc::{HashMap, Vector};
use std::vec::Vec;

use crate::type_mismatch;
use crate::vm::def;
use crate::vm_types::ModuleFileInit;

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
        match crate::vm::active_step(self, limits) {
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
        crate::vm::call_function_def(self, actor_env, &f, inst, v)
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
        crate::vm::exp_conts_(
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

    /// Evaluate a new program fragment, assuming agent is idle.
    pub fn eval_exp(&mut self, e: Exp_) -> Result<Value_, Interruption> {
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        self.agent.active.cont = Cont::Exp_(e, Vector::new());
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
