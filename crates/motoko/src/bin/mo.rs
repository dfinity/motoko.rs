use structopt::StructOpt;

use log::info;
use std::io;
use structopt::{clap, clap::Shell};

use motoko::format::{format_one_line, format_pretty};
use motoko::vm_types::Limits;

use rustyline::error::ReadlineError;
use rustyline::Editor;

pub type OurResult<X> = Result<X, OurError>;

impl From<()> for OurError {
    fn from(_: ()) -> Self {
        OurError::Unknown
    }
}

impl From<motoko::vm_types::Error> for OurError {
    fn from(err: motoko::vm_types::Error) -> Self {
        OurError::VM(err)
    }
}

impl From<motoko::parser_types::SyntaxError> for OurError {
    fn from(err: motoko::parser_types::SyntaxError) -> Self {
        OurError::Syntax(err)
    }
}

#[derive(Debug, Clone)]
pub enum OurError {
    Unknown,
    String(String),
    VM(motoko::vm_types::Error),
    Syntax(motoko::parser_types::SyntaxError),
}

/// Motoko tools in Rust.
#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "mo-rs",
    setting = clap::AppSettings::DeriveDisplayOrder
)]
pub struct CliOpt {
    /// Trace-level logging (most verbose)
    #[structopt(short = "t", long = "trace-log")]
    pub log_trace: bool,
    /// Debug-level logging (medium verbose)
    #[structopt(short = "d", long = "debug-log")]
    pub log_debug: bool,
    /// Coarse logging information (not verbose)
    #[structopt(short = "L", long = "log")]
    pub log_info: bool,

    #[structopt(subcommand)]
    pub command: CliCommand,
}

#[derive(StructOpt, Debug, Clone)]
pub enum CliCommand {
    #[structopt(
        name = "completions",
        about = "Generate shell scripts for auto-completions."
    )]
    Completions {
        shell: Shell,
    },
    Check {
        input: String,
    },
    Echo {
        input: String,
    },
    Format {
        input: String,
        #[structopt(short = "w")]
        width: usize,
    },
    Eval {
        #[structopt(short = "s", long = "step-limit")]
        step_limit: Option<usize>,

        input: String,
    },
    Repl,
}

fn init_log(level_filter: log::LevelFilter) {
    use env_logger::{Builder, WriteStyle};
    let mut builder = Builder::new();
    builder
        .filter(None, level_filter)
        .write_style(WriteStyle::Always)
        .init();
}

fn main() -> OurResult<()> {
    info!("Starting...");
    let cli_opt = CliOpt::from_args();
    info!("Init log...");
    init_log(
        match (cli_opt.log_trace, cli_opt.log_debug, cli_opt.log_info) {
            (true, _, _) => log::LevelFilter::Trace,
            (_, true, _) => log::LevelFilter::Debug,
            (_, _, true) => log::LevelFilter::Info,
            (_, _, _) => log::LevelFilter::Warn,
        },
    );
    info!("Evaluating CLI command: {:?} ...", &cli_opt.command);
    let () = match cli_opt.command {
        CliCommand::Completions { shell: s } => {
            // see also: https://clap.rs/effortless-auto-completion/
            CliOpt::clap().gen_completions_to("caniput", s, &mut io::stdout());
            info!("done");
        }
        CliCommand::Check { input } => {
            let _ = motoko::check::parse(&input)?;
            println!("check::parse: okay.");
        }
        CliCommand::Echo { input } => {
            let p = motoko::check::parse(&input)?;
            println!("{}", format_one_line(&p));
        }
        CliCommand::Format { input, width } => {
            let p = motoko::lexer::create_token_tree(&input)?;
            println!("{}", format_pretty(&p, width));
        }
        CliCommand::Eval { input, step_limit } => {
            let limits = match step_limit {
                None => Limits::none(),
                Some(limit) => Limits::none().step(limit),
            };
            let v = motoko::vm::eval_limit(&input, &limits);
            println!("final value: {:?}", v)
        }
        CliCommand::Repl => {
            let mut rl = Editor::<()>::new();
            if rl.load_history("history.txt").is_err() {
                println!("No previous history.");
            }
            use motoko::vm_types::Core;
            let mut core = Core::empty();
            loop {
                let readline = rl.readline("mo> ");
                match readline {
                    Ok(line) => {
                        let v = core.eval_str(&line);
                        println!("{:?}", v);
                        rl.add_history_entry(line.as_str());
                    }
                    Err(ReadlineError::Interrupted) => {
                        println!("CTRL-C");
                        break;
                    }
                    Err(ReadlineError::Eof) => {
                        println!("CTRL-D");
                        break;
                    }
                    Err(err) => {
                        println!("Error: {:?}", err);
                        break;
                    }
                }
            }
            rl.save_history("history.txt").unwrap();
        }
    };
    Ok(())
}
