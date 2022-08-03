use structopt::StructOpt;

use log::info;
use std::io;
use structopt::{clap, clap::Shell};

use motoko::format::format_one_line;

pub type OurResult<X> = Result<X, OurError>;

impl From<()> for OurError {
    fn from(_unit: ()) -> Self {
        OurError::Unit
    }
}

#[derive(Debug, Clone)]
pub enum OurError {
    Unit,
    String(String),
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
    };
    Ok(())
}
