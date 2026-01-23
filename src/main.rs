use clap::Parser;
use miette::Result;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::Layer;
use tracing_subscriber::Registry;
use tracing_subscriber::layer::SubscriberExt;
use tracing_tree::HierarchicalLayer;

mod commands;

use commands::Command;

fn main() -> Result<()> {
    // Parse arguments
    let args = Args::parse();

    // Set up subscriber
    let debug = if args.debug {
        true
    } else {
        std::env::var("NOVAC_DEBUG")
            .map(|v| {
                let v = v.to_lowercase();
                v == "1" || v == "true" || v == "yes"
            })
            .unwrap_or(false)
    };

    let hierarchical = HierarchicalLayer::default()
        .with_deferred_spans(true)
        .with_filter(get_env_filter(debug));
    let subscriber = Registry::default().with(hierarchical);
    tracing::subscriber::set_global_default(subscriber).unwrap();

    // Execute command
    commands::execute_command(args.command)
}

#[derive(Parser)]
struct Args {
    /// Enable debug/trace logging
    #[arg(short, long)]
    debug: bool,

    #[command(subcommand)]
    command: Command,
}

fn get_env_filter(debug: bool) -> EnvFilter {
    if debug {
        EnvFilter::new("trace")
    } else {
        EnvFilter::new("info")
    }
}
