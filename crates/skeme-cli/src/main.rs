//! Skeme CLI entry point

use anyhow::{Context, Result};
use clap::Parser;
use skeme_cli::Args;
use skeme_core::{SchemeEngine, XmlParser};
use skeme_template::{TemplateLoader, TemplateProcessor};
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<()> {
    // Parse command-line arguments
    let args = Args::parse();

    // Set up logging
    let log_level = if args.verbose { Level::DEBUG } else { Level::INFO };
    let subscriber = FmtSubscriber::builder()
        .with_max_level(log_level)
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .context("Failed to set up logging")?;

    info!("Skeme v{}", skeme_core::VERSION);
    info!("Template: {}", args.template.display());
    info!("Input: {}", args.input.display());

    // Step 1: Parse XML with DTD validation
    info!("Parsing XML input: {}", args.input.display());
    let parser = XmlParser::new();
    let grove = parser
        .parse_file(&args.input)
        .context("Failed to parse XML input")?;
    info!("XML parsed successfully");

    // Step 2: Create Scheme engine with primitives
    info!("Initializing Scheme engine");
    let mut engine = SchemeEngine::new().context("Failed to create Scheme engine")?;

    // Step 3: Set variables from CLI
    for (name, value) in &args.variables {
        info!("Setting variable: {} = {}", name, value);
        engine.set_variable(name.clone(), value.clone());
    }

    // Step 4: Make the grove root available to Scheme
    // For now, we'll load the template and let it access the grove
    // In a full implementation, we'd inject the root node into the Scheme environment
    // TODO: Register the grove root as a global variable
    info!("Grove root element: {}", grove.root().gi());

    // Step 5: Load and execute template
    info!("Loading template: {}", args.template.display());
    engine
        .load_file(args.template.to_str().unwrap())
        .context("Failed to load template")?;
    info!("Template executed successfully");

    // Step 6: Output results
    // For now, templates will handle their own output via make-entity
    // In the future, we might capture the final sosofo and process it

    println!("\nSkeme processing complete!");
    println!("Template: {}", args.template.display());
    println!("Input: {}", args.input.display());
    if !args.variables.is_empty() {
        println!("Variables: {:?}", args.variables);
    }

    Ok(())
}
