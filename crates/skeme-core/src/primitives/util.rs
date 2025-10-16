//! Utility primitives
//!
//! Implements ~20 utility primitives: keywords, time, language, style, debug.

use crate::scheme::SchemeEngine;
use anyhow::Result;

/// Register all utility primitives
pub fn register_util_primitives(_engine: &mut SchemeEngine) -> Result<()> {
    // Variable access
    // Note: We need to wrap get_variable in a closure to capture the engine reference
    // For now, we'll handle this differently - see SchemeEngine::register_variable_access

    // TODO: Implement other utility primitives when needed:
    // - load (will be built-in via Steel or via engine wrapper)
    // - keyword?, keyword->string, string->keyword
    // - time, time->string
    // - error, debug

    Ok(())
}
