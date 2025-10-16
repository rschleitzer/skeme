//! Processing control primitives
//!
//! Implements ~20 primitives for template processing control.
//!
//! Key primitives:
//! - process-children, process-children-trim
//! - process-node-list, process-element-with-id
//! - next-match
//! - literal, sosofo-append, empty-sosofo

use crate::scheme::SchemeEngine;
use anyhow::Result;
use steel::rvals::Custom;
use steel::steel_vm::register_fn::RegisterFn;

/// A SOSOFO (Specification of a Sequence of Flow Objects)
///
/// For our text backend, this is simplified to just accumulated text.
/// In full DSSSL, this would be a complex tree of flow objects.
#[derive(Debug, Clone)]
pub struct Sosofo {
    /// The text content of this sosofo
    text: String,
}

impl Sosofo {
    /// Create a new sosofo with the given text
    pub fn new(text: String) -> Self {
        Sosofo { text }
    }

    /// Create an empty sosofo
    pub fn empty() -> Self {
        Sosofo {
            text: String::new(),
        }
    }

    /// Get the text content
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Append another sosofo
    pub fn append(&self, other: &Sosofo) -> Sosofo {
        Sosofo {
            text: format!("{}{}", self.text, other.text),
        }
    }
}

// Implement Steel's Custom trait for Sosofo
impl Custom for Sosofo {}

/// Register all processing control primitives
pub fn register_processing_primitives(engine: &mut SchemeEngine) -> Result<()> {
    // SOSOFO constructors
    engine.register_fn("literal", processing_literal);
    engine.register_fn("empty-sosofo", processing_empty_sosofo);
    engine.register_fn("sosofo-append", processing_sosofo_append);

    // Flow object creation
    // Note: make is a keyword in DSSSL, but we'll handle it as a function
    // The real DSSSL `make` takes keyword arguments, but Steel doesn't support that directly
    // So we'll provide specific constructors
    engine.register_fn("make-entity", processing_make_entity);
    engine.register_fn("make-formatting-instruction", processing_make_formatting_instruction);

    // TODO: Context-dependent processing
    // - process-children (requires processing context)
    // - process-node-list (requires processing context)

    Ok(())
}

// ============================================================================
// SOSOFO Primitives
// ============================================================================

/// Create a sosofo with literal text
fn processing_literal(text: String) -> Sosofo {
    Sosofo::new(text)
}

/// Create an empty sosofo
fn processing_empty_sosofo() -> Sosofo {
    Sosofo::empty()
}

/// Append two sosofos
fn processing_sosofo_append(a: &Sosofo, b: &Sosofo) -> Sosofo {
    a.append(b)
}

// ============================================================================
// Flow Object Primitives
// ============================================================================

/// Create an entity flow object (outputs to a file)
/// In OpenJade: (make entity system-id: "filename" (literal "content"))
/// For us: (make-entity "filename" sosofo)
fn processing_make_entity(system_id: String, content: &Sosofo) -> Sosofo {
    // For now, we'll mark this as an entity in the sosofo
    // The actual file writing will happen at the top level
    Sosofo::new(format!("#<entity:{}:{}>", system_id, content.text()))
}

/// Create a formatting-instruction flow object (plain text output)
/// In OpenJade: (make formatting-instruction data: "text")
/// For us: (make-formatting-instruction "text") or just use literal
fn processing_make_formatting_instruction(data: String) -> Sosofo {
    // For plain text output, this is just like literal
    Sosofo::new(data)
}
