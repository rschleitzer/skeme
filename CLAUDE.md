# CLAUDE.md - Dazzle Project Context

> **Context document for Claude Code and future development sessions**
>
> This document provides complete background on the Dazzle project, including motivation, design decisions, technical specifications, and implementation guidance.

## Table of Contents

1. [🚨 Critical Decision](#-critical-decision---october-2025) 🆕
2. [Project Overview](#project-overview)
3. [Motivation & History](#motivation--history)
4. [DSSSL Lineage](#dsssl-lineage)
5. [OpenJade Analysis](#openjade-analysis)
6. [Primitive Catalog](#primitive-catalog)
7. [Feature Matrix](#feature-matrix)
8. [Technical Stack](#technical-stack)
9. [Architecture Design](#architecture-design) 🆕
10. [CLI Design](#cli-design)
11. [Distribution Strategy](#distribution-strategy)
12. [Implementation Roadmap](#implementation-roadmap)

---

## 🚨 Critical Decision - October 2025

**We are NOT using Steel Scheme.** We are **porting OpenJade's Scheme interpreter to Rust**.

### Why?

**Steel compatibility issues discovered** (October 19, 2025):
1. ⚠️ **Parser bug**: Steel requires `let` bindings on same line - not whitespace-agnostic
   ```scheme
   ; This FAILS in Steel but is valid R4RS Scheme:
   (let
       ((x 5))
       (+ x 3))
   ```
2. ⚠️ **Poor error messages**: Steel reports errors as spans (byte offsets), not line numbers
3. ⚠️ **Workaround trap**: Every Steel bug requires workarounds in user's DSSSL code

**User's existing DSSSL code**
- Has multi-line `let` bindings in 3+ places (sql.scm:652, 819, 828)
- Is valid R4RS Scheme
- Works perfectly in OpenJade
- Should NOT need modification for Dazzle

### The Solution

**Port OpenJade's Scheme interpreter** (~12,000 lines C++ → ~10,000 lines Rust):

**Pros**:
- ✅ 100% compatible with existing DSSSL code (proven over 25 years)
- ✅ Whitespace-agnostic (R4RS compliant)
- ✅ Full control over interpreter and error messages
- ✅ Human-friendly errors (line numbers, stack traces)
- ✅ Reasonable scope (8-12 weeks part-time)
- ✅ Performance on par with OpenJade (Rust should match or beat C++)

**Status**: Architecture designed, ready to start Phase 1

**Key files**:
- `OPENJADE_INTERPRETER_ANALYSIS.md` - Detailed porting analysis
- OpenJade sources: `/Users/r.schleitzer/repos/openjade/style/` (interpreter)

---

## Project Overview

**Dazzle** is a Rust-based code generation tool powered by Scheme templates. It reimplements the essential functionality of OpenJade's SGML backend for modern systems.

### Key Facts

- **Name**: Dazzle (evocative of brilliance and transformation)
- **Purpose**: Template-driven code generation from XML/SGML input
- **Language**: Rust (host) + Scheme (templates)
- **Primary Use Case**: fire code generation

### Why "Dazzle"?

- Evokes transformation and brilliance - what code generation does
- Memorable and distinctive
- Works as imperative verb - "dazzle this template"
- Available everywhere (crates.io, all distros, Homebrew, MacPorts)
- No trademark conflicts (only libdazzle exists, a GNOME library - different domain)

---

## Motivation & History

### The Problem

OpenJade (and its predecessor Jade) are disappearing from package managers:

- **Homebrew**: Already dropped OpenJade
- **MacPorts**: Still available, but aging
- **Linux distros**: Maintenance declining
- **Codebase**: Unmaintained C++ from 1990s

### User's Context

- Initially: SGML documentation generation
- Since 2003: **Code generation via SGML backend**
- Current project: **fire** (FHIR 5 server in Rust)
- Large existing codebase depending on OpenJade workflow

### The Solution: Dazzle

Preserve the workflow in a maintainable form:

- ✅ Pure Rust implementation (maintainable, cross-platform)
- ✅ Scheme-powered (keep template language)
- ✅ XML + DTD validation (libxml2)
- ✅ OpenJade CLI-compatible (zero retraining)
- ✅ Focus on code generation (not document formatting)
- ✅ Use DSSSL XML wrappers with entity references (OpenJade compatible)

---

## DSSSL Lineage

Understanding what Dazzle inherits and what it doesn't:

### 1. DSSSL Standard (ISO/IEC 10179:1996)

**Download**: ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf

**The "Big Bang" of our information:**

- Full specification of DSSSL (Document Style Semantics and Specification Language)
- Two languages:
  - **Transformation Language**: SGML → SGML transformations
  - **Style Language**: SGML → formatted output (print, screen)
- Based on **R4RS Scheme** (side-effect-free subset)
- Defines:
  - SDQL (Standard Document Query Language)
  - Grove model (document tree representation)
  - Flow objects (formatting primitives)
  - Style specifications

**Important**: DSSSL does NOT standardize:
- File I/O (`load`, `read`, `write`)
- The SGML backend (that's an OpenJade extension)

### 2. DSSSL-online (Jon Bosak Proposal)

Simplified subset of DSSSL proposed for web use:

- "DSSSL Lite" for WWW Consortium consideration (~1994)
- Goal: Make DSSSL practical for web browsers
- Simpler than full ISO standard
- Eventually led to CSS, not DSSSL adoption

### 3. Jade (James Clark, ~1996)

**First major DSSSL implementation:**

- Implemented DSSSL-online with restrictions and extensions
- Written in C++
- Backends: RTF, TeX, MIF, HTML, SGML
- **SGML backend**: Key extension - SGML-to-SGML transformations
- Introduced "external procedures" concept
- Added `load` procedure (not in DSSSL standard!)

### 4. OpenJade (Community fork, 1999-2010)

**Evolution of Jade:**

- Fork maintained by DSSSL community
- More backends, bug fixes, extensions
- **SGML backend remains the killer feature**
- Last significant activity: ~2010
- Current status: Unmaintained, C++, hard to build

**OpenJade Extensions not in DSSSL:**
- `load` procedure (file loading)
- SGML/XML backend for transformations
- Various external procedures
- Platform-specific features

---

## OpenJade Analysis

**Source**: Analyzed actual OpenJade 1.3.2 codebase (April 2003)

### Codebase Size

**Total: ~72,000 lines of C++** across 117 files

**Key components:**
- `style/` (39,135 lines) - DSSSL interpreter, Scheme evaluator, primitives
- `jade/` (20,641 lines) - FOT builders (HTML, RTF, TeX, MIF, **SGML backend**)
- `spgrove/` (7,006 lines) - OpenSP grove integration
- `grove/` (2,393 lines) - Grove model
- `include/` (48 lines) - Headers (depends on external OpenSP library)

**Critical files for Dazzle:**
- `style/primitive.h` - **224 Scheme primitives** defined
- `style/primitive.cxx` - 5,704 lines - Primitive implementations
- `style/Interpreter.cxx` - 2,390 lines - Scheme interpreter core
- `style/SchemeParser.cxx` - ~2,300 lines - S-expression parser
- `jade/SgmlFOTBuilder.cxx` - 2,824 lines - **SGML backend implementation**
- `grove/Node.{h,cxx}` - ~2,400 lines - Grove node interface

### OpenSP Dependency

**Important discovery**: OpenJade does NOT include OpenSP sources!

- OpenSP was split from OpenJade in 2002 (version 1.3.2)
- OpenJade's `configure` looks for external OpenSP installation
- OpenSP is ~100-150K lines of C++ (SGML/XML parser)
- Dazzle replaces OpenSP with **libxml2** (XML only, DTD validation included)

### What Dazzle Must Implement

From OpenJade analysis, Dazzle needs:

1. **224 Scheme primitives** (detailed below)
   - ~90 from R5RS (Steel provides)
   - ~104 DSSSL-specific (implement in Rust)
   - ~30 DSSSL types (stubs only - not needed for code generation)
2. **Grove query engine** (~50 core primitives)
3. **Processing & output** (~20 primitives for text generation)
4. **SGML backend concept** (simplified: only `entity` + `formatting-instruction` flow objects)
5. **R5RS Scheme interpreter** (use Steel, not port)
6. **Template file parser** (XML wrapper with entity references to .scm modules)

**Key simplification**: User only generates plain text code files (`.java`, `.rs`, etc.), not styled documents. This eliminates ~30 primitives (quantities, colors, spacing) - implement as stubs.

### What Dazzle Does NOT Need

- ❌ OpenSP parser (use libxml2 instead)
- ❌ Other FOT builders (HTML, RTF, TeX, MIF)
- ❌ DSSSL style language (flow objects, characteristics for document formatting)
- ❌ SGML-wrapped template parsing (`<style-specification>`)
- ❌ Document rendering features

---

## Primitive Catalog

**Total primitives in OpenJade**: 224 (from `style/primitive.h`)

### Steel Provides (R5RS Standard): ~90 primitives ✓

**Already implemented** in Steel Scheme:
- **Lists** (15): `cons`, `car`, `cdr`, `list`, `append`, `reverse`, `length`, `list-tail`, `list-ref`, `member`, `memv`, `assoc`, `null?`, `pair?`, `list?`
- **Strings** (14): `string`, `string-length`, `string=?`, `string<?`, `string<=?`, `string-append`, `string-ref`, `substring`, `symbol->string`, `string->symbol`, `string->list`, `list->string`
- **Numbers** (42): Arithmetic, comparison, transcendental functions, conversions
- **Predicates** (14): Type checks for symbol, boolean, procedure, string, char, number, etc.
- **Logic** (3): `not`, `equal?`, `eqv?`
- **Characters** (5): Comparison and case conversion
- **Vectors** (8): `vector?`, `vector`, `vector-ref`, `vector-set!`, `make-vector`, conversions

### Dazzle Must Implement: ~134 primitives

Organized by priority and function:

#### **CRITICAL: Grove Query Functions** (~50 primitives)

XML tree navigation and querying - see DSSSL spec Section 9 (SDQL) for details:

- **Context**: `current-node`
- **Node lists**: `node-list?`, `node-list-empty?`, `node-list-first`, `node-list-rest`, `node-list`, `node-list-length`, `node-list-ref`, `node-list-reverse`, `node-list-map`, `node-list=?`, `empty-node-list`
- **Properties**: `gi` (element name), `id`, `data` (text), `node-property`
- **Navigation**: `parent`, `ancestor`, `children`, `descendants`, `follow`, `preced`, `attributes`
- **Selection**: `select-elements`, `select-by-class`, `element-with-id`, `match-element?`
- **Attributes**: `attribute-string`, `inherited-attribute-string`, `inherited-element-attribute-string`
- **Position**: `first-sibling?`, `last-sibling?`, `absolute-first-sibling?`, `absolute-last-sibling?`, `have-ancestor?`
- **Numbering**: `child-number`, `ancestor-child-number`, `element-number`, `element-number-list`, `hierarchical-number`, `hierarchical-number-recursive`, `first-child-gi`
- **Entities**: `entity-system-id`, `entity-public-id`, `entity-generated-system-id`, `entity-text`, `entity-notation`, `entity-type`, `entity-attribute-string`
- **Notations**: `notation-system-id`, `notation-public-id`, `notation-generated-system-id`
- **Normalization**: `general-name-normalize`, `entity-name-normalize`

#### **HIGH: Processing & Sosofo** (~20 primitives)

- **Processing**: `process-children`, `process-children-trim`, `process-node-list`, `process-element-with-id`, `process-matching-children`, `process-first-descendant`, `next-match`
- **Sosofo**: `sosofo?`, `empty-sosofo`, `sosofo-append`, `literal`, `sosofo-label`, `sosofo-discard-labeled`
- **Formatting**: `format-number` (I/II/III, 1/2/3, a/b/c), `format-number-list` (1.2.3)
- **Page numbers**: `current-node-page-number-sosofo`, `page-number-sosofo` (may not need)

#### **MEDIUM: DSSSL Types** (~30 primitives) - **MOSTLY STUBS**

Only `entity` and `formatting-instruction` flow objects needed for code generation. Most types implemented as stubs:

- **Quantities** (4): `quantity?`, `table-unit`, `quantity->number`, `quantity->string` - Return dummy values
- **Spacing** (5): `display-space?`, `display-space`, `inline-space?`, `inline-space`, `display-size` - Return dummy values
- **Colors** (4): `color?`, `color`, `color-space?`, `color-space` - Return dummy values
- **Addresses** (8): `address?`, `address-local?`, `address-visited?`, `current-node-address`, `idref-address`, `entity-address`, `sgml-document-address`, `node-list-address` - Return dummy values
- **Glyphs** (5): `glyph-id?`, `glyph-id`, `glyph-subst-table?`, `glyph-subst-table`, `glyph-subst` - Return dummy values
- **Character properties** (2): `char-property`, `char-script-case` - Implement if needed for case conversion

**Implementation**: All stubs initially (1-2 days), implement properly only if templates use them.

#### **LOW: Extensions & Utilities** (~20 primitives)

- **Keywords** (3): `keyword?`, `keyword->string`, `string->keyword`
- **Time** (6): `time`, `time->string`, `time<?`, `time>?`, `time<=?`, `time>=?`
- **Language** (4): `language?`, `current-language`, `with-language`, `language`
- **Style** (3): `style?`, `merge-style`, `map-constructor`
- **Parsing**: `sgml-parse`
- **Debug/error** (4): `error`, `external-procedure`, `read-entity`, `debug`
- **Named node lists** (6): `named-node-list?`, `named-node`, `named-node-list-names`, `named-node-list-normalize`, `node-list-no-order`, `node-list-error`
- **Page conditionals** (3): `if-first-page`, `if-front-page`, `all-element-number`
- **HyTime**: `hytime-linkend`
- **String utilities**: `string-equiv?`

---

## Feature Matrix

What Dazzle implements from each ancestor:

### From DSSSL Standard (ISO 10179)

**Core Expression Language** - ✅ Via Steel Scheme:
- ✅ R5RS Scheme (superset of R4RS/DSSSL)
- ✅ ~90 standard procedures (lists, strings, math, predicates)
- ✅ Basic data types (lists, strings, numbers, booleans, vectors)
- ✅ Procedures (`define`, `lambda`, `let`, `letrec`, `let*`)
- ✅ Conditionals (`if`, `cond`, `case`)
- ✅ Comments (line `;` and block `#| ... |#`)

**NOT implementing (document formatting):**
- ❌ Flow objects for document rendering
- ❌ Style specifications for pagination
- ❌ Formatting characteristics (fonts, spacing for print)
- ❌ SPDL output

**Grove Model** - ✅ Complete implementation:
- ✅ ~50 grove query primitives
- ✅ XML tree navigation (parent, children, ancestors, descendants)
- ✅ Node properties (GI, ID, attributes, data)
- ✅ Pattern matching and selection
- ✅ Entity and notation access

### From Jade/OpenJade

**Scheme Primitives** - ✅ Complete compatibility:
- ✅ All 224 OpenJade primitives
- ✅ ~90 from R5RS (Steel provides)
- ✅ ~134 DSSSL-specific (Dazzle implements)

**Processing & Code Generation:**
- ✅ `load` procedure (OpenJade extension, not DSSSL standard)
- ✅ SGML backend concept (text output generation)
- ✅ Template-based code generation
- ✅ Processing control (`process-children`, `process-node-list`, etc.)
- ✅ Sosofo operations (`literal`, `sosofo-append`, etc.)
- ✅ External procedures (Rust functions callable from Scheme)

**CLI Compatibility:**
- ✅ `-d` template file option
- ✅ `-V` variable definitions
- ✅ `-D` search directories
- ✅ `-t` backend selection: text, xml
- ❌ `-o` output file (template controls output via `write-file`)

**NOT implementing:**
- ❌ Document formatting backends (RTF, TeX, MIF, HTML)
- ❌ SPDL generation
- ❌ Full SGML parsing (XML only, via libxml2)
- ❌ SGML-wrapped templates (`<style-specification>` format)

### Dazzle-Specific Features

**New/Enhanced:**
- ✅ Pure Rust implementation
- ✅ libxml2 for XML + DTD validation
- ✅ Steel Scheme interpreter (R5RS)
- ✅ Pure .scm template files (no SGML overhead)
- ✅ Modern error messages
- ✅ File writing from templates (`write-file`)
- ✅ Multiple output file generation

**Simplified:**
- Stdout output removed (templates write files directly)
- Single backend (code generation only)
- No SGML entity loading (use `load` instead)

---

## Technical Stack

### Language Choices

**Host Language: Rust**
- Modern, maintainable, safe
- Excellent cross-platform support
- Good FFI for libxml2
- Active ecosystem

**Template Language: Scheme (Ported from OpenJade)**
- **Decision**: Port OpenJade's Scheme interpreter to Rust (not use Steel)
- **Rationale**:
  - ⚠️ Steel has parser bugs (whitespace handling - `let` bindings must be on same line)
  - ⚠️ Steel error reporting uses spans (not line numbers) - poor UX for humans
  - ✅ OpenJade's interpreter is proven (25 years production use)
  - ✅ OpenJade is whitespace-agnostic (R4RS compliant)
  - ✅ Porting gives 100% compatibility with existing DSSSL code
  - ✅ Full control over interpreter behavior and error messages
  - ✅ Reasonable scope: ~12,000 lines C++ → ~10,000 lines Rust
- **Effort**: 8-12 weeks part-time vs. ongoing workarounds for Steel bugs
- See: `OPENJADE_INTERPRETER_ANALYSIS.md` for detailed analysis

**XML Parser: libxml2**
- Industry standard (GNOME project)
- Full DTD validation support
- Pure C (clean FFI, not C++)
- Used by xmllint, browsers, editors
- Much safer dependency than OpenSP

### Dependencies

```toml
[dependencies]
libxml = "0.3"              # libxml2 Rust bindings
steel-interpreter = "..."   # Steel Scheme runtime
clap = "4"                  # CLI argument parsing
```

**Philosophy**: Minimal dependencies for easier packaging.

---

## Template File Format: XML Wrappers with Entity References

**CRITICAL**: Templates must work with BOTH OpenJade AND Dazzle for migration compatibility.

### Why Entity References?

Steel Scheme has context isolation with `(load ...)` - each file creates a new context. Entity references let the parser concatenate `.scm` files before the interpreter sees them, following DSSSL tradition.

### Template Format

```xml
<?xml version="1.0"?>
<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY helpers SYSTEM "helpers.scm">
<!ENTITY rules   SYSTEM "rules.scm">
]>
<style-sheet>
<style-specification>
&helpers;
&rules;
</style-specification>
</style-sheet>
```

Both OpenJade/OpenSP and Dazzle/libxml2 parse this identically: resolve entities, extract content from `<style-specification>`, pass unified program to Scheme interpreter.

### CDATA Wrapping

Wrap `.scm` content in `<![CDATA[...]]>` if it contains `<` or `&` characters (Scheme predicates like `string<?`, generated code like `if (a < b)`, strings with `&`). Otherwise, no wrapping needed.

### Migration from OpenJade SGML

Add `<?xml version="1.0"?>` declaration, use lowercase element names (`<style-sheet>`, `<style-specification>`). The `.scm` files remain unchanged.

---

## Architecture Design

### **Vision**: High-Quality DSSSL Processor (Not Just Code Generator)

**Goal**: Port OpenJade's architecture to Rust, preserving its clean separation and extensibility.

**OpenJade's structure** (what we're porting):
```
grove/        → Abstract document tree interface (traits)
spgrove/      → OpenSP implementation of grove (SGML)
style/        → Scheme interpreter + DSSSL engine
jade/         → FOT builders (RTF, TeX, MIF, SGML, HTML backends)
```

**Dazzle's equivalent** (Rust workspace):
```
dazzle/
├── crates/
│   ├── dazzle-core/          → Core interpreter + abstractions
│   │   ├── scheme/           → Scheme interpreter (port of style/)
│   │   ├── dsssl/            → DSSSL style engine
│   │   ├── grove/            → Grove trait (abstract interface)
│   │   └── fot/              → Flow Object Tree trait
│   │
│   ├── dazzle-grove-libxml2/ → XML+DTD grove (initial - priority 1)
│   ├── dazzle-grove-opensp/  → Full SGML grove (future - optional)
│   │
│   ├── dazzle-backend-sgml/  → Code generation (priority 1)
│   ├── dazzle-backend-rtf/   → RTF output (future)
│   ├── dazzle-backend-tex/   → TeX output (future)
│   ├── dazzle-backend-mif/   → MIF output (future)
│   ├── dazzle-backend-html/  → HTML output (future)
│   │
│   └── dazzle-cli/           → Command-line interface
```

### **Key Architectural Principles**

1. **Grove Abstraction** (like OpenJade's grove/spgrove)
   - Define `Node` and `NodeList` traits in `dazzle-core/grove/`
   - Implementations in separate crates (`dazzle-grove-libxml2`, future `dazzle-grove-opensp`)
   - Scheme interpreter only depends on traits, not implementations
   - ✅ Swap grove implementations without changing interpreter
   - ✅ Support both XML (libxml2) and full SGML (OpenSP) long-term

2. **Backend Abstraction** (like OpenJade's FOTBuilder hierarchy)
   - Define `FotBuilder` trait in `dazzle-core/fot/`
   - Backends implement trait: SGML, RTF, TeX, MIF, HTML
   - Same Scheme interpreter for all backends
   - ✅ Add backends incrementally
   - ✅ Users can write custom backends

3. **Faithful Interpreter Port**
   - Port OpenJade's interpreter **structure**, not just functionality
   - Preserve performance optimizations (instruction-based eval, string interning, lazy node lists)
   - Use GC (`gc` crate) to match OpenJade's Collector
   - ✅ Performance on par with OpenJade (Rust should match or beat C++)
   - ✅ 100% compatibility with existing DSSSL code

4. **Clean Separation**
   - Each crate has single responsibility
   - Groves don't know about backends
   - Backends don't know about groves
   - Interpreter orchestrates via traits
   - ✅ Test components independently
   - ✅ Maintainable, modular codebase

### **Grove Trait Design** (Rust equivalent of OpenJade's grove/)

```rust
// dazzle-core/src/grove/mod.rs

/// Represents a node in the document tree (element, text, etc.)
pub trait Node {
    fn gi(&self) -> Option<&str>;                    // Element name
    fn id(&self) -> Option<&str>;                     // ID attribute
    fn data(&self) -> Option<&str>;                   // Text content
    fn parent(&self) -> Option<Box<dyn Node>>;
    fn children(&self) -> Box<dyn NodeList>;
    fn attributes(&self) -> Box<dyn NodeList>;
    fn attribute_string(&self, name: &str) -> Option<String>;
    // ... all DSSSL node properties
}

/// Represents a node list (sequence of nodes)
pub trait NodeList {
    fn first(&self) -> Option<Box<dyn Node>>;
    fn rest(&self) -> Option<Box<dyn NodeList>>;
    fn length(&self) -> usize;
    // ... all DSSSL node-list operations
}

/// Represents the document grove (root + global operations)
pub trait Grove {
    fn root(&self) -> Box<dyn Node>;
    fn element_with_id(&self, id: &str) -> Option<Box<dyn Node>>;
    // ... grove-level operations
}
```

**Implementations**:
- `dazzle-grove-libxml2`: Wrap libxml2's DOM (XML + DTD validation)
- `dazzle-grove-opensp` (future): Wrap OpenSP or port to Rust (full SGML)

### **FOT Builder Trait Design** (Rust equivalent of OpenJade's FOTBuilder)

```rust
// dazzle-core/src/fot/mod.rs

/// Backend for generating output from flow objects
pub trait FotBuilder {
    // SGML backend only needs these two:
    fn entity(&mut self, system_id: &str, content: &str) -> Result<()>;
    fn formatting_instruction(&mut self, data: &str) -> Result<()>;

    // Document formatting backends need these:
    fn start_element(&mut self, gi: &str, attrs: &[(String, String)]) -> Result<()>;
    fn end_element(&mut self) -> Result<()>;
    fn characters(&mut self, data: &str) -> Result<()>;
    // ... all flow objects (paragraph, sequence, display-group, etc.)
}
```

**Implementations**:
- `dazzle-backend-sgml`: File I/O for code generation (priority 1)
- `dazzle-backend-rtf` (future): RTF document output
- `dazzle-backend-tex` (future): TeX document output
- And more...

### **Interpreter Architecture** (Port of OpenJade's style/)

```rust
// dazzle-core/src/scheme/interpreter.rs

pub struct Interpreter {
    // Symbol tables (like OpenJade)
    symbols: SymbolTable,
    identifiers: IdentifierTable,
    units: UnitTable,
    processing_modes: ProcessingModeTable,

    // Singletons
    nil: Value,
    true_val: Value,
    false_val: Value,
    error: Value,

    // Grove (trait object - any implementation)
    grove: Box<dyn Grove>,

    // Backend (trait object - any implementation)
    backend: Box<dyn FotBuilder>,

    // GC
    collector: gc::Gc,
}
```

### **Migration Path**

**Phase 1**: SGML backend + libxml2 grove (code generation - SchwebNet use case)
- ✅ Drop-in replacement for OpenJade on XML projects
- ✅ Validates against existing workflow

**Phase 2**: Additional backends (document formatting)
- RTF, TeX, MIF, HTML backends
- ✅ Full OpenJade feature parity

**Phase 3**: OpenSP grove (full SGML support)
- Option A: FFI to existing OpenSP C++ library
- Option B: Port OpenSP to Rust (ambitious, but valuable to community)
- ✅ True OpenJade replacement (SGML + XML)

---

## CLI Design

### Final Interface

```bash
dazzle -d template.scm [-t xml] [-V key=value]... [-D dir]... input.xml
```

**Flags:**
- `-t xml` - Optional XML backend
- `-d template.scm` - **Required**. Scheme template file
- `-V key=value` - Template variables (repeatable)
- `-D directory` - Template search paths (repeatable)
- `input.xml` - Input XML file(s)

**Automatic Features:**
- DTD validation if `<!DOCTYPE>` present in XML
- Output controlled by template (no `-o` flag)
- Search paths: current dir, `-D` dirs, system dirs

### Examples

```bash
# Basic usage
dazzle -d codegen.scm grammar.xml

# With variables
dazzle -d gen.scm -V package=com.example -V version=1.0 model.xml

# With search paths
dazzle -d template.scm -D /usr/share/dazzle/templates input.xml

# Multiple variables
dazzle -d gen.scm \
  -V package=smartonfhir \
  -V outdir=src/generated \
  -V debug=true \
  grammar.xml
```

### Template File Writing

Templates write files directly:

```scheme
;; In template.scm
(define outdir (get-variable "outdir" "generated"))

(define (generate-class node)
  (let ((name (xml-get-attribute node "name")))
    (write-file 
      (string-append outdir "/" name ".java")
      (generate-java-code name))))
```

---

## Distribution Strategy

### Package Availability: "dazzle"

**Verified Available:**
- ✅ crates.io
- ✅ Debian/Ubuntu repositories
- ✅ Fedora repositories
- ✅ Arch/AUR
- ✅ Homebrew
- ✅ MacPorts
- ✅ openSUSE

**No conflicts found.**

### Release Roadmap

**Phase 1: Foundation (Week 1)**
- Publish to crates.io
- GitHub releases with binaries
- Static musl builds

**Phase 2: Package Managers (Month 1-2)**
- Arch AUR (easiest, fastest)
- Homebrew tap (your own)
- MacPorts submission

**Phase 3: Official Repos (Month 3-6)**
- Homebrew core
- MacPorts official
- Fedora
- Debian (1+ year timeline)

### macOS Priority

**Critical for user:**
- Homebrew dropped OpenJade
- MacPorts still has it, but aging
- Need both for redundancy

**Focus:**
1. **MacPorts** - More conservative, won't drop packages easily
2. **Homebrew** - Larger user base

### Linux Priority

1. **Arch AUR** - Fastest acceptance, Rust-friendly
2. **Fedora** - Active Rust community
3. **openSUSE** - German connection, good for user
4. **Debian** - Long-term stability (slow process)

---

## Implementation Notes

### Key Scheme Extensions for Dazzle

**Steel provides**: R5RS Scheme (core language, lists, strings, numbers, predicates, block comments `#| ... |#`)

**Dazzle must add**:
- `load` - Load .scm files (OpenJade extension)
- `write-file`, `ensure-dir` - File I/O
- `get-variable` - CLI variables
- Grove navigation - `current-node`, `gi`, `children`, `attribute-string`, etc.
- Processing - `process-children`, `literal`, `sosofo-append`, etc.

### DTD Validation

Automatic when `<!DOCTYPE>` present in XML. libxml2 loads and validates against DTD, reports errors before template runs.

### Rust ↔ Steel Bridge

Register Rust functions with Steel VM: file I/O (`write-file`, `ensure-dir`), CLI variables (`get-variable`), XML navigation (`xml-get-attribute`, `xml-children`, `xml-select` with XPath).

---

## Migration from OpenJade

**File changes**: Convert SGML → XML (one-time), pure `.scm` templates (no CDATA in wrapper), keep DTD unchanged.

**Command**: `openjade -t sgml -d codegen.dsl grammar.sgml > Output.java` → `dazzle -d codegen.scm grammar.xml` (output controlled by template)

**Benefits**: Modern tooling, won't disappear from repos, better editor support, cleaner code, cross-platform binaries.

---

## Implementation Roadmap

**Revised Strategy**: Port OpenJade's Scheme interpreter to Rust (not use Steel).

**Timeline**: 3-5 months part-time (10-15 hours/week). **Total effort**: ~12,000 lines C++ → ~10,000 lines Rust.

### Phase 1: Core Architecture & Traits (2-3 weeks)

**Goal**: Define trait-based architecture, set up workspace

**Deliverables**:
1. **Workspace structure**: Multi-crate Cargo workspace
   - `dazzle-core/` (interpreter + traits)
   - `dazzle-grove-libxml2/` (XML grove)
   - `dazzle-backend-sgml/` (code generation)
   - `dazzle-cli/` (command-line)

2. **Trait definitions**:
   - `Grove`, `Node`, `NodeList` traits
   - `FotBuilder` trait
   - Define complete API surface

3. **Value enum** (complete):
   - All Scheme types (nil, bool, integer, real, char, string, symbol, pair, vector, ...)
   - DSSSL types (node-list, sosofo, quantity, ...)
   - GC strategy decided

4. **Decisions**:
   - GC approach (`gc` crate vs. `Rc` vs. custom)
   - Error handling (miette, anyhow, custom)
   - String interning strategy

**Milestone**: Clean architecture, all types defined, traits documented

---

### Phase 2: Scheme Interpreter Core (4-5 weeks)

**Goal**: Port OpenJade's `style/` directory to Rust

**Components**:

1. **Parser** (port `SchemeParser.cxx`):
   - Lexer with tokenization
   - S-expression parser
   - **Whitespace-agnostic** (critical - fix Steel's bug!)
   - Number parsing (integers, reals, quantities)
   - String, character, symbol parsing
   - Quasiquote, unquote support
   - ~2,500 lines C++ → ~2,000 lines Rust

2. **Evaluator** (port `Interpreter.cxx`):
   - Expression compilation
   - Instruction-based evaluation (preserve OpenJade's optimization)
   - Environment handling
   - Symbol/identifier tables
   - ~2,400 lines C++ → ~2,000 lines Rust

3. **Object Model** (port `ELObj.*`):
   - Complete `Value` enum implementation
   - GC integration
   - Equality, printing, type predicates
   - ~1,300 lines C++ → ~1,000 lines Rust

4. **R4RS Primitives** (port relevant parts of `primitive.cxx`):
   - Lists: cons, car, cdr, list, append, reverse, length, member, assoc (~15 functions)
   - Strings: string, string-append, substring, string-ref, string-length (~14 functions)
   - Numbers: +, -, *, /, <, >, =, min, max, floor, sqrt, sin, cos, ... (~42 functions)
   - Predicates: null?, pair?, symbol?, number?, string?, ... (~14 functions)
   - Logic: not, equal?, eqv? (~3 functions)
   - Characters: char=?, char<?, char-upcase, char-downcase (~5 functions)
   - Vectors: vector, vector-ref, vector-set!, make-vector (~8 functions)
   - **Total**: ~90 primitives

**Validation**: Run scheme test files, compare outputs with OpenJade

**Milestone**: Can evaluate Scheme: `(let ((x 5)) (+ x 3))`, load utilities.scm

---

### Phase 3: libxml2 Grove Implementation (3-4 weeks)

**Goal**: XML grove with DTD validation

**Components**:

1. **libxml2 wrapper**:
   - Safe Rust wrapper around libxml2 bindings
   - DTD validation integration
   - Entity resolution

2. **Grove trait implementations**:
   - `LibXml2Node` implementing `Node` trait
   - `LibXml2NodeList` implementing `NodeList` trait
   - `LibXml2Grove` implementing `Grove` trait

3. **Grove primitives** (~50 functions):
   - Context: current-node
   - Properties: gi, id, data, attribute-string, inherited-attribute-string
   - Navigation: parent, ancestor, children, descendants, follow, preced, attributes
   - Selection: select-elements, select-by-class, element-with-id, match-element?
   - Node lists: node-list-first, node-list-rest, node-list-length, node-list-map, ...
   - Position: first-sibling?, last-sibling?, child-number, element-number
   - Entities: entity-system-id, entity-public-id, entity-text
   - Notations: notation-system-id, notation-public-id

**Validation**: Grove query tests against XML files

**Milestone**: `(gi (current-node))` works on Icons.xml

---

### Phase 4: SGML Backend & Processing (3-4 weeks)

**Goal**: Code generation works

**Components**:

1. **SGML backend**:
   - Implement `FotBuilder` trait for code generation
   - `entity` flow object (file I/O)
   - `formatting-instruction` flow object (text output)

2. **Processing primitives** (~20 functions):
   - process-children, process-children-trim, process-node-list
   - process-element-with-id, process-matching-children, process-first-descendant
   - next-match (critical for DSSSL rule chaining)

3. **Sosofo** (~7 functions):
   - sosofo-append, literal, empty-sosofo
   - sosofo-label, sosofo-discard-labeled
   - sosofo?

4. **Utilities**:
   - format-number, format-number-list (I/II/III, 1.2.3 formatting)
   - error, debug
   - load (template file loading)

**Validation**: Icons.xml → Dazzle → compare output to OpenJade

**Milestone**: Dazzle generates Icons output files matching OpenJade

---

### Phase 5: DSSSL Types & Remaining Primitives (2-3 weeks)

**Goal**: 100% primitive compatibility

**Components**:

1. **DSSSL type stubs** (~30 functions):
   - Quantities: quantity?, table-unit, quantity->number
   - Spacing: display-space?, inline-space?
   - Colors: color?, color-space?
   - Addresses: address?, idref-address, entity-address
   - Glyphs: glyph-id?, glyph-subst-table?
   - **Implementation**: Return dummy values (not needed for code generation)

2. **Remaining utilities** (~20 functions):
   - Keywords: keyword?, keyword->string, string->keyword
   - Time: time, time->string, time<?, time>?
   - Language: language?, current-language, with-language
   - Style: style?, merge-style, map-constructor
   - Named node lists: named-node-list?, named-node
   - sgml-parse (may stub out)

**Validation**: All 236 primitives implemented or stubbed

**Milestone**: Full OpenJade primitive compatibility

---

### Phase 6: CLI & Template Loading (2 weeks)

**Goal**: Production-ready CLI

**Components**:

1. **CLI** (using clap):
   - `-d template.dsl` - template file
   - `-V key=value` - variables
   - `-D directory` - search paths
   - `-t backend` - backend selection

2. **Template loading**:
   - XML entity expansion (via libxml2)
   - Extract content from `<style-specification>`
   - Search paths for `load` procedure

3. **Error reporting**:
   - **Line numbers** in errors (not spans!)
   - Stack traces for Scheme errors
   - Use `miette` for beautiful errors

**Validation**: CLI matches OpenJade behavior

**Milestone**: Drop-in replacement for `openjade -t sgml`

---

### Phase 7: SchwebNet Migration & Testing (3-4 weeks)

**Goal**: Production validation

**Work**:

1. **SchwebNet test suite**:
   - Run all SchwebNet XML files through Dazzle
   - Compare outputs to OpenJade byte-for-byte
   - Fix any discrepancies

2. **Performance tuning**:
   - Profile hot paths
   - Optimize grove queries
   - Tune GC if needed
   - **Target**: Within 2x of OpenJade speed (should match or beat with Rust)

3. **Comprehensive tests**:
   - Unit tests for all 236 primitives
   - Integration tests with real .scm files
   - Regression tests (Dazzle vs. OpenJade outputs)

4. **Documentation**:
   - README with quickstart
   - Primitive reference
   - Migration guide from OpenJade
   - Architecture documentation

**Milestone**: SchwebNet fully migrated, Dazzle proven in production

---

### Phase 8: Distribution (Ongoing)

**Immediate** (Week 1 after v1.0):
- Publish to crates.io
- GitHub releases with binaries (Linux, macOS, Windows)
- Static musl builds

**Short-term** (Month 1-2):
- Arch AUR package
- Homebrew tap (user's own)
- MacPorts submission

**Medium-term** (Month 3-6):
- Homebrew core
- MacPorts official
- Fedora package
- openSUSE package

**Long-term** (6+ months):
- Debian/Ubuntu packages

---

### Future Phases (Optional)

**Phase 9: Additional Backends**
- RTF backend (port `jade/RtfFOTBuilder.cxx`) - 2-3 weeks
- TeX backend (port `jade/TeXFOTBuilder.cxx`) - 2-3 weeks
- MIF backend - 2-3 weeks
- HTML backend - 2-3 weeks

**Phase 10: OpenSP Grove** (Full SGML support)
- Option A: FFI to existing OpenSP C++ - 3-4 weeks
- Option B: Port OpenSP to Rust - 6-12 months (massive effort, but community win)

---

### Summary

**Total timeline (part-time, 10-15 hours/week)**: 18-22 weeks (~4-5 months)

**Milestones**:
- ✅ Month 1: Architecture + Scheme interpreter core
- ✅ Month 2: Grove implementation + grove primitives
- ✅ Month 3: SGML backend + processing primitives
- ✅ Month 4: Remaining primitives + CLI
- ✅ Month 5: SchwebNet migration + production validation

**Success criteria**:
- All 236 primitives implemented (90 real + ~30 stubs)
- Identical output to OpenJade on SchwebNet
- Performance within 2x of OpenJade (target: match or beat)
- DTD validation working
- CLI compatible with OpenJade
- Published to crates.io
- Available via package managers (AUR, Homebrew, MacPorts)

### Testing Strategy

**Multi-level testing**:

1. **Unit tests**: Each primitive function tested independently
2. **Parser tests**: Whitespace handling, edge cases, error recovery
3. **Evaluator tests**: Scheme semantics, environment handling
4. **Grove tests**: XML navigation, node properties, DTD validation
5. **Integration tests**: Full .scm files evaluated
6. **Regression tests**: Compare Dazzle vs. OpenJade outputs byte-for-byte
7. **Production validation**: SchwebNet migration

**Test data**:
- OpenJade's own test suite (if available)
- User's real DSSSL templates (utilities.scm, rules.scm, etc.)
- SchwebNet XML files (7.3 MB, 20+ files)
- Edge cases for whitespace handling (multi-line `let`, `letrec`, etc.)

### Success Criteria

**v1.0 Definition**:
- ✅ All 236 primitives implemented or stubbed
- ✅ Identical output to OpenJade on SchwebNet (byte-for-byte)
- ✅ DTD validation working
- ✅ CLI compatible with OpenJade (`-d`, `-V`, `-D`, `-t`)
- ✅ Performance within 2x of OpenJade (target: match or beat)
- ✅ Whitespace-agnostic parser (no Steel bugs!)
- ✅ Human-friendly error messages (line numbers, not spans)
- ✅ Complete documentation (README, primitive reference, migration guide)
- ✅ Published to crates.io
- ✅ Cross-platform binaries (Linux, macOS, Windows)
- ✅ Available in at least one package manager (AUR or Homebrew)

---

## References

### Standards & Specs

- **DSSSL**: ISO/IEC 10179:1996
  - Online: ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf
  - Local copy: `/Users/r.schleitzer/Documents/dsssl96b.pdf`
  - **Key sections for Dazzle implementation**:
    - Section 8: Grove architecture and node properties
    - Section 9: SDQL (Standard Document Query Language) - grove queries
    - Section 10: Processing model - rules, modes, `next-match`
    - Section 6: Data types - quantities, colors, addresses
- **R4RS Scheme**: IEEE Std 1178-1990 (DSSSL base)
- **R5RS Scheme**: http://www.schemers.org/Documents/Standards/R5RS/
- **XML**: W3C XML 1.0 Specification
- **DTD**: Part of XML spec

### Implementations

- **Jade**: https://www.jclark.com/jade/
- **OpenJade**: https://openjade.sourceforge.net/
- **Steel**: https://github.com/mattwparas/steel
- **libxml2**: https://gitlab.gnome.org/GNOME/libxml2

### User's Projects

- **Scaly.io**: Parser generator project
- Custom grammar DTD for parser specifications
- Large existing codebase using OpenJade

---

## Design Decisions Summary

**Architecture**: Multi-crate workspace (dazzle-core, dazzle-grove-*, dazzle-backend-*, dazzle-cli). **Interpreter**: Port OpenJade's Scheme interpreter to Rust (~12K lines C++ → ~10K lines Rust). **Grove**: Trait-based abstraction - libxml2 initially, OpenSP optional future. **Backends**: Trait-based - SGML first (code gen), RTF/TeX/MIF/HTML future. **Scope**: Full DSSSL processor (not just code generator). **Primitives**: All 236 OpenJade (90 R4RS, ~50 grove, ~20 processing, ~30 stubs, ~36 utilities). **CLI**: OpenJade-compatible (`-d`, `-V`, `-D`, `-t`), template controls output. **Performance**: Match or beat OpenJade (Rust should be faster). **Distribution**: crates.io first, then AUR/Homebrew/MacPorts, official repos later. **Implementation**: Faithful port preserving OpenJade's architecture and optimizations. **Timeline**: 4-5 months part-time.

---

## Success Metrics & Future

**Success**: Drop-in OpenJade replacement, 224 primitives identical, available in MacPorts/Homebrew, maintainable Rust codebase.

**Bonus**: REPL mode, watch mode, template debugger, S-expression input, JSON output, template libraries.

**Project Status**: In development | **License**: MIT

---

## Quick Reference

**OpenJade Analysis**:
- 72K lines C++ total (117 files)
- **Core interpreter**: ~12K lines (style/ directory) - **this is what we're porting**
  - Parser: 2,479 lines (SchemeParser.cxx)
  - Interpreter: 2,390 lines (Interpreter.cxx)
  - Object model: 1,314 lines (ELObj.cxx)
  - Primitives: 5,704 lines (primitive.cxx) - 236 functions
- SGML backend: 2,824 lines (jade/SgmlFOTBuilder.cxx)
- OpenSP: 100-150K lines (separate library) → **replaced with libxml2**

**Implementation Strategy**:
- **NOT using Steel** (parser bugs, poor error messages)
- **Port OpenJade interpreter** (~12K C++ → ~10K Rust)
- Trait-based architecture (grove/spgrove pattern)
- Multi-crate workspace (core, grove-libxml2, backend-sgml, cli)
- Faithful port preserving performance optimizations

**Timeline (Part-time, 10-15 hours/week)**:
- 18-22 weeks (~4-5 months)
- **Phase 1** (2-3 weeks): Architecture & traits
- **Phase 2** (4-5 weeks): Scheme interpreter core (parser, eval, 90 primitives)
- **Phase 3** (3-4 weeks): libxml2 grove (50 primitives)
- **Phase 4** (3-4 weeks): SGML backend & processing (20 primitives)
- **Phase 5** (2-3 weeks): DSSSL types & utilities (66 primitives)
- **Phase 6** (2 weeks): CLI
- **Phase 7** (3-4 weeks): SchwebNet migration & testing

**Risks**:
- **Low**: OpenJade design is proven, C++ is straightforward
- **Medium**: GC tuning, instruction-based eval, lazy node lists
- **Mitigation**: Incremental validation, compare outputs, comprehensive tests

**Key Differentiators from Steel**:
- ✅ Whitespace-agnostic parser (R4RS compliant)
- ✅ Line numbers in errors (not spans)
- ✅ 100% OpenJade compatibility (proven design)
- ✅ Full control (no upstream dependency bugs)
- ✅ Extensible architecture (multiple groves, multiple backends)
