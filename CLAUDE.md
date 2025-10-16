# CLAUDE.md - Skeme Project Context

> **Context document for Claude Code and future development sessions**
> 
> This document provides complete background on the Skeme project, including motivation, design decisions, technical specifications, and implementation guidance.

## Table of Contents

1. [Project Overview](#project-overview)
2. [Motivation & History](#motivation--history)
3. [DSSSL Lineage](#dsssl-lineage)
4. [Feature Matrix](#feature-matrix)
5. [Technical Stack](#technical-stack)
6. [CLI Design](#cli-design)
7. [Distribution Strategy](#distribution-strategy)
8. [Implementation Notes](#implementation-notes)

---

## Project Overview

**Skeme** (pronounced like "Scheme") is a Rust-based code generation tool powered by Scheme templates. It reimplements the essential functionality of OpenJade's SGML backend for modern systems.

### Key Facts

- **Name**: Skeme (intentional spelling - works as noun and verb: "I skeme the templates")
- **Purpose**: Template-driven code generation from XML/SGML input
- **Language**: Rust (host) + Scheme (templates)
- **Primary Use Case**: fire code generation

### Why "Skeme"?

- Sounds like "Scheme" - honest about the technology
- Looks like "schema" - structural/template connotation
- Works as imperative verb - "skeme this template"
- Available everywhere (crates.io, all distros, Homebrew, MacPorts)
- No trademark conflicts

---

## Motivation & History

### The Problem

OpenJade (and its predecessor Jade) are disappearing from package managers:

- **Homebrew**: Already dropped OpenJade
- **MacPorts**: Still available, but aging
- **Linux distros**: Maintenance declining
- **Codebase**: Unmaintained C++ from 1990s

### User's Context

- Using OpenJade since 1997 (28 years)
- Initially: SGML documentation generation
- Since 2003: **Code generation via SGML backend**
- Current project: **fire** (FHIR 5 server in Rust)
- Large existing codebase depending on OpenJade workflow

### The Solution: Skeme

Preserve the workflow in a maintainable form:

- ✅ Pure Rust implementation (maintainable, cross-platform)
- ✅ Scheme-powered (keep template language)
- ✅ XML + DTD validation (libxml2)
- ✅ OpenJade CLI-compatible (zero retraining)
- ✅ Focus on code generation (not document formatting)
- ✅ Drop SGML-as-spec overhead (use pure .scm files)

---

## DSSSL Lineage

Understanding what Skeme inherits and what it doesn't:

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

## Feature Matrix

What Skeme implements from each ancestor:

### From DSSSL Standard (ISO 10179)

**Core Expression Language:**
- ✅ R4RS Scheme subset (side-effect-free)
- ✅ Basic data types (lists, strings, numbers, booleans)
- ✅ Procedures (`define`, `lambda`, `let`, `letrec`)
- ✅ Conditionals (`if`, `cond`, `case`)
- ✅ List operations (`cons`, `car`, `cdr`, `map`, etc.)
- ✅ String operations
- ✅ Math operations

**NOT implementing (document formatting):**
- ❌ Flow objects
- ❌ Style specifications
- ❌ Formatting characteristics
- ❌ SPDL output

**Partial implementation:**
- 🔸 Grove model (simplified for code generation)
- 🔸 SDQL concepts (adapted for XML navigation)

### From Jade/OpenJade

**Core Features:**
- ✅ `load` procedure (OpenJade extension, not DSSSL standard)
- ✅ SGML backend concept (adapted to XML)
- ✅ Template-based code generation
- ✅ External procedures (Rust functions callable from Scheme)

**CLI Compatibility:**
- ✅ `-d` template file option
- ✅ `-V` variable definitions
- ✅ `-D` search directories
- ✅ `-t` backend selection: text, xml
- ❌ `-o` output file (template controls output)

**NOT implementing:**
- ❌ Document formatting backends (RTF, TeX, MIF)
- ❌ SPDL generation
- ❌ Full SGML parsing (XML only, via libxml2)

### Skeme-Specific Features

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

**Template Language: Scheme (via Steel)**
- **Steel**: R5RS Scheme implementation in Rust
- Chosen because:
  - ✅ R5RS compatible (superset of R4RS/DSSSL)
  - ✅ Actively maintained
  - ✅ Designed for Rust embedding
  - ✅ Production-ready
  - ✅ Good documentation
- Why not scheme-rs: Too new (2025), async-focused, less mature

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

### Project Structure

```
skeme/
├── skeme-core/      # libxml2 + Steel integration
├── skeme-template/  # Template engine, file writing
├── skeme-cli/       # Command-line interface
├── examples/        # Example templates
├── tests/           # Integration tests
└── docs/            # Documentation
```

---

## CLI Design

### Final Interface

```bash
skeme -d template.scm [-t xml] [-V key=value]... [-D dir]... input.xml
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
skeme -d codegen.scm grammar.xml

# With variables
skeme -d gen.scm -V package=com.example -V version=1.0 model.xml

# With search paths
skeme -d template.scm -D /usr/share/skeme/templates input.xml

# Multiple variables
skeme -d gen.scm \
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

### Package Availability: "skeme"

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

### Scheme Features

**From R4RS (DSSSL base):**
```scheme
; Core language
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Lists
(map (lambda (x) (* x 2)) '(1 2 3))  ; => (2 4 6)

; Strings
(string-append "Hello" " " "World")  ; => "Hello World"
```

**From R5RS (Steel provides):**
```scheme
; Line comments (R4RS)
; This is a comment

; Block comments (R5RS)
#|
Multi-line block comment
Can nest: #| inner |#
|#

; S-expression comments (R5RS)
#; (this whole expression is ignored
     (even if multi-line))
```

**Extensions needed for Skeme:**
```scheme
; load - OpenJade extension (not DSSSL standard)
(load "helpers.scm")

; File I/O - Skeme additions
(write-file "Output.java" content)
(ensure-dir "src/generated")

; Variables from CLI
(get-variable "package" "com.default")

; XML navigation - Skeme API
(xml-get-attribute node "name")
(xml-children node)
(xml-select node "//rule")
```

### Template Pattern

**Typical Skeme template:**

```scheme
;; codegen.scm - Main template

; Load utilities
(load "lib/xml-helpers.scm")
(load "lib/java-naming.scm")

; Main generation function
(define (generate-class node)
  (let* ((name (xml-get-attribute node "name"))
         (filename (string-append outdir "/" name ".java")))
    (make entity system-id: filename
      (make formatting-instruction (string-append
        "package " package ";\n\n"
        "public class " name " {\n"
        "  // Generated code\n"
        "}\n")))))

### XML + DTD Handling

**Automatic DTD validation:**

```xml
<?xml version="1.0"?>
<!DOCTYPE grammar SYSTEM "grammar.dtd">
<grammar>
  <rule name="expression">
    <!-- ... -->
  </rule>
</grammar>
```

When Skeme parses this:
1. libxml2 sees `<!DOCTYPE>`
2. Loads `grammar.dtd` automatically
3. Validates XML against DTD
4. Reports errors if validation fails
5. Only passes validated document to template

**No flag needed - it just works!**

### Bridge Functions (Rust ↔ Scheme)

**What Rust must expose to Steel:**

```rust
// File I/O
vm.register_fn("write-file", |path: String, content: String| {
    std::fs::write(path, content)
});

vm.register_fn("ensure-dir", |path: String| {
    std::fs::create_dir_all(path)
});

// CLI variables
vm.register_fn("get-variable", |key: String, default: String| {
    variables.get(&key).cloned().unwrap_or(default)
});

// XML navigation (simplified)
vm.register_fn("xml-get-attribute", |node: Node, name: String| {
    node.get_attribute(&name)
});

vm.register_fn("xml-children", |node: Node| {
    node.children()
});

vm.register_fn("xml-select", |node: Node, xpath: String| {
    node.select(&xpath)  // XPath queries
});
```

---

## Migration from OpenJade

### File Organization

**Before (OpenJade):**
```
project/
├── grammar.sgml        # SGML source
├── grammar.dtd         # DTD
└── codegen.dsl         # DSSSL spec (SGML format!)
    <!DOCTYPE style-sheet ...>
    <style-sheet>
      <style-specification>
        <![CDATA[
        (define (foo) ...)  ; Scheme in CDATA
        ]]>
      </style-specification>
    </style-sheet>
```

**After (Skeme):**
```
project/
├── grammar.xml         # XML source (converted from SGML)
├── grammar.dtd         # DTD (unchanged!)
└── codegen.scm         # Pure Scheme template
    ;; No SGML overhead!
    (define (foo) ...)
```

### Command Migration

**Before:**
```bash
openjade -t sgml -d codegen.dsl grammar.sgml > Output.java
```

**After:**
```bash
skeme -d codegen.scm grammar.xml
# Output.java written by template
```

### Benefits of Migration

**Technical:**
- ✅ No SGML escaping (`<` is just `<`, not `&lt;`)
- ✅ No CDATA sections
- ✅ Pure Scheme files (better editor support)
- ✅ Block comments available (`#| ... |#`)
- ✅ Cleaner, more maintainable code

**Operational:**
- ✅ Won't disappear from package managers
- ✅ Modern tooling (Rust ecosystem)
- ✅ Active maintenance
- ✅ Cross-platform binaries
- ✅ Better error messages

---

## Development Workflow

### Using Claude Code

**Initial setup:**

- Set up Skeme workspace with three crates:
- skeme-core: libxml2 + Steel integration
- skeme-template: Template engine
- skeme-cli: CLI interface
- Dependencies: libxml2, Steel, clap"

**Incremental development:**

- Integrate Steel Scheme interpreter, 
- expose basic file I/O functions to Scheme

# Add XML support
- Add libxml2 integration with DTD validation,
- expose XML DOM to Scheme for navigation

# Build CLI
- Create CLI matching OpenJade interface:
-d template, -V variables, -D search paths"

### Testing Strategy

**Use existing templates as test cases:**
- Convert SGML → XML (one-time with `opensp -x`)
- Run both OpenJade and Skeme
- Compare outputs
- Fix discrepancies

**Test pyramid:**
```
Integration Tests (existing templates)
        ↑
    Feature Tests (new functionality)
        ↑
    Unit Tests (Rust functions)
```

---

## References

### Standards & Specs

- **DSSSL**: ISO/IEC 10179:1996 - ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf
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

**Language Decisions:**
- Rust (not C++) - Maintainability, safety, modern tooling
- Steel (not scheme-rs) - Proven, R5RS, designed for embedding
- libxml2 (not pure Rust) - Only mature DTD validation option

**Feature Decisions:**
- Code generation only (not document formatting)
- XML only (not SGML)
- Pure .scm templates (not SGML specs)
- `load` over SGML entities
- Template-controlled output (no stdout redirection)

**CLI Decisions:**
- Drop `-o` (template controls output)
- Keep `-d`, `-V`, `-D` (OpenJade compatible)
- Automatic DTD validation (no flag needed)

**Distribution Decisions:**
- crates.io first (fast, foundational)
- AUR + Homebrew tap (quick adoption)
- MacPorts priority (stability, won't drop it)
- Official repos later (once proven)

---

## Success Metrics

**Project succeeds if:**

1. ✅ User can replace OpenJade with Skeme
3. ✅ Available in MacPorts + Homebrew (won't disappear)
4. ✅ Can build from source anywhere
5. ✅ Easier to maintain than OpenJade
6. ✅ Community can contribute (Rust vs C++)

**Bonus success:**
- Others adopt Skeme for code generation
- Templates shared as libraries
- Used beyond original use case

---

## Future Possibilities

**Not in scope for v1.0, but possible later:**

- **S-expression input**: Alternative to XML
- **More Scheme implementations**: Make backend pluggable
- **Pure Rust DTD**: If libxml2 becomes problematic
- **Template library**: Reusable code generators
- **REPL mode**: Interactive template development
- **Watch mode**: Auto-regenerate on file changes
- **Language bindings**: Call Skeme from other languages

---

## Contact & Contributing

**Project Status**: In development  
**Target**: v1.0 release when feature-complete  
**License**: MIT
