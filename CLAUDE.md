# CLAUDE.md - Skeme Project Context

> **Context document for Claude Code and future development sessions**
> 
> This document provides complete background on the Skeme project, including motivation, design decisions, technical specifications, and implementation guidance.

## Table of Contents

1. [Project Overview](#project-overview)
2. [Motivation & History](#motivation--history)
3. [DSSSL Lineage](#dsssl-lineage)
4. [OpenJade Analysis](#openjade-analysis)
5. [Primitive Catalog](#primitive-catalog)
6. [Feature Matrix](#feature-matrix)
7. [Technical Stack](#technical-stack)
8. [CLI Design](#cli-design)
9. [Distribution Strategy](#distribution-strategy)
10. [Implementation Roadmap](#implementation-roadmap)

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

**Critical files for Skeme:**
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
- Skeme replaces OpenSP with **libxml2** (XML only, DTD validation included)

### What Skeme Must Implement

From OpenJade analysis, Skeme needs:

1. **224 Scheme primitives** (detailed below)
   - ~90 from R5RS (Steel provides)
   - ~104 DSSSL-specific (implement in Rust)
   - ~30 DSSSL types (stubs only - not needed for code generation)
2. **Grove query engine** (~50 core primitives)
3. **Processing & output** (~20 primitives for text generation)
4. **SGML backend concept** (simplified: only `entity` + `formatting-instruction` flow objects)
5. **R5RS Scheme interpreter** (use Steel, not port)
6. **Template file parser** (pure .scm files, no SGML wrapper)

**Key simplification**: User only generates plain text code files (`.java`, `.rs`, etc.), not styled documents. This eliminates ~30 primitives (quantities, colors, spacing) - implement as stubs.

### What Skeme Does NOT Need

- ❌ OpenSP parser (use libxml2 instead)
- ❌ Other FOT builders (HTML, RTF, TeX, MIF)
- ❌ DSSSL style language (flow objects, characteristics for document formatting)
- ❌ SGML-wrapped template parsing (`<style-specification>`)
- ❌ Document rendering features

---

## Primitive Catalog

**Total primitives in OpenJade**: 224 (from `style/primitive.h`)

### Steel Provides (R5RS Standard): ~90 primitives ✓

These are **already implemented** in Steel Scheme:

**Lists & Pairs** (15):
- `cons`, `car`, `cdr`, `list`, `append`, `reverse`, `length`
- `list-tail`, `list-ref`, `member`, `memv`, `assoc`
- `null?`, `pair?`, `list?`

**Strings** (14):
- `string`, `string-length`, `string=?`, `string<?`, `string<=?`
- `string-append`, `string-ref`, `substring`
- `symbol->string`, `string->symbol`
- `string->list`, `list->string`

**Numbers & Math** (42):
- Arithmetic: `+`, `-`, `*`, `/`, `quotient`, `remainder`, `modulo`
- Comparison: `=`, `<`, `>`, `<=`, `>=`
- Functions: `min`, `max`, `floor`, `ceiling`, `truncate`, `round`, `abs`
- Transcendental: `sqrt`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `expt`
- Conversions: `number->string`, `string->number`, `exact->inexact`, `inexact->exact`

**Predicates** (14):
- `symbol?`, `boolean?`, `procedure?`, `string?`, `char?`
- `number?`, `integer?`, `real?`
- `exact?`, `inexact?`, `zero?`, `positive?`, `negative?`, `odd?`, `even?`

**Logic & Comparison** (3):
- `not`, `equal?`, `eqv?`

**Characters** (5):
- `char=?`, `char<?`, `char<=?`, `char-upcase`, `char-downcase`

**Vectors (R5RS)** (8):
- `vector?`, `vector`, `vector-ref`, `vector-set!`
- `make-vector`, `vector->list`, `list->vector`, `vector-fill!`

### Skeme Must Implement: ~134 primitives

Organized by priority and function:

#### **CRITICAL: Grove Query Functions** (~50 primitives)

XML tree navigation and querying:

```scheme
;; Current context
(current-node)                              ; Current node being processed

;; Node list operations
(node-list? obj)                            ; Is this a node list?
(node-list-empty? nl)                       ; Is node list empty?
(node-list-first nl)                        ; First node
(node-list-rest nl)                         ; Rest of list
(node-list nl ...)                          ; Create node list
(node-list-length nl)                       ; Length
(node-list-ref nl n)                        ; Get nth node
(node-list-reverse nl)                      ; Reverse
(node-list-map proc nl)                     ; Map over nodes
(node-list=? nl1 nl2)                       ; Compare
(empty-node-list)                           ; Empty list

;; Node properties
(gi [node])                                 ; Element name (GI = Generic Identifier)
(id [node])                                 ; ID attribute
(data node)                                 ; Text content
(node-property propname node [default])     ; Get property

;; Tree navigation
(parent [node])                             ; Parent node
(ancestor gi [node])                        ; Ancestor with GI
(children node)                             ; Child nodes
(descendants node)                          ; All descendants
(follow node)                               ; Following siblings
(preced node)                               ; Preceding siblings
(attributes node)                           ; Attribute nodes

;; Selection & filtering
(select-elements nl patterns)               ; Select by patterns
(select-by-class nl class)                  ; Select by class
(element-with-id id [node])                 ; Find by ID
(match-element? pattern node)               ; Pattern match

;; Attributes
(attribute-string name [node])              ; Get attribute value
(inherited-attribute-string name [node])    ; Inherited attribute
(inherited-element-attribute-string gi name [node]) ; From ancestor

;; Position predicates
(first-sibling? [node])                     ; Is first sibling?
(last-sibling? [node])                      ; Is last sibling?
(absolute-first-sibling? [node])            ; Absolutely first?
(absolute-last-sibling? [node])             ; Absolutely last?
(have-ancestor? gi [node])                  ; Has ancestor?

;; Numbering (for auto-numbering)
(child-number [node])                       ; Position among siblings
(ancestor-child-number gi [node])           ; Ancestor position
(element-number [node])                     ; Number of this type
(element-number-list gi-list [node])        ; Multi-level numbering
(hierarchical-number gi-list [node])        ; Hierarchical (1.2.3)
(hierarchical-number-recursive gi [node])   ; Recursive hierarchical
(first-child-gi [node])                     ; GI of first child

;; Entity operations
(entity-system-id name [node])              ; Entity system ID
(entity-public-id name [node])              ; Entity public ID
(entity-generated-system-id name [node])    ; Generated system ID
(entity-text name [node])                   ; Entity text
(entity-notation name [node])               ; Entity notation
(entity-type name [node])                   ; Entity type
(entity-attribute-string name attr [node])  ; Entity attribute

;; Notation operations
(notation-system-id name [node])            ; Notation system ID
(notation-public-id name [node])            ; Notation public ID
(notation-generated-system-id name [node])  ; Generated system ID

;; Name normalization
(general-name-normalize name [node])        ; Normalize name
(entity-name-normalize name [node])         ; Normalize entity name
```

#### **HIGH: Processing & Sosofo** (~20 primitives)

Control code generation flow:

```scheme
;; Processing control
(process-children)                          ; Process child nodes
(process-children-trim)                     ; Process, trim whitespace
(process-node-list nl)                      ; Process node list
(process-element-with-id id)                ; Process by ID
(process-matching-children patterns ...)    ; Process matching children
(process-first-descendant patterns ...)     ; Process first descendant
(next-match [style])                        ; Continue to next rule

;; Sosofo (Specification of Sequence of Flow Objects)
(sosofo? obj)                               ; Is sosofo?
(empty-sosofo)                              ; Empty sosofo
(sosofo-append sosofo ...)                  ; Append sosofos
(literal str ...)                           ; Output literal text
(sosofo-label label sosofo)                 ; Label sosofo
(sosofo-discard-labeled label sosofo)       ; Discard labeled

;; Formatting
(format-number n format)                    ; Format number (I, II, III, 1, 2, 3, a, b, c)
(format-number-list nums sep format)        ; Format list (1.2.3)

;; Page numbers (may not need for code generation)
(current-node-page-number-sosofo)           ; Current page number
(page-number-sosofo)                        ; Page number sosofo
```

#### **MEDIUM: DSSSL Types** (~30 primitives) - **MOSTLY STUBS**

**User's use case**: Only `entity` and `formatting-instruction` flow objects for plain text code generation.

**Result**: Most DSSSL types are **NOT NEEDED**. Implement as stubs returning defaults.

```scheme
;; Quantities (12pt, 2em, etc.) - ❌ NOT NEEDED (document layout)
(quantity? obj)                             ; Stub: always #f
(table-unit n)                              ; Stub: return simple wrapper
(quantity->number q)                        ; Stub: return 0
(quantity->string q [unit])                 ; Stub: return "0"

;; Spacing - ❌ NOT NEEDED (typographic layout)
(display-space? obj)                        ; Stub: always #f
(display-space min [opt max])               ; Stub: return dummy object
(inline-space? obj)                         ; Stub: always #f
(inline-space min [opt max])                ; Stub: return dummy object
(display-size)                              ; Stub: return (0 0)

;; Colors - ❌ NOT NEEDED (no styling in plain code files)
(color? obj)                                ; Stub: always #f
(color colorspace components ...)           ; Stub: return dummy object
(color-space? obj)                          ; Stub: always #f
(color-space name)                          ; Stub: return dummy object

;; Addresses - 🔸 MAYBE (cross-references in docs?)
(address? obj)                              ; Stub: always #f
(address-local? addr)                       ; Stub: always #t
(address-visited? addr)                     ; Stub: always #f
(current-node-address)                      ; Could return node ID if needed
(idref-address idref)                       ; Stub: return dummy
(entity-address name)                       ; Stub: return dummy
(sgml-document-address sysid docgi)         ; Stub: return dummy
(node-list-address nl)                      ; Stub: return dummy

;; Glyphs - 🔸 MAYBE (special chars in identifiers?)
(glyph-id? obj)                             ; Stub: always #f
(glyph-id pubid str)                        ; Stub: return dummy
(glyph-subst-table? obj)                    ; Stub: always #f
(glyph-subst-table alist)                   ; Stub: return dummy
(glyph-subst glyph table)                   ; Stub: return glyph unchanged

;; Character properties - 🔸 MAYBE (char classification)
(char-property char prop [lang])            ; Could implement for case conversion
(char-script-case char default ...)         ; Stub: return default
```

**Implementation priority**: All stubs initially (1-2 days), implement properly only if actually used in templates.

#### **LOW: Extensions & Utilities** (~20 primitives)

OpenJade-specific or less common:

```scheme
;; Keywords
(keyword? obj)                              ; Is keyword?
(keyword->string kw)                        ; To string
(string->keyword str)                       ; From string

;; Time
(time)                                      ; Current time
(time->string time [format])                ; Format time
(time<? t1 t2)                              ; Compare
(time>? t1 t2)
(time<=? t1 t2)
(time>=? t1 t2)

;; Language
(language? obj)                             ; Is language?
(current-language)                          ; Get current
(with-language lang proc)                   ; Execute with language
(language code country)                     ; External proc

;; Style
(style? obj)                                ; Is style?
(merge-style styles ...)                    ; Merge styles
(map-constructor name proc)                 ; Map constructor

;; Parsing
(sgml-parse sysid [args ...])               ; Parse document

;; Debug/error
(error msg)                                 ; Signal error
(external-procedure name)                   ; Get external proc
(read-entity name)                          ; Read entity
(debug obj)                                 ; Debug output

;; Named node lists (rarely used)
(named-node-list? obj)
(named-node name nnl)
(named-node-list-names nnl)
(named-node-list-normalize nnl names norm)
(node-list-no-order nl)
(node-list-error msg default)

;; Page conditionals
(if-first-page then else)                   ; First page?
(if-front-page then else)                   ; Front page?
(all-element-number [node])                 ; All element numbers

;; HyTime
(hytime-linkend)                            ; HyTime linkend

;; String utilities
(string-equiv? s1 s2 lang)                  ; Language-aware compare
```

---

## Feature Matrix

What Skeme implements from each ancestor:

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
- ✅ ~134 DSSSL-specific (Skeme implements)

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

## Implementation Roadmap

### Overview

**Total scope**: ~134 Rust functions to implement (~104 real + ~30 stubs) + integration code

**Estimated timeline**: 3-6 months (solo, experienced Rust developer)

**Key simplification**: User only uses `entity` + `formatting-instruction` flow objects for plain text code generation. No document formatting, colors, spacing, or page layout needed.

### Phase 1: Foundation (2-3 weeks)

**Goal**: Basic project structure + minimal viable product

**Tasks:**
1. Set up Cargo workspace with three crates:
   - `skeme-core`: libxml2 + Steel integration
   - `skeme-template`: Template engine, file I/O
   - `skeme-cli`: Command-line interface

2. Dependencies:
   ```toml
   libxml = "0.3"                 # libxml2 bindings
   steel-core = "0.5"             # Steel Scheme interpreter
   clap = { version = "4", features = ["derive"] }
   ```

3. Implement **~30 critical primitives**:
   - Grove basics: `current-node`, `gi`, `id`, `children`, `parent`, `attribute-string`, `data`
   - Node lists: `node-list`, `node-list-empty?`, `node-list-first`, `node-list-rest`
   - Processing: `process-children`, `literal`, `sosofo-append`, `empty-sosofo`
   - Selection: `select-elements`, `element-with-id`

4. Basic file I/O:
   - `write-file`, `ensure-dir`
   - `get-variable` (CLI variables)
   - `load` (load .scm files)

**Deliverable**: Can run simple templates that traverse XML and output text.

### Phase 2: Complete Grove Support (3-4 weeks)

**Goal**: Full XML navigation and querying

**Tasks:**
1. Implement **~50 grove primitives**:
   - Tree navigation: `ancestor`, `descendants`, `follow`, `preced`, `attributes`
   - Position predicates: `first-sibling?`, `last-sibling?`, `have-ancestor?`
   - Numbering: `child-number`, `element-number`, `hierarchical-number`
   - Entities: `entity-system-id`, `entity-public-id`, etc.
   - Notations: `notation-system-id`, `notation-public-id`, etc.

2. Pattern matching:
   - `select-elements` with pattern support
   - `match-element?` predicate
   - Pattern parser

3. Node list operations:
   - `node-list-map`, `node-list-ref`, `node-list-reverse`, `node-list-length`
   - Lazy evaluation where possible

**Deliverable**: Full OpenJade grove query compatibility.

### Phase 3: Processing & Sosofo (2-3 weeks)

**Goal**: Template processing control and output generation

**Tasks:**
1. Implement **~20 processing primitives**:
   - `process-node-list`, `process-element-with-id`
   - `process-matching-children`, `process-first-descendant`
   - `next-match` (template rule chaining)
   - `process-children-trim` (whitespace handling)

2. Sosofo system:
   - Sosofo data structure (concatenation of output fragments)
   - `sosofo-append`, `sosofo-label`, `sosofo-discard-labeled`
   - `literal` (text output)

3. Formatting helpers:
   - `format-number` (I, II, III, 1, 2, 3, a, b, c, etc.)
   - `format-number-list` (1.2.3 hierarchical numbering)

**Deliverable**: Complete code generation workflow working.

### Phase 4: DSSSL Types (1-2 days) - **SIMPLIFIED**

**Goal**: Stub implementations for DSSSL types (not needed for code generation)

**User's use case**: Only `entity` + `formatting-instruction` flow objects → plain text output

**Tasks:**
1. Implement **~30 type primitives as STUBS**:
   - Quantities: Return dummy values (not used for code generation)
   - Colors: Return dummy values (not used for code generation)
   - Addresses: Return dummy values (rarely used)
   - Glyphs: Return dummy values (rarely used)
   - Spacing: Return dummy values (not used for code generation)
   - Character properties: Stub `char-script-case`, maybe implement `char-property`

2. Stub implementation pattern:
   ```rust
   vm.register_fn("quantity?", |_obj| false);
   vm.register_fn("color?", |_obj| false);
   vm.register_fn("display-size", || vec![0, 0]);
   // etc.
   ```

3. **Only implement properly if templates actually use them** (unlikely)

**Deliverable**: All type primitives callable (return sensible defaults), templates don't error.

### Phase 5: Extensions & Utilities (1-2 weeks)

**Goal**: Remaining OpenJade primitives

**Tasks:**
1. Implement **~20 utility primitives**:
   - Keywords: `keyword?`, `keyword->string`, `string->keyword`
   - Time: `time`, `time->string`, `time<?`, etc.
   - Language: `language?`, `current-language`, `with-language`
   - Style: `style?`, `merge-style`
   - Debug: `error`, `debug`
   - Named node lists (if needed)

2. Optional features:
   - `sgml-parse` (parse nested documents)
   - `read-entity` (read entity content)
   - Page conditionals (if needed: `if-first-page`, `if-front-page`)

**Deliverable**: 100% OpenJade primitive compatibility.

### Phase 6: CLI & Integration (1-2 weeks)

**Goal**: Complete CLI tool

**Tasks:**
1. Command-line interface:
   - `-d template.scm` - Template file
   - `-V key=value` - Variables (repeatable)
   - `-D directory` - Search paths (repeatable)
   - `-t xml` - Backend selection
   - Input XML file(s)

2. Template loading:
   - Search path resolution
   - `load` procedure integration
   - Error reporting

3. XML parsing:
   - libxml2 integration
   - DTD validation (automatic if `<!DOCTYPE>` present)
   - Error reporting

**Deliverable**: Complete CLI tool, OpenJade-compatible interface.

### Phase 7: Testing & Documentation (2-4 weeks)

**Goal**: Production-ready release

**Tasks:**
1. Testing:
   - Unit tests for each primitive
   - Integration tests with real templates
   - Compare output with OpenJade
   - Test with user's actual templates

2. Documentation:
   - README with quick start
   - Primitive reference
   - Migration guide (OpenJade → Skeme)
   - Template examples

3. Packaging:
   - Publish to crates.io
   - GitHub releases with binaries
   - Static musl builds for Linux
   - macOS universal binaries

**Deliverable**: v1.0 release.

### Phase 8: Distribution (Ongoing, 1-6 months)

**Goal**: Available in package managers

**Tasks:**
1. **Immediate** (Week 1-2):
   - Publish to crates.io
   - GitHub releases
   - Documentation site

2. **Short-term** (Month 1-2):
   - Arch AUR package
   - Homebrew tap (personal)
   - MacPorts submission

3. **Medium-term** (Month 3-6):
   - Homebrew core
   - MacPorts official
   - Fedora package
   - openSUSE package

4. **Long-term** (6+ months):
   - Debian package (slow process)
   - Ubuntu PPA

### Testing Strategy

**Test pyramid:**
```
Production Tests (user's actual templates)
        ↑
Integration Tests (synthetic templates)
        ↑
    Primitive Tests (unit tests)
        ↑
    Type Tests (Rust ↔ Steel conversions)
```

**Compatibility testing:**
1. Convert user's SGML → XML (one-time: `opensp -x`)
2. Run both OpenJade and Skeme on same XML
3. Compare outputs
4. Fix discrepancies
5. Add regression test

### Success Criteria

**Phase completion checklist:**

- [ ] Phase 1: Can generate simple code from XML
- [ ] Phase 2: Full XML navigation works
- [ ] Phase 3: Complex templates with processing control work
- [ ] Phase 4: DSSSL types all functional
- [ ] Phase 5: All 224 primitives implemented
- [ ] Phase 6: CLI matches OpenJade interface
- [ ] Phase 7: User's templates work identically to OpenJade
- [ ] Phase 8: Available in at least 2 package managers

**v1.0 Release criteria:**
1. ✅ All 224 primitives working
2. ✅ User's actual templates generate identical output to OpenJade
3. ✅ DTD validation working
4. ✅ CLI compatible with OpenJade
5. ✅ Documentation complete
6. ✅ Published to crates.io
7. ✅ Binaries for macOS, Linux, Windows

---

## References

### Standards & Specs

- **DSSSL**: ISO/IEC 10179:1996
  - Online: ftp://ftp.jclark.com/pub/dsssl/dsssl96b.pdf
  - Local copy: `/Users/r.schleitzer/Documents/dsssl96b.pdf`
  - **Key sections for Skeme implementation**:
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

**Language Decisions:**
- **Rust** (not C++) - Maintainability, safety, modern tooling, active ecosystem
- **Steel Scheme** (not scheme-rs, not port OpenJade interpreter) - Proven, R5RS-compliant, designed for embedding, actively maintained
- **libxml2** (not pure Rust, not OpenSP) - Only mature option for DTD validation, industry standard

**Scope Decisions:**
- **Code generation only** (not document formatting)
  - Implement SGML backend concept (text output)
  - No RTF, TeX, MIF, HTML backends
  - No flow objects for pagination
- **XML only** (not SGML input)
  - Use libxml2 for parsing
  - Keep DTD validation
  - One-time SGML → XML conversion acceptable
- **Pure .scm templates** (not SGML-wrapped `<style-specification>`)
  - R5RS Scheme files with block comments
  - No CDATA sections needed
  - Better editor support

**Primitive Decisions:**
- **All 224 OpenJade primitives** for full compatibility
- **~90 from R5RS** via Steel (lists, strings, math, etc.)
- **~134 custom implementations** (grove queries, processing, DSSSL types)
- **Priority**: Grove navigation > Processing > Types > Utilities

**CLI Decisions:**
- **Drop `-o` flag** - Template controls output via `write-file`
- **Keep `-d`, `-V`, `-D`** - OpenJade compatible interface
- **Automatic DTD validation** - No flag needed, triggered by `<!DOCTYPE>`
- **Search paths** - Current dir, `-D` dirs, system dirs

**Distribution Decisions:**
- **crates.io first** - Fast, foundational, developer audience
- **AUR + Homebrew tap** - Quick adoption, early feedback
- **MacPorts priority** - Stability, won't drop packages easily
- **Official repos later** - Once proven (Homebrew core, Fedora, Debian)

**Implementation Decisions:**
- **Use Steel, not port interpreter** - 3-4 months saved, R5RS compliance
- **Implement primitives as Rust functions** - Type safety, performance
- **libxml2 FFI** - Battle-tested, not reinvent parser
- **Phased development** - MVP first, then expand
- **Test against user's templates** - Real-world validation

---

## Success Metrics

**Project succeeds if:**

1. ✅ **Drop-in replacement** - User can replace OpenJade with Skeme for their existing templates
2. ✅ **Full compatibility** - All 224 OpenJade primitives work identically
3. ✅ **Output identical** - Templates generate same code as OpenJade
4. ✅ **Available in package managers** - MacPorts + Homebrew minimum (won't disappear)
5. ✅ **Build from source anywhere** - Pure Rust + libxml2 (standard dependency)
6. ✅ **Easier to maintain** - Modern codebase, good documentation
7. ✅ **Community can contribute** - Rust vs C++, clear architecture

**Bonus success:**
- **Others adopt Skeme** - Beyond original use case
- **Template libraries** - Shared, reusable code generators
- **Extended features** - REPL mode, watch mode, template debugger
- **Multiple backends** - S-expression input, JSON output, etc.

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

---

## Analysis Summary (Updated 2025-01-16)

### Key Findings from OpenJade Analysis

1. **OpenJade codebase**: ~72,000 lines C++ (not including OpenSP)
2. **OpenSP dependency**: Separate project, ~100-150K lines (not needed - use libxml2)
3. **Total primitives**: 224 Scheme functions
   - 90 from R5RS (Steel provides)
   - 134 DSSSL-specific (Skeme must implement)
4. **SGML backend**: 2,824 lines (core feature to preserve)
5. **Critical files**:
   - `style/primitive.h` - All 224 primitives defined
   - `style/primitive.cxx` - 5,704 lines of implementations
   - `jade/SgmlFOTBuilder.cxx` - 2,824 lines backend logic

### Implementation Strategy

**Chosen approach**: Hybrid (reimplementation with Steel)

**What we use:**
- ✅ Steel Scheme (R5RS interpreter) - ~90 primitives free
- ✅ libxml2 (XML + DTD validation) - battle-tested
- ✅ Pure Rust (modern, safe, maintainable)

**What we implement:**
- ✅ ~134 DSSSL primitives as Rust functions
- ✅ Grove query engine (XML navigation)
- ✅ Processing control (sosofo, template rules)
- ✅ DSSSL types (quantities, colors, addresses, glyphs)
- ✅ File I/O and CLI integration

**What we drop:**
- ❌ OpenSP parser (use libxml2 instead)
- ❌ OpenJade interpreter (use Steel instead)
- ❌ Other backends (RTF, TeX, MIF, HTML)
- ❌ SGML-wrapped templates (pure .scm files)
- ❌ Document formatting features

### Effort Estimate

**Timeline**: 3-6 months (solo, experienced Rust developer)

**Breakdown**:
- Foundation: 2-3 weeks
- Grove support: 3-4 weeks
- Processing & Sosofo: 2-3 weeks
- DSSSL types: **1-2 days** (stubs only - major time saver!)
- Extensions: 1-2 weeks
- CLI integration: 1-2 weeks
- Testing & docs: 2-4 weeks
- Distribution: Ongoing (1-6 months)

**Key milestone**: MVP in ~6-8 weeks (Phases 1-3)
**Near-complete**: ~10-12 weeks (Phases 1-6)

### Risk Mitigation

**Low risks**:
- Steel Scheme integration - well-documented, designed for embedding
- libxml2 FFI - stable, widely used
- Primitive implementation - straightforward Rust functions

**Medium risks**:
- Pattern matching complexity - requires parser
- Node list lazy evaluation - performance critical
- Template rule chaining (`next-match`) - requires careful design

**Mitigation**:
- Start with user's actual templates (real-world validation)
- Compare output with OpenJade at each phase
- Build test suite from day one

### Next Steps

1. **Phase 1 start**: Set up Cargo workspace
2. **Implement MVP**: ~30 critical primitives
3. **Test with simple template**: Validate approach
4. **Iterate**: Add primitives based on user needs
5. **Full compatibility**: All 224 primitives working
