# Skeme Examples

This directory contains example templates demonstrating Skeme's capabilities.

## Quick Start

```bash
# Build skeme
cargo build --release

# Run a basic example
./target/release/skeme -d examples/hello.scm examples/hello.xml

# Run with variables
./target/release/skeme -d examples/demo.scm -V project=myapp -V version=1.0 examples/hello.xml
```

## Examples

### hello.scm

Basic template demonstrating:
- Scheme evaluation
- Arithmetic operations
- SOSOFO creation (`literal`, `make-formatting-instruction`)
- SOSOFO composition (`sosofo-append`)

```bash
./target/release/skeme -d examples/hello.scm examples/hello.xml
```

### demo.scm

More comprehensive demonstration:
- CLI variables (passed with `-V name=value`)
- All SOSOFO primitives
- R5RS Scheme features (lists, string operations)
- Grove primitives availability

```bash
./target/release/skeme -d examples/demo.scm -V project=myapp examples/hello.xml
```

## Available Primitives

### Grove Primitives (14)
- `gi` - Get element name
- `id` - Get ID attribute
- `data` - Get text content
- `attribute-string` - Get attribute value
- `children` - Get child nodes
- `parent` - Get parent node
- `node-list-empty?` - Check if node list is empty
- `node-list-first` - Get first node
- `node-list-rest` - Get rest of nodes
- `node-list-length` - Get node list length
- `element?` - Check if element node
- `text?` - Check if text node

### Processing Primitives (5)
- `literal` - Create text sosofo
- `empty-sosofo` - Create empty sosofo
- `sosofo-append` - Combine sosofos
- `make-entity` - Create entity flow object (file output)
- `make-formatting-instruction` - Create formatting instruction (text output)

### R5RS Scheme (via Steel)
All R5RS primitives are available:
- List operations: `list`, `map`, `filter`, `apply`, etc.
- String operations: `string-append`, `string-length`, etc.
- Arithmetic: `+`, `-`, `*`, `/`, etc.
- I/O: `display`, `newline`, etc.

## Current Limitations

**Phase 1 MVP** - The following features are not yet implemented:

1. **Grove access from templates** - Templates cannot yet navigate the parsed XML tree. The grove is parsed but not exposed to Scheme.

2. **File output** - `make-entity` is defined but doesn't actually write files yet.

3. **Process-children** - Context-dependent processing primitives not yet implemented.

4. **DTD validation** - libxml2 DTD validation is wired up but not yet fully tested.

## Next Steps (Phase 2)

- Inject parsed grove into Scheme environment
- Implement actual file writing from `make-entity`
- Add `process-children` and processing context
- Implement more grove primitives (select-elements, element-with-id, etc.)
- Add template search paths (`-D` option)
- Implement `load` primitive for loading helper libraries

## Template Structure

A typical Skeme template:

```scheme
;; Load helper libraries (when implemented)
;; (load "lib/helpers.scm")

;; Access CLI variables
;; Passed with: -V outdir=generated -V package=com.example
;; Available as: outdir, package

;; Process the grove (when implemented)
;; (define root (current-grove))
;; (define sections (select-elements root "section"))

;; Generate output
(define content (literal "Generated code here"))
(make-entity "output.txt" content)

;; Return success
#t
```

## Testing

Run the test suite:

```bash
cargo test --workspace
```

All 22 tests should pass.
