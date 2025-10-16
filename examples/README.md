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

### Grove Primitives (17)
- `grove-root` - Get root node from grove
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
- `select-elements` - Filter node-list by element name
- `descendants` - Get all descendant nodes (depth-first)

### Processing Primitives (6)
- `literal` - Create text sosofo
- `empty-sosofo` - Create empty sosofo
- `sosofo-append` - Combine sosofos
- `make-entity` - Create entity flow object (file output)
- `make-formatting-instruction` - Create formatting instruction (text output)
- `write-sosofo` - Write sosofo to its associated file

### R5RS Scheme (via Steel)
All R5RS primitives are available:
- List operations: `list`, `map`, `filter`, `apply`, etc.
- String operations: `string-append`, `string-length`, etc.
- Arithmetic: `+`, `-`, `*`, `/`, etc.
- I/O: `display`, `newline`, etc.

## Working Features ✓

- **Grove navigation** - Full XML tree navigation with 17 primitives
- **File output** - `make-entity` + `write-sosofo` write generated code to files
- **DTD validation** - libxml2 parses and validates against DTDs
- **Template search paths** - `-D dir` flag to specify template directories
- **Variables** - `-V key=value` flags injected as Scheme variables
- **Auto directory creation** - Output directories created automatically

## Known Limitations

1. **Load primitive** - `load` for loading helper libraries not yet implemented
   - Workaround: Use Steel's module system or inline helpers

2. **Process-children** - Context-dependent processing not yet implemented
   - Workaround: Use `children` + `select-elements` + `map`

3. **Advanced grove primitives** - Some DSSSL primitives not yet implemented:
   - `element-with-id`, `ancestor`, `preced`, `follow`, etc.
   - XPath support would be nice but not in DSSSL spec

## Next Steps

- Implement `load` primitive for modular templates
- Add `process-children` for context-dependent processing
- More grove primitives as needed
- Performance optimization for large documents

## Template Structure

A typical Skeme template:

```scheme
;; Access the XML root (automatically available as current-root)
(define class-node current-root)
(define class-name (attribute-string class-node "name"))

;; Navigate the XML tree
(define all-children (children class-node))
(define fields (select-elements all-children "field"))
(define methods (select-elements all-children "method"))

;; Access CLI variables
;; Passed with: -V outdir=generated -V package=com.example
;; Available in Scheme as: outdir, package

;; Generate code
(define code (string-append
  "package " package ";\n\n"
  "public class " class-name " {\n"
  "  // Generated code\n"
  "}\n"))

;; Write to file
(define output-file (string-append outdir "/" class-name ".java"))
(define output-sosofo (make-entity output-file (literal code)))
(write-sosofo output-sosofo)

;; Return success
#t
```

See `examples/codegen-improved.scm` for a complete working example.

## Testing

Run the test suite:

```bash
cargo test --workspace
```

All 22 tests should pass.
