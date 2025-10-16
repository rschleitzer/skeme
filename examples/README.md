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

### Grove Primitives (27)

**Node Property Accessors:**
- `grove-root` - Get root node from grove
- `gi` - Get element name
- `id` - Get ID attribute
- `data` - Get text content
- `attribute-string` - Get attribute value by name

**Node Navigation:**
- `children` - Get child nodes as node-list
- `parent` - Get parent node
- `descendants` - Get all descendant nodes (depth-first)

**Node-list Operations:**
- `node-list` - Create node-list from nodes (used with `apply`)
- `node-list-empty?` - Check if node list is empty
- `node-list-first` - Get first node
- `node-list-rest` - Get rest of nodes
- `node-list-length` - Get node list length
- `node-list->list` - Convert node-list to Scheme list
- `empty-node-list` - Create empty node-list
- `node-list-reverse` - Reverse node-list order

**Node-list Filtering and Searching:**
- `select-elements` - Filter node-list by element name
- `select-children` - Select immediate children by element name
- `element-with-id` - Find element by ID attribute anywhere in tree

**Node-list Higher-Order Functions:**
- `node-list-filter` - Filter node-list with predicate function
- `node-list-map` - Map procedure over node-list
- `node-list-some` - Test if any node matches predicate
- `node-list-contains?` - Check if node-list contains specific node
- `node-list-count` - Count nodes in node-list (alias for node-list-length)

**Type Predicates:**
- `element?` - Check if element node
- `text?` - Check if text node

**Context Variables:**
- `current-root` - Root node of current grove (auto-set)
- `current-node` - Current processing node (initially set to current-root)

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

- **Grove navigation** - Full XML tree navigation with 27 primitives
- **Node-list operations** - Filter, map, search with predicate functions
- **File output** - `make-entity` + `write-sosofo` write generated code to files
- **DTD validation** - libxml2 parses and validates against DTDs
- **Template search paths** - `-D dir` flag to specify template directories
- **Variables** - `-V key=value` flags injected as Scheme variables
- **Auto directory creation** - Output directories created automatically

## Practical Examples

### Using select-children

```scheme
;; Get all <field> elements that are immediate children
(define class-node current-root)
(define fields (select-children class-node "field"))

;; Process each field
(define field-list (node-list->list fields))
(map (lambda (field)
       (let ((name (attribute-string field "name"))
             (type (attribute-string field "type")))
         (generate-field name type)))
     field-list)
```

### Using node-list-filter

```scheme
;; Filter fields by predicate
(define all-fields (select-children class-node "field"))

;; Get only required fields
(define required-fields
  (node-list-filter
    (lambda (field)
      (equal? (attribute-string field "required") "true"))
    all-fields))

;; Get only string fields
(define string-fields
  (node-list-filter
    (lambda (field)
      (equal? (attribute-string field "type") "String"))
    all-fields))
```

### Using node-list-map

```scheme
;; Extract all field names
(define field-names
  (node-list-map
    (lambda (field) (attribute-string field "name"))
    all-fields))

;; field-names is now a regular Scheme list of strings
```

### Using element-with-id

```scheme
;; Find element anywhere in tree by ID
(define employee-class (element-with-id current-grove "employee-class"))

(if employee-class
    (let ((name (attribute-string employee-class "name")))
      (display (string-append "Found class: " name)))
    (display "Class not found"))
```

### Using node-list-some

```scheme
;; Check if any field has specific property
(define has-id-field?
  (node-list-some
    (lambda (field)
      (equal? (attribute-string field "name") "id"))
    all-fields))

(if has-id-field?
    (generate-with-id)
    (generate-without-id))
```

### Using node-list-contains?

```scheme
;; Check if specific node is in a filtered list
(define required-fields
  (node-list-filter
    (lambda (f) (equal? (attribute-string f "required") "true"))
    all-fields))

(define name-field (node-list-first (select-children class-node "name")))

(if (node-list-contains? required-fields name-field)
    (display "Name field is required")
    (display "Name field is optional"))
```

### Using current-node

```scheme
;; current-node is automatically set to current-root at start
(define root-name (gi current-node))  ; Get root element name

;; You can rebind current-node for context-dependent processing
(define (process-with-context node)
  (let ((current-node node))  ; Locally rebind
    ;; Now current-node refers to 'node' in this scope
    (process-children current-node)))
```

## Code Reuse with XML Entities

Skeme supports **XML-embedded DSSSL templates** with entity expansion for sharing helper code:

### Quick Example

```xml
<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY helpers SYSTEM "lib/helpers.scm">
]>
<style-sheet>
  <style-specification>
&helpers;

(define code (use-helper-function))
  </style-specification>
</style-sheet>
```

### Running

```bash
skeme -D lib -d template.dsl input.xml
```

Entities are expanded automatically by libxml2. See `XML_DSSSL_GUIDE.md` for complete documentation.

### Examples with Entities

- `codegen-xml.dsl` - Basic XML template with entity includes
- `codegen-enhanced.dsl` - **NEW!** Demonstrates all new node-list primitives:
  - `select-children` for direct child selection
  - `node-list-filter` for filtering with predicates
  - `node-list-map` for extracting values
  - `node-list-some` for conditional logic
  - `node-list-count` for statistics
- `lib/string-utils-simple.scm` - Helper functions (no special chars)
- `lib/string-utils-cdata.scm` - Helper with CDATA (uses `char<?`, `char>=?`, etc.)
- `lib/java-helpers.scm` - Java code generation helpers

**Tip**: Use `<![CDATA[...]]>` anywhere you need `<` and `>` characters. Entity refs (`&name;`) must be outside CDATA.

## Known Limitations

1. **Process-children** - Context-dependent processing not yet implemented
   - Workaround: Use `children` + `select-elements` + `node-list-map`

2. **Advanced grove primitives** - Some DSSSL primitives not yet implemented:
   - `ancestor`, `preced`, `follow`, `follow-sibling`, `preceding-sibling`
   - Can be added if needed for specific use cases

## Next Steps

- Add `process-children` for context-dependent processing (if needed)
- More grove primitives as specific use cases require them
- Performance optimization for large documents
- Standard library of common patterns (as inline examples)

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
