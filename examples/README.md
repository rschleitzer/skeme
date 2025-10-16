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

### simple-true-dsssl.scm - **TRUE DSSSL!** ✓

Authentic DSSSL syntax - OpenJade compatible!
- **`(element gi body...)`** - Real DSSSL syntax (no explicit lambdas)
- **`(make sequence ...)`** - Flow object construction
- **`(make entity system-id: "file" ...)`** - File output
- `process-children` recursion
- Default rule handling

```bash
./target/release/skeme -d examples/simple-true-dsssl.scm examples/class.xml
```

### true-dsssl.scm - **TRUE DSSSL!** ✓

Full Java code generator with TRUE DSSSL syntax:
- Authentic DSSSL construction rules
- OpenJade-compatible syntax
- Hash table rule lookup (O(1) performance)
- Helper function includes

```bash
./target/release/skeme -d examples/true-dsssl.scm examples/class.xml
```

### true-dsssl.dsl - **TRUE DSSSL + XML!** ✓

XML-embedded TRUE DSSSL template:
- Authentic DSSSL in XML `<style-specification>`
- Entity includes for helper functions
- OpenJade-compatible construction rules
- Ready for migration from OpenJade!

```bash
./target/release/skeme -d examples/true-dsssl.dsl examples/class.xml
```

### simple-modes.scm - **DSSSL MODES!** ✓

Demonstrates DSSSL processing modes:
- Same element processed differently in different contexts
- `(mode name ...)` to define mode-specific rules
- `(with-mode name body)` to switch modes
- Perfect for generating TOC + content, summaries + details

```bash
./target/release/skeme -d examples/simple-modes.scm examples/class.xml
```

### modes-example.scm - **ADVANCED MODES!** ✓

Full Java generator using modes:
- TOC mode for table of contents
- Default mode for full implementation
- Shows real-world mode usage
- OpenJade-compatible mode semantics

```bash
./target/release/skeme -d examples/modes-example.scm examples/class.xml
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

## TRUE DSSSL Construction Rules

Skeme supports **authentic DSSSL syntax** - fully compatible with OpenJade!

```scheme
;; Define rules for each element type - TRUE DSSSL SYNTAX!
;; Notice: NO explicit (lambda () ...) wrappers

(element class
  (make entity
    system-id: (string-append "generated/" (attribute-string current-node "name") ".java")
    (make sequence
      (literal "public class ")
      (literal (attribute-string current-node "name"))
      (literal " {\n")
      (process-children)
      (literal "}\n"))))

(element field
  (make sequence
    (literal "  private ")
    (literal (attribute-string current-node "type"))
    (literal " ")
    (literal (attribute-string current-node "name"))
    (literal ";\n")))

;; Default rule for unmatched elements
(default (process-children))

;; Start processing from root
(process-root)
```

### Key Features:

- **`(element gi body...)`** - TRUE DSSSL syntax (GI as symbol, not string!)
- **`(make flow-class characteristics... content...)`** - Flow object construction
- **`(make sequence ...)`** - Concatenate sosofos
- **`(make entity system-id: "file.txt" ...)`** - Write to file
- **`(process-children)`** - Recursively apply rules to children
- **`(process-root)`** - Start processing from document root
- **`(default body...)`** - Fallback rule for unmatched elements
- **`current-node`** - Automatically rebound during rule execution
- **Association list rule lookup** - O(n) but works with all Scheme implementations

See `examples/simple-true-dsssl.scm` and `examples/true-dsssl.dsl` for complete examples.

## DSSSL Modes - **NEW!**

Process the same element differently in different contexts using **modes**:

```scheme
;; Default mode - full details
(element field
  (literal (string-append "Field: " (attribute-string current-node "name") "\n")))

;; Summary mode - just names
(mode summary
  (element field
    (literal (string-append "  - " (attribute-string current-node "name") "\n")))

  (default (process-children)))

;; Use modes
(element class
  (make sequence
    (literal "SUMMARY:\n")
    (with-mode summary (process-children))  ;; Process in summary mode
    (literal "\nDETAILS:\n")
    (process-children)))                     ;; Process in default mode
```

### Mode Features:

- **`(mode name rules...)`** - Define mode-specific rules
- **`(with-mode name body...)`** - Process with specific mode active
- **Nested modes** - Modes can switch to other modes
- **Per-mode defaults** - Each mode can have its own default rule
- **OpenJade compatible** - Authentic DSSSL mode semantics

See `examples/simple-modes.scm` and `examples/modes-example.scm` for complete examples.

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

### DSSSL Construction Rules (TRUE DSSSL!) - **NEW!**

**Macros (authentic DSSSL syntax):**
- **`(element gi body...)`** - Define construction rule (GI as symbol, NOT string!)
- **`(make flow-class characteristics... content...)`** - Create flow object
  - `(make sequence ...)` - Concatenate sosofos
  - `(make entity system-id: "file.txt" ...)` - Write to file
- **`(default body...)`** - Define default rule for unmatched elements

**Mode support (OpenJade compatible):**
- **`(mode name rules...)`** - Define mode-specific rules
- **`(with-mode name body...)`** - Process in specific mode
- Modes enable different processing for same elements in different contexts
- Each mode has its own element rules and default rule
- Modes can be nested (switch modes within modes)

**Processing functions:**
- `process-root` - Start processing from document root
- `process-children` - Process children of current-node
- `process-node` - Apply construction rule to specific node
- `process-node-list` - Apply construction rules to node-list

**Performance:**
- Association list rule lookup - O(n) per mode
- Optimized for typical template sizes
- Compatible with OpenJade construction rule semantics

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

1. **Pattern matching** - Advanced pattern matching in element rules
   - DSSSL supports complex patterns: `(element (or field method) ...)`
   - Skeme currently uses simple GI symbol matching
   - Can be added if needed for specific use cases

2. **Advanced grove primitives** - Some DSSSL primitives not yet implemented:
   - `ancestor`, `preced`, `follow`, `follow-sibling`, `preceding-sibling`
   - Can be added if needed for specific use cases

3. **Flow object characteristics** - Limited flow object support
   - DSSSL has rich characteristics: `font-size:`, `font-weight:`, etc.
   - Skeme focuses on code generation (sequence, entity)
   - Document formatting flow objects can be added if needed

## Next Steps

- Add pattern matching for element rules: `(element (or field method) ...)`
- Add more grove primitives as specific use cases require them:
  - `ancestor`, `preceding-sibling`, `following-sibling`
  - `preced`, `follow` (document order navigation)
- Performance optimization for very large documents
- Standard library of common patterns
- More flow object classes if document formatting needed

## Template Structure

Skeme supports two styles of templates:

### Style 1: TRUE DSSSL (Construction Rules) - Recommended!

```scheme
;; Authentic DSSSL syntax - compatible with OpenJade!
;; Notice: GI is a SYMBOL, not a string
;; Notice: NO explicit (lambda () ...) wrappers

(element class
  (make entity
    system-id: (string-append "generated/" (attribute-string current-node "name") ".java")
    (make sequence
      (literal "public class ")
      (literal (attribute-string current-node "name"))
      (literal " {\n")
      (process-children)
      (literal "}\n"))))

(element field
  (make sequence
    (literal "  private ")
    (literal (attribute-string current-node "type"))
    (literal " ")
    (literal (attribute-string current-node "name"))
    (literal ";\n")))

(default (process-children))

;; Start processing
(process-root)
```

### Style 2: Imperative (Direct Navigation)

A typical imperative Skeme template:

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
