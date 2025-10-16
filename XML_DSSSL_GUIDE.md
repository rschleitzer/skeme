# XML-Embedded DSSSL Templates

## Overview

Skeme supports XML-embedded DSSSL templates with **XML entity expansion** for code reuse, following the **DSSSL philosophy** (ISO/IEC 10179:1996) of embedding Scheme code in markup, but using modern XML instead of SGML.

## Benefits

✅ **Standard XML** - Uses W3C XML 1.0 entities (not custom preprocessors)
✅ **libxml2 expansion** - Entities expanded automatically by parser
✅ **DSSSL philosophy** - Scheme embedded in markup (same `<style-sheet>/<style-specification>` structure)
✅ **Clean migration** - SGML → XML just needs closing tags
✅ **No CDATA needed** - Entity content merged directly into document

## Basic Structure

```xml
<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY helpers SYSTEM "lib/helpers.scm">
]>
<style-sheet>
  <style-specification>
;; DSSSL/Scheme code here

&helpers;

;; More code
  </style-specification>
</style-sheet>
```

## Entity Inclusion

### Define Entities in DOCTYPE

```xml
<!DOCTYPE style-sheet [
  <!ENTITY string-utils SYSTEM "lib/string-utils.scm">
  <!ENTITY java-helpers SYSTEM "lib/java-helpers.scm">
]>
```

### Reference Entities in Code

```xml
<style-specification>
;; Include helper libraries
&string-utils;
&java-helpers;

;; Use the included functions
(define getter-code (make-getter "name" "String"))
</style-specification>
```

**Important**: Entity references (`&name;`) must be **outside CDATA sections**. Entity expansion only works in parsed content, not in CDATA.

## Complete Example

### Helper File: `lib/java-helpers.scm`

```scheme
(define (make-getter field-name field-type)
  (string-append
    "    public " field-type " get" (capitalize field-name) "() {\n"
    "        return " field-name ";\n"
    "    }\n"))

(define (make-setter field-name field-type)
  (string-append
    "    public void set" (capitalize field-name)
    "(" field-type " " field-name ") {\n"
    "        this." field-name " = " field-name ";\n"
    "    }\n"))
```

### Template: `codegen.dsl`

```xml
<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY java-helpers SYSTEM "lib/java-helpers.scm">
]>
<style-sheet>
  <style-specification>
&java-helpers;

(define class-node current-root)
(define class-name (attribute-string class-node "name"))
(define fields (select-elements (children class-node) "field"))

;; Generate Java POJO
(define java-code
  (string-append
    "public class " class-name " {\n"
    ;; ... generate fields, getters, setters ...
    "}"))

(write-sosofo (make-entity "generated/MyClass.java" (literal java-code)))
  </style-specification>
</style-sheet>
```

### Usage

```bash
skeme -d codegen.dsl -D lib input.xml
```

The `-D lib` flag tells Skeme to search for entity files in the `lib/` directory.

## How It Works

1. **Parse XML**: libxml2 parses the template file
2. **Load Entities**: `SYSTEM` entities loaded from filesystem
3. **Expand References**: `&helpers;` replaced with file content
4. **Extract Code**: Text from `<style-specification>` extracted
5. **Evaluate**: Expanded Scheme code passed to Steel interpreter

All entity expansion happens in step 2-3 by libxml2, before Steel sees the code.

## CDATA Sections

CDATA sections protect special characters (`<`, `>`, `&`) from XML parsing. You can use them anywhere.

### Entity References and CDATA

**Important**: Entity references (`&name;`) don't expand inside CDATA:

```xml
<style-specification>
  <![CDATA[
    &helpers;  <!-- NOT expanded - stays as literal "&helpers;" -->
  ]]>
</style-specification>
```

**Correct**: Put entity references outside CDATA:

```xml
<style-specification>
&helpers;  <!-- Expanded by XML parser -->

<![CDATA[
;; Now use < and > freely
(if (char<? ch #\z) ...)
]]>
</style-specification>
```

## Using `<` and `>` Characters

Scheme code often needs `<` and `>` characters (`char<?`, `<=`, etc.), but XML treats `<` as the start of a tag. Use CDATA sections to protect these characters anywhere they appear.

### CDATA Sections

Wrap any Scheme code containing `<` or `>` in `<![CDATA[...]]>`:

```xml
<style-specification>
<![CDATA[
;; All operators work in CDATA
(define (in-range? ch)
  (and (char>=? ch #\a) (char<=? ch #\z)))

(if (char<? my-char #\Z)
    "uppercase"
    "not uppercase")
]]>
</style-specification>
```

### In Helper Files

Helper files are also XML (when used as entities), so use CDATA there too:

```scheme
<!-- lib/helpers.scm -->
<![CDATA[
(define (compare-chars ch1 ch2)
  (char<? ch1 ch2))
]]>
```

### Mixing Entities and CDATA

Entity references must be **outside** CDATA (entities don't expand in CDATA):

```xml
<style-specification>
&string-utils;  <!-- Entity reference - must be outside CDATA -->

<![CDATA[
;; CDATA protects < and >
(if (char<? ch #\z)
    (capitalize str))
]]>
</style-specification>
```

The helper file itself can contain CDATA:

```scheme
<!-- lib/string-utils.scm -->
<![CDATA[
(define (capitalize str) ...)
]]>
```

## Entity Search Paths

Entities are resolved relative to:

1. Template file's directory
2. Directories specified with `-D` flags

```bash
# Search for entities in multiple locations
skeme -D /usr/share/skeme/lib -D ./mylib -d template.dsl input.xml
```

## Migration from SGML/OpenJade

DSSSL (the ISO standard) specifies that Scheme code is embedded in SGML `<style-sheet>` documents. OpenJade implemented this faithfully.

### Before (SGML):

```sgml
<!DOCTYPE style-sheet SYSTEM "dsssl.dtd" [
  <!ENTITY helpers SYSTEM "helpers.scm">
]>
<style-sheet>
  <style-specification>
    <![CDATA[
      ;; Code here...
    ]]>
  </style-specification>
</style-sheet>
```

### After (XML):

```xml
<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY helpers SYSTEM "helpers.scm">
]>
<style-sheet>
  <style-specification>
;; Code here - no CDATA needed!
&helpers;
  </style-specification>
</style-sheet>
```

**Changes**:
- Add `<?xml version="1.0"?>` declaration
- Remove `SYSTEM "dsssl.dtd"` (Skeme doesn't validate against DSSSL DTD)
- Remove `<![CDATA[...]]>` (entities don't expand in CDATA)
- Add closing tags if missing (`</style-sheet>`, etc.)

## Pure Scheme Files Still Supported

You can still use pure `.scm` files:

```bash
skeme -d template.scm input.xml
```

Skeme auto-detects:
- XML if file starts with `<?xml` or `<`
- Pure Scheme otherwise

## Technical Details

- Entity expansion uses libxml2's `XML_PARSE_NOENT` flag
- External entities loaded with `XML_PARSE_DTDLOAD`
- XPath `//style-specification` extracts DSSSL code
- No circular entity protection needed (libxml2 handles it)
- Entity files can include other entities (nested expansion works)

## Best Practices

1. **Organize helpers by purpose**: `string-utils.scm`, `java-gen.scm`, etc.
2. **Use CDATA liberally**: Wrap any Scheme code with `<` or `>` in `<![CDATA[...]]>`
3. **Entity refs outside CDATA**: Always put `&name;` references outside CDATA sections
4. **Use search paths** (`-D`) for portable templates
5. **Document dependencies** at top of template file

## Example Project Structure

```
project/
├── lib/
│   ├── string-utils.scm
│   ├── java-naming.scm
│   └── code-templates.scm
├── templates/
│   ├── pojo-gen.dsl          # XML template
│   ├── interface-gen.dsl     # XML template
│   └── classic-gen.scm       # Pure Scheme (also works!)
└── specs/
    ├── Person.xml
    └── Order.xml

# Generate code
$ skeme -D lib -d templates/pojo-gen.dsl specs/Person.xml
```

## Troubleshooting

**Problem**: Entities not expanded

- **Check**: Are entity refs inside CDATA in the main template? (Move outside)
- **Check**: Is helper file path correct? (Use `-D` flag)
- **Check**: Does helper file exist and is readable?

**Problem**: XML parse error in helper file - "unexpected <"

- **Cause**: Helper has `<` or `>` characters like `char<?`
- **Fix**: Wrap helper content in `<![CDATA[...]]>` - see "Using < and > in Helper Files" section above

**Problem**: "No <style-specification> found"

- **Check**: XML template has `<style-specification>` element?
- **Check**: Proper nesting: `<style-sheet><style-specification>...`

**Problem**: Functions from helper not defined

- **Check**: Entity reference `&name;` is outside CDATA in main template
- **Check**: Helper file content is wrapped in CDATA (if using special chars)

## Summary

Skeme's XML entity approach gives you:

- ✅ **Standard technology** (XML 1.0 entities)
- ✅ **Familiar to DSSSL users** (same structure as OpenJade)
- ✅ **No custom syntax** (pure W3C XML)
- ✅ **Parser handles complexity** (libxml2 does the work)
- ✅ **Clean migration path** (SGML → XML minimal changes)

It's "alter Hut" (old hat) in the best way - proven, standard XML technology!
