<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY string-utils SYSTEM "lib/string-utils-simple.scm">
  <!ENTITY java-helpers SYSTEM "lib/java-helpers.scm">
]>
<style-sheet>
  <style-specification>
;; TRUE DSSSL template with construction rules - OpenJade compatible!
;; Notice: GI as SYMBOL, no explicit lambdas, authentic make expressions

&string-utils;
&java-helpers;

;; ============================================================================
;; Construction Rules - TRUE DSSSL SYNTAX
;; ============================================================================

;; Root element: class
;; Notice: 'class' is a SYMBOL, not a string!
;; Notice: NO (lambda () ...) wrapper!
(element class
  (let ((class-name (attribute-string current-node "name"))
        (package-name (attribute-string current-node "package")))
    (make entity
      system-id: (string-append "generated/" class-name ".java")
      (make sequence
        (literal (string-append "package " package-name ";\n\n"))
        (literal (string-append "public class " class-name " {\n\n"))
        (process-children)
        (literal "}\n")))))

;; Field element: generate private field + getter + setter
(element field
  (let ((field-name (attribute-string current-node "name"))
        (field-type (attribute-string current-node "type")))
    (make sequence
      (literal (make-field field-name field-type))
      (literal (make-getter field-name field-type))
      (literal (make-setter field-name field-type)))))

;; Default rule: skip text nodes and unmatched elements
(default (empty-sosofo))

;; ============================================================================
;; Start Processing
;; ============================================================================

;; Process the document tree starting from root
(process-root)
  </style-specification>
</style-sheet>
