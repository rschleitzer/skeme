<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY string-utils SYSTEM "lib/string-utils-simple.scm">
  <!ENTITY java-helpers SYSTEM "lib/java-helpers.scm">
]>
<style-sheet>
  <style-specification>
;; TRUE DSSSL Construction Rules (OpenJade-compatible syntax!)
;; This demonstrates authentic DSSSL with XML entity includes

&string-utils;
&java-helpers;

;; ============================================================================
;; Construction Rules - TRUE DSSSL SYNTAX
;; ============================================================================

;; Root element rule - NO explicit lambda!
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

;; Field element rule
(element field
  (let ((field-name (attribute-string current-node "name"))
        (field-type (attribute-string current-node "type")))
    (make sequence
      (literal (make-field field-name field-type))
      (literal (make-getter field-name field-type))
      (literal (make-setter field-name field-type)))))

;; Default rule for unmatched elements
(default (process-children))

;; ============================================================================
;; Start Processing
;; ============================================================================

(process-root)
  </style-specification>
</style-sheet>
