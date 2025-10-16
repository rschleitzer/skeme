;; TRUE DSSSL Construction Rules
;; This is authentic DSSSL syntax - compatible with OpenJade!

;; Load helper functions
(load "lib/string-utils-simple.scm")
(load "lib/java-helpers.scm")

;; ============================================================================
;; Construction Rules (TRUE DSSSL SYNTAX)
;; ============================================================================

;; Root element: class
;; Notice: NO explicit (lambda () ...) wrapper!
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
      ;; Private field declaration
      (literal (make-field field-name field-type))
      ;; Getter method
      (literal (make-getter field-name field-type))
      ;; Setter method
      (literal (make-setter field-name field-type)))))

;; Default rule: skip text nodes and unmatched elements
(default (empty-sosofo))

;; ============================================================================
;; Start Processing
;; ============================================================================

;; Process the document tree starting from root
;; In OpenJade, this is typically the last expression
(process-root)
