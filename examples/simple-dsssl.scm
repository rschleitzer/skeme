;; Simple TRUE DSSSL construction rules example
;; Generates a text report from XML data - OpenJade compatible!

;; ============================================================================
;; Construction Rules - TRUE DSSSL SYNTAX
;; ============================================================================

;; Root element rule
;; Notice: 'class' is a SYMBOL, not a string!
;; Notice: NO (lambda () ...) wrapper!
(element class
  (let ((name (attribute-string current-node "name")))
    (make sequence
      (literal (string-append "Class: " name "\n"))
      (literal "Fields:\n")
      (process-children))))

;; Field element rule
(element field
  (let ((name (attribute-string current-node "name"))
        (type (attribute-string current-node "type")))
    (literal (string-append "  - " name " (" type ")\n"))))

;; Default rule: pass through children
(default (process-children))

;; ============================================================================
;; Start Processing and Write Output
;; ============================================================================

;; Generate report file using make entity
(make entity
  system-id: "generated/class-report.txt"
  (process-root))
