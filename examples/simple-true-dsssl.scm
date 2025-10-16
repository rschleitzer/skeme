;; Simple TRUE DSSSL Example
;; Shows authentic DSSSL syntax - exactly like OpenJade!

;; ============================================================================
;; Construction Rules
;; ============================================================================

;; Root element
(element class
  (make sequence
    (literal "Class: ")
    (literal (attribute-string current-node "name"))
    (literal "\n\nFields:\n")
    (process-children)))

;; Field elements
(element field
  (make sequence
    (literal "  - ")
    (literal (attribute-string current-node "name"))
    (literal " (")
    (literal (attribute-string current-node "type"))
    (literal ")\n")))

;; Default: pass through children
(default (process-children))

;; ============================================================================
;; Start Processing and Write Output
;; ============================================================================

;; Generate report file
(make entity
  system-id: "generated/class-report.txt"
  (process-root))
