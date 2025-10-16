;; Simple DSSSL Modes Example
;; Shows how the same element can be processed differently

;; ============================================================================
;; Default Mode - Full Details
;; ============================================================================

(element class
  (make sequence
    (literal "=== CLASS REPORT ===\n\n")

    ;; Generate summary first (using summary mode)
    (literal "SUMMARY:\n")
    (with-mode summary (process-children))
    (literal "\n")

    ;; Then full details (default mode)
    (literal "FULL DETAILS:\n")
    (process-children)))

(element field
  (let ((name (attribute-string current-node "name"))
        (type (attribute-string current-node "type")))
    (literal (string-append "  Field: " name "\n"))
    (literal (string-append "    Type: " type "\n"))
    (literal (string-append "    Description: A " type " field named " name "\n\n"))))

(default (process-children))

;; ============================================================================
;; Summary Mode - Just Names
;; ============================================================================

(mode summary
  (element field
    (let ((name (attribute-string current-node "name")))
      (literal (string-append "  - " name "\n"))))

  (default (process-children)))

;; ============================================================================
;; Start Processing and Write Output
;; ============================================================================

(make entity
  system-id: "generated/class-report-with-modes.txt"
  (process-root))
