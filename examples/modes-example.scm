;; TRUE DSSSL with MODES Example
;; Demonstrates processing same element differently in different contexts

;; ============================================================================
;; Default Mode - Full Content Generation
;; ============================================================================

(element class
  (let ((name (attribute-string current-node "name"))
        (package (attribute-string current-node "package")))
    (make entity
      system-id: (string-append "generated/" name ".java")
      (make sequence
        ;; First generate TOC
        (literal "// TABLE OF CONTENTS:\n")
        (with-mode toc (process-children))
        (literal "\n")

        ;; Then full content
        (literal (string-append "package " package ";\n\n"))
        (literal (string-append "public class " name " {\n\n"))
        (process-children)
        (literal "}\n")))))

(element field
  (let ((name (attribute-string current-node "name"))
        (type (attribute-string current-node "type")))
    (make sequence
      (literal (string-append "    private " type " " name ";\n\n"))
      (literal (string-append "    public " type " get"))
      (literal (capitalize name))
      (literal (string-append "() { return " name "; }\n\n"))
      (literal (string-append "    public void set"))
      (literal (capitalize name))
      (literal (string-append "(" type " " name ") { this." name " = " name "; }\n\n")))))

(default (process-children))

;; ============================================================================
;; TOC Mode - Table of Contents (just field names)
;; ============================================================================

(mode toc
  (element field
    (let ((name (attribute-string current-node "name")))
      (literal (string-append "//   - " name "\n"))))

  (default (process-children)))

;; ============================================================================
;; Helper Function
;; ============================================================================

(define (capitalize str)
  "Capitalize first letter of string"
  (if (string=? str "")
      ""
      (string-append
        (string (char-upcase (string-ref str 0)))
        (substring str 1 (string-length str)))))

;; ============================================================================
;; Start Processing
;; ============================================================================

(process-root)
