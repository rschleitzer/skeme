;; Java Code Generator Template
;; Generates a Java class from XML specification

(display "=== Java Code Generator ===")
(newline)

;; Helper: Filter node-list to get only elements
(define (elements-only node-list)
  (define (filter-elements nl result)
    (if (node-list-empty? nl)
        result
        (let ((first (node-list-first nl))
              (rest (node-list-rest nl)))
          (if (element? first)
              (filter-elements rest (append result (list first)))
              (filter-elements rest result)))))
  (filter-elements node-list '()))

;; Get the root (class element)
(define class-node current-root)
(define class-name (attribute-string class-node "name"))
(define package-name (attribute-string class-node "package"))

(display "Generating class: ")
(display class-name)
(newline)
(display "Package: ")
(display package-name)
(newline)
(newline)

;; Start building the Java code
(define package-decl
  (string-append "package " package-name ";\n\n"))

(define class-header
  (string-append "public class " class-name " {\n"))

;; Get all child elements
(define all-children (children class-node))
(define child-elements (elements-only all-children))

(display "Found ")
(display (length child-elements))
(display " child elements")
(newline)

;; Process fields
(define (generate-field field-node)
  (let ((field-name (attribute-string field-node "name"))
        (field-type (attribute-string field-node "type")))
    (string-append "    private " field-type " " field-name ";\n")))

;; Process methods
(define (generate-method method-node)
  (let ((method-name (attribute-string method-node "name"))
        (return-type (attribute-string method-node "returnType")))
    (string-append
      "    public " return-type " " method-name "() {\n"
      "        // TODO: Implement\n"
      "        return null;\n"
      "    }\n")))

;; Iterate through elements and generate code
(display "\nGenerating fields and methods...")
(newline)

(define fields-code
  (apply string-append
    (map generate-field
      (filter (lambda (node)
                (string=? (gi node) "field"))
              child-elements))))

(define methods-code
  (apply string-append
    (map generate-method
      (filter (lambda (node)
                (string=? (gi node) "method"))
              child-elements))))

(define class-footer "}\n")

;; Combine all parts
(define complete-class
  (string-append
    package-decl
    class-header
    "\n"
    fields-code
    "\n"
    methods-code
    class-footer))

;; Display the generated code
(display "\n=== Generated Java Code ===\n")
(display complete-class)
(display "=== End of Generated Code ===\n")

(newline)
(display "Code generation complete!")
(newline)

;; Return success
#t
