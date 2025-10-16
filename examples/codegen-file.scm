;; Java Code Generator Template - With File Output
;; Generates a Java class from XML specification and writes to file

(display "=== Java Code Generator with File Output ===")
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

;; Start building the Java code
(define package-decl
  (string-append "package " package-name ";\n\n"))

(define class-header
  (string-append "public class " class-name " {\n"))

;; Get all child elements
(define all-children (children class-node))
(define child-elements (elements-only all-children))

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

;; Generate fields and methods
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

;; Create output filename
(define output-file (string-append "generated/" class-name ".java"))

(display "Writing to file: ")
(display output-file)
(newline)

;; Create entity flow object with filename and content
(define output-sosofo (make-entity output-file (literal complete-class)))

;; Write the file
(define write-result (write-sosofo output-sosofo))

(if write-result
    (begin
      (display "Successfully wrote file: ")
      (display output-file)
      (newline))
    (begin
      (display "Failed to write file!")
      (newline)))

(display "Code generation complete!")
(newline)

;; Return success
#t
