;; Improved Java Code Generator
;; Uses select-elements for cleaner code

(display "=== Improved Java Code Generator ===\n")

;; Get the root class element
(define class-node current-root)
(define class-name (attribute-string class-node "name"))
(define package-name (attribute-string class-node "package"))

(display "Generating class: ")
(display class-name)
(display "\n")

;; Use select-elements to get specific child elements
(define all-children (children class-node))
(define fields (select-elements all-children "field"))
(define methods (select-elements all-children "method"))

(display "Found ")
(display (node-list-length fields))
(display " fields and ")
(display (node-list-length methods))
(display " methods\n")

;; Generate a field declaration
(define (generate-field field-node)
  (let ((field-name (attribute-string field-node "name"))
        (field-type (attribute-string field-node "type")))
    (string-append "    private " field-type " " field-name ";\n")))

;; Generate a method
(define (generate-method method-node)
  (let ((method-name (attribute-string method-node "name"))
        (return-type (attribute-string method-node "returnType")))
    (string-append
      "    public " return-type " " method-name "() {\n"
      "        // TODO: Implement\n"
      "        return null;\n"
      "    }\n")))

;; Convert node-list to list for mapping
(define (node-list->list nl)
  (if (node-list-empty? nl)
      '()
      (cons (node-list-first nl) (node-list->list (node-list-rest nl)))))

;; Generate all fields
(define fields-code
  (apply string-append
    (map generate-field (node-list->list fields))))

;; Generate all methods
(define methods-code
  (apply string-append
    (map generate-method (node-list->list methods))))

;; Build the complete class
(define complete-class
  (string-append
    "package " package-name ";\n\n"
    "public class " class-name " {\n\n"
    fields-code
    "\n"
    methods-code
    "}\n"))

;; Write to file
(define output-file (string-append "generated/" class-name ".java"))
(define output-sosofo (make-entity output-file (literal complete-class)))
(write-sosofo output-sosofo)

(display "Successfully generated ")
(display output-file)
(display "\n")

#t
