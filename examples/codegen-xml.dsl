<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY string-utils SYSTEM "lib/string-utils-cdata.scm">
  <!ENTITY java-helpers SYSTEM "lib/java-helpers.scm">
]>
<style-sheet>
  <style-specification>
;; XML-embedded DSSSL template with entity includes
;; This demonstrates the DSSSL-style approach with modern XML

&string-utils;
&java-helpers;

;; Main code generation logic
(define class-node current-root)
(define class-name (attribute-string class-node "name"))
(define package-name (attribute-string class-node "package"))

;; Get all field elements
(define all-children (children class-node))
(define fields (select-elements all-children "field"))

;; Convert node-list to list for processing
(define field-list (node-list->list fields))

;; Generate field declarations
(define field-decls
  (apply string-append
    (map (lambda (field)
           (make-field
             (attribute-string field "name")
             (attribute-string field "type")))
         field-list)))

;; Generate getters
(define getters
  (apply string-append
    (map (lambda (field)
           (make-getter
             (attribute-string field "name")
             (attribute-string field "type")))
         field-list)))

;; Generate setters
(define setters
  (apply string-append
    (map (lambda (field)
           (make-setter
             (attribute-string field "name")
             (attribute-string field "type")))
         field-list)))

;; Build complete Java class
(define java-code
  (string-append
    "package " package-name ";\n\n"
    "public class " class-name " {\n\n"
    field-decls "\n"
    getters "\n"
    setters
    "}\n"))

;; Write to file
(define output-file (string-append "generated/" class-name ".java"))
(write-sosofo (make-entity output-file (literal java-code)))
  </style-specification>
</style-sheet>
