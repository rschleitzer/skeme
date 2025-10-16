<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY string-utils SYSTEM "lib/string-utils-cdata.scm">
  <!ENTITY java-helpers SYSTEM "lib/java-helpers.scm">
]>
<style-sheet>
  <style-specification>
;; Enhanced DSSSL template demonstrating new node-list primitives
;; Shows: select-children, node-list-filter, node-list-map, etc.

&string-utils;
&java-helpers;

;; Use current-root (automatically available)
(define class-node current-root)
(define class-name (attribute-string class-node "name"))
(define package-name (attribute-string class-node "package"))

;; Use select-children instead of select-elements on children
(define all-fields (select-children class-node "field"))

;; Filter fields by type using node-list-filter
(define required-fields
  (node-list-filter
    (lambda (field)
      (equal? (attribute-string field "required") "true"))
    all-fields))

(define optional-fields
  (node-list-filter
    (lambda (field)
      (not (equal? (attribute-string field "required") "true")))
    all-fields))

;; Check if class has ID field using node-list-some
(define has-id-field?
  (node-list-some
    (lambda (field)
      (equal? (attribute-string field "name") "id"))
    all-fields))

;; Extract field names using node-list-map
(define field-names
  (node-list-map
    (lambda (field) (attribute-string field "name"))
    all-fields))

;; Count fields using node-list-count
(define field-count (node-list-count all-fields))
(define required-count (node-list-count required-fields))
(define optional-count (node-list-count optional-fields))

;; Convert node-lists to regular lists for processing
(define all-fields-list (node-list->list all-fields))
(define required-list (node-list->list required-fields))
(define optional-list (node-list->list optional-fields))

;; Generate field declarations
(define field-decls
  (apply string-append
    (map (lambda (field)
           (make-field
             (attribute-string field "name")
             (attribute-string field "type")))
         all-fields-list)))

;; Generate constructor with required fields only
(define constructor
  (string-append
    "    public " class-name "(")
    (if (node-list-empty? required-fields)
        ""
        (apply string-append
          (map (lambda (field)
                 (string-append
                   (attribute-string field "type") " "
                   (attribute-string field "name")))
               required-list)))
    ") {\n"
    (apply string-append
      (map (lambda (field)
             (let ((name (attribute-string field "name")))
               (string-append "        this." name " = " name ";\n")))
           required-list))
    "    }\n"))

;; Generate getters
(define getters
  (apply string-append
    (map (lambda (field)
           (make-getter
             (attribute-string field "name")
             (attribute-string field "type")))
         all-fields-list)))

;; Generate setters
(define setters
  (apply string-append
    (map (lambda (field)
           (make-setter
             (attribute-string field "name")
             (attribute-string field "type")))
         all-fields-list)))

;; Generate class comment
(define class-comment
  (string-append
    "/**\n"
    " * " class-name " - Generated class\n"
    " * Total fields: " (number->string field-count) "\n"
    " * Required fields: " (number->string required-count) "\n"
    " * Optional fields: " (number->string optional-count) "\n"
    (if has-id-field?
        " * Has ID field: yes\n"
        " * Has ID field: no\n")
    " */\n"))

;; Build complete Java class
(define java-code
  (string-append
    "package " package-name ";\n\n"
    class-comment
    "public class " class-name " {\n\n"
    field-decls "\n"
    constructor "\n"
    getters "\n"
    setters
    "}\n"))

;; Write to file
(define output-file (string-append "generated/" class-name ".java"))
(write-sosofo (make-entity output-file (literal java-code)))
  </style-specification>
</style-sheet>
