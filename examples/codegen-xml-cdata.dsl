<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY string-utils SYSTEM "lib/string-utils-cdata.scm">
  <!ENTITY java-helpers SYSTEM "lib/java-helpers.scm">
]>
<style-sheet>
  <style-specification>
;; Test CDATA in entity files

&string-utils;
&java-helpers;

;; Main code generation logic
(define class-node current-root)
(define class-name (attribute-string class-node "name"))
(define package-name (attribute-string class-node "package"))

(define all-children (children class-node))
(define fields (select-elements all-children "field"))
(define field-list (node-list->list fields))

;; Generate Java POJO
(define java-code
  (string-append
    "package " package-name ";\n\n"
    "public class " class-name " {\n"
    "    // Generated with CDATA helper\n"
    "}\n"))

(write-sosofo (make-entity "generated/TestCDATA.java" (literal java-code)))
  </style-specification>
</style-sheet>
