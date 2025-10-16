<?xml version="1.0"?>
<!DOCTYPE style-sheet [
  <!ENTITY string-utils SYSTEM "lib/string-utils-cdata.scm">
]>
<style-sheet>
  <style-specification>
&string-utils;

<![CDATA[
;; CDATA in main template for code with < and >
(define (compare-length str1 str2)
  "Return true if str1 is shorter"
  (< (string-length str1) (string-length str2)))

;; Regular Scheme code (no special chars in this section)
(define class-node current-root)
(define class-name (attribute-string class-node "name"))

;; More code with < operator
(define fields-with-short-names
  (filter (lambda (f)
            (< (string-length (attribute-string f "name")) 10))
          (node-list->list (select-elements (children class-node) "field"))))
]]>

;; Use functions from everywhere
(define code (capitalize class-name))
(write-sosofo (make-entity "generated/Mixed.txt" (literal code)))
  </style-specification>
</style-sheet>
