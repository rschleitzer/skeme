;; Java Code Generation Helpers
;; Reusable functions for generating Java code

;; Convert type name to getter prefix
(define (getter-prefix type-name)
  (if (string=? type-name "boolean")
      "is"
      "get"))

;; Capitalize first letter
(define (capitalize str)
  (if (string=? str "")
      ""
      (string-append
        (string-upcase (substring str 0 1))
        (substring str 1 (string-length str)))))

;; Generate getter method name
(define (getter-name field-name type-name)
  (string-append
    (getter-prefix type-name)
    (capitalize field-name)))

;; Generate setter method name
(define (setter-name field-name)
  (string-append "set" (capitalize field-name)))

;; Generate a getter method
(define (generate-getter field-name field-type)
  (string-append
    "    public " field-type " " (getter-name field-name field-type) "() {\n"
    "        return " field-name ";\n"
    "    }\n"))

;; Generate a setter method
(define (generate-setter field-name field-type)
  (string-append
    "    public void " (setter-name field-name) "(" field-type " " field-name ") {\n"
    "        this." field-name " = " field-name ";\n"
    "    }\n"))

(display "Java helpers loaded\n")
