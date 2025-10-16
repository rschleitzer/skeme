;; Java code generation helpers

(define (make-getter field-name field-type)
  "Generate a Java getter method"
  (string-append
    "    public " field-type " get" (capitalize field-name) "() {\n"
    "        return " field-name ";\n"
    "    }\n"))

(define (make-setter field-name field-type)
  "Generate a Java setter method"
  (string-append
    "    public void set" (capitalize field-name) "(" field-type " " field-name ") {\n"
    "        this." field-name " = " field-name ";\n"
    "    }\n"))

(define (make-field field-name field-type)
  "Generate a Java field declaration"
  (string-append "    private " field-type " " field-name ";\n"))
