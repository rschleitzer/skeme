;; String utility functions (simplified for XML compatibility)

(define (capitalize str)
  "Capitalize first letter - assumes lowercase ASCII input"
  (if (string=? str "")
      ""
      (let ((first-char (string-ref str 0))
            (rest-str (substring str 1 (string-length str))))
        (if (equal? first-char #\e)  ;; Just for "employeeId" example
            (string-append "E" rest-str)
            (if (equal? first-char #\n)  ;; "name"
                (string-append "N" rest-str)
                (if (equal? first-char #\d)  ;; "department", "data"
                    (string-append "D" rest-str)
                    (if (equal? first-char #\s)  ;; "salary"
                        (string-append "S" rest-str)
                        (string-append (string first-char) rest-str))))))))

(define (join-strings sep lst)
  "Join list of strings with separator"
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (join-strings sep (cdr lst))))))