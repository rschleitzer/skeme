;; String utility functions

(define (capitalize str)
  "Capitalize the first character of a string"
  (if (string=? str "")
      ""
      ;; Simple ASCII uppercase for first char
      (let ((first-char (string-ref str 0)))
        (string-append
          (if (and (char>=? first-char #\a) (char<=? first-char #\z))
              (string (integer->char (- (char->integer first-char) 32)))
              (string first-char))
          (substring str 1 (string-length str))))))

(define (join-strings sep lst)
  "Join a list of strings with a separator"
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (join-strings sep (cdr lst))))))
