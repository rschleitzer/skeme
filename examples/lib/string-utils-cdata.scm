<![CDATA[
;; String utility functions with CDATA protection

(define (char-less-than? ch limit)
  "Test if char is less than limit"
  (char<? ch limit))

(define (capitalize str)
  "Capitalize first letter"
  (if (string=? str "")
      ""
      (let ((first-char (string-ref str 0))
            (rest-str (substring str 1 (string-length str))))
        (if (and (char>=? first-char #\a) (char<=? first-char #\z))
            (string-append
              (string (integer->char (- (char->integer first-char) 32)))
              rest-str)
            (string-append (string first-char) rest-str)))))

(define (join-strings sep lst)
  "Join list of strings with separator"
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (join-strings sep (cdr lst))))))
]]>