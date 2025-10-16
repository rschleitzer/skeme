;; Skeme Hello World Template
;; This demonstrates basic Skeme functionality

;; Display a message
(display "Skeme template executed!")
(newline)

;; Show that we can do arithmetic
(display "2 + 3 = ")
(display (+ 2 3))
(newline)

;; Show that our primitives work
(display "Creating a literal sosofo...")
(newline)
(define my-sosofo (literal "Hello from a sosofo!"))

;; Show that we can create flow objects
(define another-sosofo (make-formatting-instruction "Generated text content"))

;; Demonstrate sosofo-append
(define combined (sosofo-append my-sosofo another-sosofo))

(display "Template processing complete!")
(newline)

;; Return success
#t
