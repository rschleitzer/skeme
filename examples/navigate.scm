;; Skeme XML Navigation Template
;; Demonstrates grove primitives for navigating XML

(display "=== XML Navigation Demo ===")
(newline)
(newline)

;; The parsed XML root is available as 'current-root'
(display "Root element: ")
(display (gi current-root))
(newline)

;; Get the children
(display "Getting children...")
(newline)
(define root-children (children current-root))

;; Check if we have children
(if (node-list-empty? root-children)
    (begin
      (display "  No children found")
      (newline))
    (begin
      (display "  Found ")
      (display (node-list-length root-children))
      (display " child nodes")
      (newline)))

;; Get the first child
(display "\nFirst child: ")
(define first-child (node-list-first root-children))
(if first-child
    (begin
      (if (element? first-child)
          (begin
            (display (gi first-child))
            (display " (element)"))
          (display "(text node)"))
      (newline))
    (begin
      (display "none")
      (newline)))

;; Try to get some attributes
(display "\nChecking for attributes...")
(newline)
(define id-value (id current-root))
(if id-value
    (begin
      (display "  ID attribute: ")
      (display id-value)
      (newline))
    (begin
      (display "  No ID attribute")
      (newline)))

;; Get text content
(display "\nText content of root:")
(newline)
(define content (data current-root))
(display content)
(newline)

;; Show parent navigation
(display "\nParent of root: ")
(define root-parent (parent current-root))
(if root-parent
    (display (gi root-parent))
    (display "(none - this is the root)"))
(newline)

(display "\n=== Navigation Complete ===")
(newline)

;; Return success
#t
