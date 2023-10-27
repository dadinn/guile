(define-module (ice-9 expect-hygienic)
  #:export
  (expect
   *expect-port*
   *expect-char-proc*
   *expect-eof-proc*
   *expect-timeout*
   *expect-timeout-proc*))

(use-modules
 (system syntax)
 ((srfi srfi-1) #:prefix srfi1:))

(define *expect-port*
  (make-parameter #f))
(define *expect-char-proc*
  (make-parameter #f))
(define *expect-eof-proc*
  (make-parameter #f))
(define *expect-timeout*
  (make-parameter #f))
(define *expect-timeout-proc*
  (make-parameter #f))

(define (expect-select port timeout)
  (let* ((secs-usecs (gettimeofday))
         (relative
          (- timeout
             (car secs-usecs)
             (/ (cdr secs-usecs)
                ;; one million.
                1000000))))
    (and (> relative 0)
         (pair?
          (car
           (select
            (list port)
            '() '()
            relative))))))

(define-syntax expect-with-bindings
  (lambda (stx)
    (syntax-case stx (=>)
      ((expect-with-bindings
        (procedure-bindings ...)
        (clauses-with-bindings ...)
        ((matcher => consumer) more-clauses ...))
       #'(expect-with-bindings
          (procedure-bindings
           ...
           (matcher-binding matcher)
           (consumer-binding consumer)
           (consumer-binding (lambda (result) (apply consumer-binding result))))
          (clauses-with-bindings
           ...
           (matcher-binding => consumer-binding))
          (more-clauses ...)))
      ((expect-with-bindings
        (procedure-bindings ...)
        (clauses-with-bindings ...)
        ((matcher body ...) more-clauses ...))
       #'(expect-with-bindings
          (procedure-bindings
           ...
           (matcher-binding matcher))
          (clauses-with-bindings
           ...
           (matcher-binding body ...))
          (more-clauses ...)))
      ((expect-with-bindings
        (procedure-bindings ...)
        ((matcher-binding body ...) ...)
        ;; no more clauses
        ())
       #'(let* ((input-port (*expect-port*))
                (char-proc (*expect-char-proc*))
                (eof-proc (*expect-eof-proc*))
                (timeout-proc (*expect-timeout-proc*))
                (timeout (*expect-timeout*))
                (timeout
                 (and timeout
                  (let ((secs-usecs (gettimeofday)))
                    (+ (car secs-usecs)
                       timeout
                       (/ (cdr secs-usecs)
                          ;; one million.
                          1000000)))))
                procedure-bindings ...)
           (let loop ((content ""))
             (if (and timeout (not (expect-select input-port timeout)))
                 (and timeout-proc (timeout-proc content))
                 (let* ((char (read-char input-port))
                        (eof? (eof-object? char))
                        (next-content
                         (if (not eof?)
                             (string-append
                              content (string char))
                             content)))
                   (when char-proc
                     (char-proc char))
                   (cond
                    ((matcher-binding next-content eof?) body ...)
                    ...
                    (eof? (and eof-proc (eof-proc content)))
                    (else (loop next-content)))))))))))

(define (syntax-capture stx sym)
  "Capture identifier from syntax context, or return false if not found."
  (srfi1:any
   (lambda (id)
     (and (eqv? sym (syntax->datum id))
          (datum->syntax stx sym)))
   (syntax-locally-bound-identifiers stx)))

(define-syntax expect
  (lambda (stx)
    (syntax-case stx ()
      ((expect clause clauses ...)
       (with-syntax
           ((expect-port-stx
             (or (syntax-capture #'expect 'expect-port)
                 #'(or (*expect-port*) (current-input-port))))
            (expect-char-proc-stx
             (or (syntax-capture #'expect 'expect-char-proc)
                 #'(*expect-char-proc*)))
            (expect-eof-proc-stx
             (or (syntax-capture #'expect 'expect-eof-proc)
                 #'(*expect-eof-proc*)))
            (expect-timeout-stx
             (or (syntax-capture #'expect 'expect-timeout)
                 #'(*expect-timeout*)))
            (expect-timeout-proc-stx
             (or (syntax-capture #'expect 'expect-timeout-proc)
                 #'(*expect-timeout-proc*))))
         #'(parameterize
               ((*expect-port* expect-port-stx)
                (*expect-char-proc* expect-char-proc-stx)
                (*expect-eof-proc* expect-eof-proc-stx)
                (*expect-timeout* expect-timeout-stx)
                (*expect-timeout-proc* expect-timeout-proc-stx))
             (expect-with-bindings
              () () (clause clauses ...))))))))
