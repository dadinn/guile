;;;; 	Copyright (C) 1996, 1998, 1999, 2001, 2006 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;

;;; Commentary:

;; This module is documented in the Guile Reference Manual.
;; These macros are exported:
;;    expect, expect-chars, expect-strings, interact.

;;; Code:

(define-module (ice-9 expect)
  #:export
  (expect
   expect-strings
   expect-chars
   interact
   *expect-port*
   *expect-char-proc*
   *expect-eof-proc*
   *expect-timeout*
   *expect-timeout-proc*
   *expect-strings-compile-flags*
   *expect-strings-exec-flags*))

(use-modules
 (system syntax)
 ((srfi srfi-1) #:prefix srfi1:)
 ((ice-9 regex) #:select
  (match:substring
   match:count))
 ((ice-9 threads) #:select
  (cancel-thread thread-exited?
   call-with-new-thread))
 ((ice-9 rdelim) #:select
  (read-line)))

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
(define *expect-strings-compile-flags*
  (make-parameter regexp/newline))
(define *expect-strings-exec-flags*
  (make-parameter regexp/noteol))

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
                        (char (and (not (eof-object? char)) char))
                        (next-content
                         (if char
                             (string-append
                              content (string char))
                             content)))
                   (cond
                    ((matcher-binding next-content char)
                     body ...)
                    ...
                    ((not char)
                     (and eof-proc (eof-proc content)))
                    (else
                     (loop next-content)))))))))))

(define (syntax-capture stx sym)
  "Capture identifier from syntax context, or return false if not found."
  (srfi1:any
   (lambda (id)
     (and (eqv? sym (syntax->datum id))
          (datum->syntax stx sym)))
   (syntax-locally-bound-identifiers stx)))


;;; expect: each test is a procedure which is applied to the accumulating
;;; string.
(define-syntax expect
  (lambda (stx)
    (syntax-case stx ()
      ((expect (matcher-with-eof-flag body ...) ...)
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
         #'(let* ((char-proc expect-char-proc-stx))
             (parameterize
                 ((*expect-port* expect-port-stx)
                  (*expect-eof-proc* expect-eof-proc-stx)
                  (*expect-timeout* expect-timeout-stx)
                  (*expect-timeout-proc* expect-timeout-proc-stx))
               (expect-with-bindings
                () ()
                (((let ((matcher-inner-binding matcher-with-eof-flag))
                    (lambda (content char)
                      (when (and char-proc char)
                        (char-proc char))
                      (matcher-inner-binding content (not char))))
                  body ...) ...)))))))))

;;; match a string against a regexp, returning a list of strings (required
;;; by the => syntax) or #f.  called once each time a character is added
;;; to s (eof? will be #f), and once when eof is reached (with eof? #t).
(define (expect-regexec rx s eof? exec-flags)
  ;; if expect-strings-exec-flags contains regexp/noteol,
  ;; remove it for the eof test.
  (let* ((flags (if (and eof? (logand exec-flags regexp/noteol))
                    (logxor exec-flags regexp/noteol)
                    exec-flags))
         (matches (regexp-exec rx s 0 flags)))
    (if matches
        (do ((i (- (match:count matches) 1) (- i 1))
             (result '() (cons (match:substring matches i) result)))
            ((< i 0) result))
        #f)))

;;; the regexec front-end to expect:
;;; each test must evaluate to a regular expression.
(define-syntax expect-strings
  (lambda (stx)
    (syntax-case stx ()
      ((expect-strings (pattern body more-body ...) ...)
       (with-syntax
           ((expect-port-stx
             (or (syntax-capture #'expect-strings 'expect-port)
                 #'(or (*expect-port*) (current-input-port))))
            (expect-char-proc-stx
             (or (syntax-capture #'expect-strings 'expect-char-proc)
                 #'(*expect-char-proc*)))
            (expect-eof-proc-stx
             (or (syntax-capture #'expect-strings 'expect-eof-proc)
                 #'(*expect-eof-proc*)))
            (expect-timeout-stx
             (or (syntax-capture #'expect-strings 'expect-timeout)
                 #'(*expect-timeout*)))
            (expect-timeout-proc-stx
             (or (syntax-capture #'expect-strings 'expect-timeout-proc)
                 #'(*expect-timeout-proc*)))
            (expect-strings-compile-flags-stx
             (or (syntax-capture #'expect-strings 'expect-strings-compile-flags)
                 #'(*expect-strings-compile-flags*)))
            (expect-strings-exec-flags-stx
             (or (syntax-capture #'expect-strings 'expect-strings-exec-flags)
                 #'(*expect-strings-exec-flags*))))
         #'(let* ((compile-flags expect-strings-compile-flags-stx)
                  (exec-flags expect-strings-exec-flags-stx)
                  (char-proc expect-char-proc-stx))
             (parameterize
                 ((*expect-port* expect-port-stx)
                  (*expect-eof-proc* expect-eof-proc-stx)
                  (*expect-timeout* expect-timeout-stx)
                  (*expect-timeout-proc* expect-timeout-proc-stx))
               (expect-with-bindings
                () ()
                (((let ((rx (make-regexp pattern compile-flags)))
                    (lambda (content char)
                      (when (and char-proc char)
                        (char-proc char))
                      (expect-regexec rx content (not char) exec-flags)))
                  body more-body ...) ...)))))))))

(define-syntax expect-chars
  (lambda (stx)
    (syntax-case stx ()
      ((expect-chars clause clauses ...)
       (with-syntax
           ((expect-port-stx
             (or (syntax-capture #'expect-chars 'expect-port)
                 #'(or (*expect-port*) (current-input-port))))
            (expect-eof-proc-stx
             (or (syntax-capture #'expect-chars 'expect-eof-proc)
                 #'(*expect-eof-proc*)))
            (expect-timeout-stx
             (or (syntax-capture #'expect-chars 'expect-timeout)
                 #'(*expect-timeout*)))
            (expect-timeout-proc-stx
             (or (syntax-capture #'expect-chars 'expect-timeout-proc)
                 #'(*expect-timeout-proc*))))
         #'(parameterize
               ((*expect-port* expect-port-stx)
                (*expect-eof-proc* expect-eof-proc-stx)
                (*expect-timeout* expect-timeout-stx)
                (*expect-timeout-proc* expect-timeout-proc-stx))
             (expect-with-bindings
              () () (clause clauses ...))))))))

(define-syntax interact
  (lambda (stx)
    (syntax-case stx ()
      ((interact output-port)
       #'(interact (current-input-port) output-port))
      ((interact input-port output-port)
       (with-syntax
           ((expect-port-stx
             (or (syntax-capture #'interact 'expect-port)
                 #'(or (*expect-port*) (current-input-port))))
            (expect-char-proc-stx
             (or (syntax-capture #'interact 'expect-char-proc)
                 #'(or (*expect-char-proc*) display)))
            (expect-eof-proc-stx
             (or (syntax-capture #'interact 'expect-eof-proc)
                 #'(*expect-eof-proc*))))
         #'(let* ((char-proc expect-char-proc-stx)
                  (interact-input-port input-port)
                  (interact-output-port output-port)
                  (interact-thread
                   (call-with-new-thread
                    (lambda ()
                      (let loop ((line (read-line interact-input-port)))
                        (unless
                            (or (eof-object? line)
                                (string-ci=? line ",return"))
                          (display line interact-output-port)
                          (newline interact-output-port)
                          (force-output interact-output-port)
                          (loop (read-line interact-input-port))))
                      (newline interact-output-port)
                      (force-output interact-output-port)))))
             (dynamic-wind
               (const #t)
               (lambda ()
                 (parameterize
                     ((*expect-port* expect-port-stx)
                      (*expect-eof-proc* expect-eof-proc-stx)
                      (*expect-timeout* #f)
                      (*expect-timeout-proc* #f))
                   (when (eq? (*expect-port*) interact-input-port)
                     (error "expect and interact input ports are the same"))
                   (expect-with-bindings
                    () ()
                    (((lambda (content char)
                        (when (and char-proc char)
                          (char-proc char))
                        (thread-exited? interact-thread)))))))
               (lambda ()
                 (cancel-thread interact-thread)))))))))

+;;; expect.scm ends here
