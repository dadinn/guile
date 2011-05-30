;;; List --- List scripts that can be invoked by guile-tools  -*- coding: iso-8859-1 -*-

;;;; 	Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
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
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;;; Boston, MA 02110-1301 USA

;;; Commentary:

;; Usage: list
;;
;; List scripts that can be invoked by guile-tools.

;;; Code:

(define-module (scripts list)
  #:use-module ((srfi srfi-1) #:select (fold append-map))
  #:export (list-scripts))


(define (directory-files dir)
  (if (and (file-exists? dir) (file-is-directory? dir))
      (let ((dir-stream (opendir dir)))
        (let loop ((new (readdir dir-stream))
                   (acc '()))
          (if (eof-object? new)
              (begin
                (closedir dir-stream)
                acc)
              (loop (readdir dir-stream)
                    (if (or (string=? "."  new)             ; ignore
                            (string=? ".." new))            ; ignore
                        acc
                        (cons new acc))))))
      '()))

(define (strip-extensions path)
  (or-map (lambda (ext)
            (and
             (string-suffix? ext path)
             (substring path 0
                        (- (string-length path) (string-length ext)))))
          (append %load-compiled-extensions %load-extensions)))

(define (unique l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        ((equal? (car l) (cadr l)) (unique (cdr l)))
        (else (cons (car l) (unique (cdr l))))))

(define (find-submodules head)
  (let ((shead (map symbol->string head)))
    (unique
     (sort
      (append-map (lambda (path)
                    (fold (lambda (x rest)
                            (let ((stripped (strip-extensions x)))
                              (if stripped (cons stripped rest) rest)))
                          '()
                          (directory-files
                           (fold (lambda (x y) (in-vicinity y x)) path shead))))
                  %load-path)
      string<?))))

(define (list-scripts . args)
  (for-each (lambda (x)
              ;; would be nice to show a summary.
              (format #t "~A\n" x))
            (find-submodules '(scripts))))

(define main list-scripts)
