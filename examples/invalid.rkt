#!/usr/bin/env racket
#lang racket/base

(require "../src/cli.rkt")

(backup 
  "/tmp/example.bak"
  ; --- files ---
  "/a/b" ; non-existent file
  (file "/b/c") ; non-existent file 
  (file "/b/c" #:encrypt? #t) ; non-existent file
  (cmd "" "echo 123") ; empty file-name
  (cmd "out" "") ; empty cmd
  )
