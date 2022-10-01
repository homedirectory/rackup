#!/usr/bin/env racket
#lang racket/base

(require "../src/cli.rkt")

(backup 
  "/tmp/example.bak"
  ; --- files ---
  "/etc/hosts" ; string
  (file "/etc/hosts") ; file 
  (file "/etc/hosts" #:encrypt? #t) ; encrypted file 
  (file "/etc/X11/xinit" #:encrypt? #t) ; encrypted directory 
  (cmd "msg.txt" 
       "echo \"Hi, this is $(id -un)@$(hostname)\"")
  )
