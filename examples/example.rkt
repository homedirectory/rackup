#!/usr/bin/env racket
#lang racket/base

(require "../src/cli.rkt")

(backup 
  "/tmp/example.bak"
  ; --- files ---
  "/etc/hosts" ; string
  (file "/etc/hostname") ; file 
  (file "/etc/vimrc" #:encrypt? #t) ; encrypted file 
  (file "/etc/X11/xinit" #:encrypt? #t) ; encrypted directory 
  (cmd "msg.txt" 
       "echo \"Hi, this is $(id -un)@$(hostname)\"")
  (cmd "secret.txt" "uname -a" #:encrypt? #t)
  )
