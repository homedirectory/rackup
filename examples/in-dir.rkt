#!/usr/bin/env racket
#lang racket/base

(require "../src/cli.rkt")

(backup 
  "/tmp/in-dir.bak"
  ; --- files ---
  (in-dir "/etc"
          "hosts"
          (file "vimrc" #:encrypt? #t)
          (in-dir "X11" #:encrypt? #t
                  #:excluded (list "xinit" "xorg.conf.d/10-monitor.conf")))
  )
