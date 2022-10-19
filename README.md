## Rackup
A backup utility made with Racket.

### Usage
Backup configurations are written as Racket programs that use `rackup` as a library dependency.

Run your backup script with `--help` to see the list of available options. You can test your backup scripts with `--simulate` or `--print-files`.

#### Example
```racket
#lang racket/base
(require rackup)

(backup "/tmp/example.bak"
  (files
      "/etc/hosts" ; shorthand
      (file "/etc/hostname")
      (file "/etc/vimrc" #:encrypt? #t) ; encrypted file 
      (file "/etc/X11/xinit" #:encrypt? #t) ; encrypted directory 
      (in-dir "~/" #:excluded (list "Downloads" "Desktop"))
      ; stores stdout of given shell command in a file named "msg.txt"
      (cmd "msg.txt" "echo \"Hi, this is $(id -un)@$(hostname)\"")
      ; can encrypt stdout
      (cmd "secret.txt" "uname -a" #:encrypt? #t)))
```

The resulting backup will have the following structure inside:
```
/etc/
    hosts
    hostname
~/* (except Downloads and Desktop)
stdout/var/tmp/msg.txt_<RANDOM>
encrypted/var/tmp/var/tmp/all-encrypted_<RANDOM>.enc
```

All files that are to be encrypted are packed in a single tarball and encrypted with a single password, regardless of where they are specified in the backup script.

It's also possible to append the date to the resulting backup filename.

```racket
(backup "/tmp/example.bak"
    #:date-format "%d-%m-%Y"
    ...)
```

Or use the default format, which is `%Y-%m-%d_%Hh%Mm%Ss`:
```racket
(backup "/tmp/example.bak"
    #:append-date #t
    ...)
```
