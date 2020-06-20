#!guile -s
!#
;; For usage instructions do: manifest-to-manifest.scm -h

;; Original obtained from here: https://lists.gnu.org/archive/html/help-guix/2019-07/msg00131.html
;; Some changes made by me.

(use-modules (guix profiles)
             (ice-9 match)
             (ice-9 pretty-print)
	     (ice-9 getopt-long))

(define (guix-manifest where ver)
  (sort (map (lambda (entry)
	       (let ((out (manifest-entry-output entry)))
		 (if (string= out "out")
		     (if ver
			 (format #f "~a@~a"
				 (manifest-entry-name entry)
				 (manifest-entry-version entry))
			 (manifest-entry-name entry))
		     (format #f "~a:~a"
			     (manifest-entry-name entry)
			     (manifest-entry-output entry)))))
	     (manifest-entries (profile-manifest where)))
        string<?))

(define (guix-commit)
  (let ((guix-manifest (profile-manifest (string-append (getenv "HOME") 
							"/.config/guix/current"))))
    (match (assq 'source (manifest-entry-properties (car (manifest-entries 
							  guix-manifest))))
      (('source ('repository ('version 0) _ _
                             ('commit commit) _ ...))
       commit)
      (_ #f))))

(let* ((cl (command-line))
       (optspec `((help (single-char #\h)
			(required? #f))
		  (profile (single-char #\p)
			   (required? #f)
			   (value #t)
			   (predicate ,(lambda (p)
					 (file-exists?
					  (string-append p "/manifest")))))
		  (include-versions (single-char #\v)
				    (required? #f)
				    (value #f))
		  (output (single-char #\o)
			  (required? #f)
			  (value #t))))
       (opts (getopt-long (command-line) optspec))
       (profile (option-ref opts 'profile (getenv "GUIX_PROFILE")))
       (incv (option-ref opts 'include-versions #f))
       (help (option-ref opts 'help #f))
       (outfile (option-ref opts 'output #f)))
  (if help
      (format #t "Usage: manifest-to-manifest.scm [OPTION]...
Create manifest object code for packages in a guix profile.
The output can be used with the -m option of \"guix profile\"

Options:
 -h, --help              Display this help
 -p, --profile=SYMLINK   Specify guix profile (default=~a)
 -v, --include-versions  Include package versions in output
 -o, --output=FILE       Write output to FILE instead of stdout
" (getenv "GUIX_PROFILE"))
      (let ((out (if outfile
		     (open-output-file outfile)
		     (current-output-port))))
	(format out ";; Using guix commit: ~a\n" (guix-commit))
	(format out ";; Manifest generated from ~a\n" profile)
	(pretty-print `(specifications->manifest ',(guix-manifest profile incv)) out))))

