#!guile \
-e main -s
!#
;; For usage instructions do: manifest-to-manifest.scm -h

;; Original obtained from here: https://lists.gnu.org/archive/html/help-guix/2019-07/msg00131.html
;; Some changes made by me.

(use-modules (guix profiles)
	     (gnu packages)
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
  (substring
   (cadr (assq 'commit
	       (cdadr (assq 'provenance
			    (manifest-entry-properties (car (manifest-entries 
							     (profile-manifest %current-profile))))))))
   0 7))
  
(define (main args) 
  (let* ((optspec `((help (single-char #\h)
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
	 (opts (getopt-long args optspec))
	 (profile (option-ref opts 'profile %current-profile))
	 (incv (option-ref opts 'include-versions #f))
	 (help (option-ref opts 'help #f))
	 (outfile (option-ref opts 'output #f)))
    (if help
	(format #t "Usage: ~a [OPTION]...
Create manifest object code for packages in a guix profile.
The output can be used with the -m option of \"guix profile\"

Options:
 -h, --help              Display this help
 -p, --profile=SYMLINK   Specify guix profile (default=~a)
 -v, --include-versions  Include package versions in output
 -o, --output=FILE       Write output to FILE instead of stdout
"
		(basename (current-filename))
		%current-profile)
	(let ((out (if outfile
		       (open-output-file outfile)
		       (current-output-port))))
	  (format out ";; Using guix commit: ~a\n" (guix-commit))
	  (format out ";; Manifest generated from ~a\n" profile)
	  (pretty-print `(specifications->manifest ',(guix-manifest profile incv)) out)
	  ))))

