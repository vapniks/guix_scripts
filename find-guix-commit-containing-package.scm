#!/home/ben/.guix-profile/bin/guile -s
!#

(use-modules (guix profiles) (guix store) (srfi srfi-1) (guix build utils))

(define (find-guix-commit package-name package-version)
  ;; Check manifests in all generations of the current profile
  (let ((pkgentry #f)
	(guixentry #f)
	(pull-profile "/var/guix/profiles/per-user/root/current-guix")
	(pull-generation #f)
	(chash #f))
    ;; loop over all generations of the user profile
    (do ((ns (profile-generations %current-profile) (cdr ns)))
	((or pkgentry (null-list? ns)))
      ;; get the manifest entry for the package@version we're looking for if it exists in this profile generation
      (set! pkgentry (manifest-lookup (profile-manifest (generation-file-name %current-profile (car ns)))
				      (manifest-pattern (name package-name)
							(version package-version)))))
    (when pkgentry
      ;; get the commit hash of the version of guix that produced the package
      (set! chash (substring
		   (cadr (assq 'commit
			       (cdadr (assq 'provenance
					    (manifest-entry-properties pkgentry)))))
		   0 7))
      ;; loop over the pull profile generations
      (do ((ns (profile-generations pull-profile) (cdr ns)))
	  (guixentry)
	(set! pull-generation (car ns))
	;; get the manifest entry for guix@chash if it exists in this profile generation
	(set! guixentry (manifest-lookup (profile-manifest (generation-file-name pull-profile pull-generation))
					 (manifest-pattern (name "guix")
							   (version chash))))))
    ;; return the pull generation and commit hash
    (values pull-generation chash)))

;; Run this when called from the command line:
(let ((cmdline (command-line)))
  (when (> (length cmdline) 1)
    (if (< (length cmdline) 3)
	(format #t "Usage: ~a <PKG> <VERSION>~%" (basename (current-filename)))
	(let ((pkg (cadr cmdline))
	      (version (caddr cmdline)))
	  (call-with-values (lambda () (find-guix-commit pkg version))
	    (lambda (pullgen chash)
	      (if pullgen
		  (format #t "Package ~a@~a found in guix pull generation: ~a, with commit hash: ~a~%"
			  pkg version pullgen chash)
		  (format #t "Can't find guix pull profile for version ~a of ~a~%" version pkg))))))))
