(begin
  (use-modules (guix ci)
               (srfi srfi-1)
               (ice-9 match))

  (define (find-latest)
    (define latest
      (find (lambda (evaluation)
              (and (string=? (evaluation-spec evaluation)
                             "guix-modular-master")
                   (evaluation-complete? evaluation)))
            (latest-evaluations "https://ci.guix.gnu.org" 10)))
    (and latest
         (match (evaluation-checkouts latest)
           ((checkout . rest)
            (checkout-commit checkout))
           (anything (error anything)))))

  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (commit (or (find-latest)
                     (error "Could not find recent build of Guix!"))))))
