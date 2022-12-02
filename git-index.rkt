#lang racket/base
(require racket/list
         racket/system
         net/url
         net/uri-codec
         racket/match
         racket/function
         racket/path
         racket/pretty
         net/base64
         racket/port
         json
         racket/file)

(define DEBUG? #f)
(define (dprintf fmt . args)
  (when DEBUG?
    (apply printf fmt args)))

(define (basic-auth-header user password)
  (format "Authorization: Basic ~a"
          (base64-encode
           (string->bytes/utf-8
            (format "~a:~a"
                    user password)))))

(define cache-path "/tmp/git-index.cache")
(define (github-api/pages id secret token path query)
  (let loop ([page 1])
    (dprintf "loop ~v\n" (vector id secret token path query page))
    (define api-u
      (url "https" #f "api.github.com" #f #t
           (map (λ (x) (path/param x empty)) path)
           (append query
                   (list (cons 'client_id id)
                         (cons 'client_secret secret)
                         (cons 'page (number->string page))))
           #f))
    (make-directory* cache-path)
    (define api-u-cache-path
      (build-path
       cache-path
       (string->path-element
        (regexp-replace* #rx"/"
                         (url->string api-u)
                         "_"))))
    (define r
      (cond
        [(and #f ;; cache is turned off in production
              (file-exists? api-u-cache-path)
              (> (file-or-directory-modify-seconds api-u-cache-path)
                 (- (current-seconds) (* 24 60 60))))
         (with-input-from-file api-u-cache-path read-json)]
        [else
         (define-values (s hs rip)
           (http-sendrecv/url
            api-u
            #:headers (list (basic-auth-header
                             token
                             "x-oauth-basic"))))
         (define bs (port->bytes rip))
         (with-output-to-file api-u-cache-path
           (λ () (display bs))
           #:exists 'replace)
         (define r
           (with-handlers ([exn:fail?
                            (λ (x)
                              (eprintf "Error with JSON from ~a:\n"
                                       (url->string api-u))
                              (eprintf "~a\n" s)
                              (for ([h (in-list hs)])
                                (eprintf "~a\n" h))
                              (eprintf "\n~a\n"
                                       bs)
                              (raise x))])
             (read-json (open-input-bytes bs))))
         r]))
    (if (list? r)
      (append
       r
       (if (empty? r)
         empty
         (loop (add1 page))))
      empty)))

(module+ main
  (require racket/cmdline
           racket/runtime-path)
  (define push? #f)
  (define cron? #f)
  (command-line #:program "git-index"
                #:once-each
                ["--push"
                 "push every owned repo"
                 (set! push? #t)]
                ["--cron"
                 "running from cron"
                 (set! cron? #t)])

  (define-runtime-path index-dir ".")
  (define me "jeapostrophe")
  (define my-home "/home/jay/Dev/scm/github.jeapostrophe/")
  (define my-dist "/home/jay/Dev/dist/")
  (define owned-ignore
    (list "home" "work" "jpn"))
  (define observer-ignore
    (list "sf" "tom7" "suckless"))
  (define observer-rewrite
    ;; xxx this is derived for the fork's "parent"
    (hash "jeapostrophe/zguide" "imatix/zguide"
          "jeapostrophe/sxml" "jbclements/sxml"
          "jeapostrophe/BigCrunch" "hloople/BigCrunch"
          "jeapostrophe/glfw" "glfw/glfw"
          "jeapostrophe/FrameworkBenchmarks" "TechEmpower/FrameworkBenchmarks"
          "dyoo/whalesong" "samth/whalesong"
          "jeapostrophe/web-server-under-sandbox" "dyoo/web-server-under-sandbox"))

  (define id (file->string (build-path index-dir ".client_id")))
  (define secret (file->string (build-path index-dir ".client_secret")))
  (define token (file->string (build-path index-dir ".token")))

  (define-syntax-rule
    (dir-cond [(p ...) ty] ...
              [else e])
    (let/ec esc
      (let ([pth (build-path my-home p ...)])
        (when (directory-exists? pth)
          (esc pth ty)))
      ...
      e))

  ;; If we would warn, delete the cache so that next time maybe the
  ;; problem will go away
  (define (warning! . a)
    (when (directory-exists? cache-path)
      (delete-directory/files cache-path))
    (apply eprintf a))

  (define OWNED (make-hash))
  (define MINE (make-hash))
  (define (check-owner! r)
    (let/ec return
      (define n (hash-ref r 'name))
      (define-values (dir type)
        (dir-cond
         [(            n) 'active]
         [(   "0_DEAD" n) 'dead]
         [("0_DORMANT" n) 'dormant]
         [(  "0_MOVED" n) 'moved]
         [("0_REWRITE" n) 'rewrite]
         [else
          (let ()
            (return (warning! "to clone own: ~a\n" n)))]))

      (hash-set! OWNED n #t)
      (hash-set! MINE (format "~a/~a" me n) #t)

      ;; xxx maybe check if we should push

      (when push?
        (parameterize ([current-directory dir])
          (system "git push")))

      ;; xxx check homepage
      (define r-path (build-path dir "README"))

      (define (edit-readme)
        (unless cron?
          (parameterize ([current-directory dir])
            (system "touch README")
            (system "emacsclient -c README")
            (system "git add README")
            (system "git commit -m 'Adding readme' README"))))
      (define (set-desc! d)
        (system* "/usr/bin/curl"
                 "-X" "PATCH"
                 "-S" "-s"
                 "-o" "/tmp/git-index.url"
                 "-u"
                 (format "~a:~a" token "x-oauth-basic")
                 "-d"
                 (jsexpr->string (hash 'name n
                                       'description d
                                       'homepage ""))
                 (format "https://api.github.com/repos/~a/~a" me n)))

      (cond
        [(file-exists? r-path)
         (define l (file->lines r-path))
         (cond
           [(empty? l)
            (warning! "README is empty: ~a\n" n)
            (edit-readme)]
           [else
            (unless (eq? type 'active)
              (unless (regexp-match
                       (format "^\\[~a\\]"
                               (regexp-quote
                                (symbol->string type)))
                       (first l))
                (warning! "README missing type: ~a\n" n)
                (edit-readme)))
            (match* ((first l) (hash-ref r 'description))
              [(x x)
               (void)]
              [(x "")
               (warning! "git description missing ~a\n" n)
               (set-desc! x)]
              [(x y)
               (warning! "git description and README mismatch: ~a\n" n)
               (set-desc! x)])])]
        [else
         (warning! "Adding README: ~a\n" n)
         (edit-readme)])

      (define l-path (build-path dir "LICENSE"))
      (unless (file-exists? l-path)
        (warning! "Adding LICENSE: ~a\n" n)
        (copy-file (build-path index-dir "LICENSE") l-path)
        (parameterize ([current-directory dir])
          (system "git add LICENSE")
          (system "git commit -m 'Adding license' LICENSE")))))
  (define (dir-check-owner!)
    (define (bp . n)
      (apply build-path my-home n))
    (define (bdl d)
      (directory-list d #:build? #t))
    (define dirs (list (bp) (bp "0_DEAD") (bp "0_DORMANT")
                       (bp "0_MOVED") (bp "0_REWRITE")))
    (for ([d (in-list (append-map bdl dirs))]
          #:unless (member d dirs))
      (when (directory-exists? d)
        (define-values (_b np _m) (split-path d))
        (define n (path->string np))
        (unless (or (hash-has-key? OWNED n)
                    (member n owned-ignore)
                    (hash-has-key?
                     OWNED
                     ;; xxx check has_wiki
                     (regexp-replace (regexp-quote ".wiki")
                                     n
                                     "")))
          (warning! "   to upload: ~a\n" n)))))

  (define OBSERVED (make-hash))
  (define (check-observer! r)
    (define raw-n (hash-ref r 'full_name))
    (define n
      (or (hash-ref observer-rewrite raw-n #f)
          raw-n))
    (unless (hash-has-key? MINE n)
      (define p (build-path my-dist n))
      (unless (directory-exists? p)
        (warning! "    to clone: ~a\n" n))

      ;; xxx maybe check to see if can be updated?
      ;; git remote update
      ;; git status | grep behind > /dev/null

      (hash-set! OBSERVED n #t)))
  (define (dir-check-observer!)
    (for ([up (in-list (directory-list my-dist))])
      (define fup (build-path my-dist up))
      (cond
        [(directory-exists? fup)
         (define u (path->string up))
         (let/ec esc
           (define has-dir? #f)
           (for ([rp (in-list (directory-list fup))])
             (define frp (build-path fup rp))
             (when (directory-exists? frp)
               (set! has-dir? #t)
               (define r (path->string rp))
               (unless (or (hash-has-key? OBSERVED (format "~a/~a" u r))
                           (member u observer-ignore))
                 (esc (warning! "     to star: ~a [~a]\n" u r)))))
           (unless has-dir?
             (warning! "     to star: ~a\n" u)))]
        [(equal? up ".DS_Store")
         (void)]
        [else
         (warning! "non-directory in dist: ~a\n" up)])))

  (define orgs
    (map (λ (r)
           (hash-ref r 'login))
         (github-api/pages id secret token (list "user" "orgs") empty)))
  (define repos-orgs
    (flatten (map (λ (o)
                    (github-api/pages id secret token
                                      (list "orgs" o "repos")
                                      (list (cons 'type "member"))))
                  orgs)))
  (define repos-owned
    (github-api/pages id secret token
                      (list "user" "repos")
                      (list (cons 'type "owner"))))
  (define-values (repos-forks repos-owned-mine)
    (partition (λ (r) (hash-ref r 'fork))
               (filter (λ (r)
                         (not (regexp-match #rx"^racket/" (hash-ref r 'full_name))))
                       (append repos-owned repos-orgs))))
  (for-each check-owner! repos-owned-mine)

  (define repos-member
    (github-api/pages id secret token
                      (list "user" "repos")
                      (list (cons 'type "member"))))
  (define repos-starred
    (github-api/pages id secret token
                      (list "users" me "starred")
                      empty))
  (for-each check-observer! (append repos-forks repos-member repos-starred))

  (dir-check-observer!)
  (dir-check-owner!)

  (let ()
    (define exp (build-path my-home "exp"))
    (define-syntax-rule (regexp-match-or p rx ...)
      (or (regexp-match rx p) ...))
    (define (git-ignored? p)
      (regexp-match-or p
                       "README"
                       "LICENSE"
                       "compiled"
                       "dict.rktd"
                       "workout-plan-inner.tex"
                       "workout-plan.html"
                       (regexp-quote ".git")
                       (format "^~a.*" (regexp-quote ".#"))
                       "^#.*#$"
                       "~$"
                       "aux$"
                       "log$"
                       "bak$"
                       "pdf$"))
    (define files
      (filter-not git-ignored?
                  (sort (map path->string (directory-list exp)) string-ci<=?)))
    (define (rest* l)
      (if (empty? l) empty (rest l)))
    (define idx (rest* (rest* (file->lines (build-path exp "README")))))
    (let/ec esc
      (let loop ([fs files] [is idx])
        (match* (fs is)
          [((list) (list))
           (void)]
          [((cons f fs) (list))
           (warning! "~~exp - missing idx for ~a\n" f)
           (loop fs empty)]
          [((list) (cons i is))
           (esc (warning! "~~exp - missing file for ~a\n" i))]
          [((cons f fs) (cons i is))
           (cond
             [(regexp-match (format "^~a - " (regexp-quote f)) i)
              (loop fs is)]
             [else
              (warning! "~~exp - missing idx for ~a\n" f)
              (loop fs (cons i is))])])))))
