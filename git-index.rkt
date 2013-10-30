#lang racket/base
(require racket/list
         net/url
         racket/function
         racket/path
         net/base64
         racket/port
         json
         racket/file)

(define (basic-auth-header user password)
  (format "Authorization: Basic ~a"
          (base64-encode
           (string->bytes/utf-8
            (format "~a:~a"
                    user password)))))

(define cache-path "/tmp/git-index.cache")
(module+ main
  (make-directory* cache-path))
(define (github-api/pages id secret token path query)
  (let loop ([page 1])
    (define api-u
      (url "https" #f "api.github.com" #f #t
           (map (λ (x) (path/param x empty)) path)
           (append query
                   (list (cons 'client_id id)
                         (cons 'client_secret secret)
                         (cons 'page (number->string page))))
           #f))
    (define api-u-cache-path
      (build-path
       cache-path
       (string->path-element
        (regexp-replace* #rx"/"
                         (url->string api-u)
                         "_"))))
    (define r
      (cond
        [(and (file-exists? api-u-cache-path)
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
         (define r (read-json (open-input-bytes bs)))
         r]))
    (append
     r
     (if (empty? r)
       empty
       (loop (add1 page))))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path index-dir ".")
  (define me "jeapostrophe")
  (define my-home "/home/jay/Dev/scm/github.jeapostrophe/")
  (define my-dist "/home/jay/Dev/dist/")
  (define owned-ignore
    (list "home" "work" "jpn"))
  (define observer-rewrite
    (hash "jeapostrophe/zguide" "imatix/zguide"
          "jeapostrophe/web-server-under-sandbox" "dyoo/web-server-under-sandbox"))

  (define id (file->string (build-path index-dir ".client_id")))
  (define secret (file->string (build-path index-dir ".client_secret")))
  (define token (file->string (build-path index-dir ".token")))

  ;; xxx look at everything in ~exp to see if it is documented

  (define-syntax-rule
    (dir-cond [(p ...) ty] ...
              [else e])
    (let/ec esc
      (let ([pth (build-path my-home p ...)])
        (when (directory-exists? pth)
          (esc pth ty)))
      ...
      e))

  (define warning! eprintf)

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
          (return (warning! "to clone own: ~a\n" n))]))
      (hash-set! OWNED n #t)
      (hash-set! MINE (format "~a/~a" me n) #t)
      ;; xxx make sure ones i own have a README, LICENSE, and a
      ;; reference to the index
      (void)))
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
                    (member n owned-ignore))
          (warning! "   to upload: ~a\n" n)))))

  (define OBSERVED (make-hash))
  (define (check-observer! r)
    (define n (hash-ref r 'full_name))
    (unless (hash-has-key? MINE n)
      ;; xxx something should happen special for forks

      (define p (build-path my-dist n))
      (unless (directory-exists? p)
        (warning! "    to clone: ~a\n" n))
      (hash-set! OBSERVED n #t)))
  (define (dir-check-observer!)
    (for ([up (in-list (directory-list my-dist))])
      (define fup (build-path my-dist up))
      (when (directory-exists? fup)
        (define u (path->string up))
        (let/ec esc
          (for ([rp (in-list (directory-list fup))])
            (define frp (build-path fup rp))
            (when (directory-exists? frp)
              (define r (path->string rp))
              (unless (hash-has-key? OBSERVED (format "~a/~a" u r))
                (esc (warning! "     to star: ~a [~a]\n" u r)))))))))

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
               (append repos-owned repos-orgs)))
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

  (void))
