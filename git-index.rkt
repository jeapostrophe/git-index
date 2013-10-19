#lang racket/base
(require racket/list
         net/url
         racket/function
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
    (define-values (s hs ip)
      (http-sendrecv/url
       api-u
       #:headers (list (basic-auth-header
                        token
                        "x-oauth-basic"))))

    (define r (read-json ip))
    (append
     r
     (if (empty? r)
       empty
       (loop (add1 page))))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path index-dir ".")
  (define-runtime-path root-dir "..")
  (define id (file->string (build-path index-dir ".client_id")))
  (define secret (file->string (build-path index-dir ".client_secret")))
  (define token (file->string (build-path index-dir ".token")))

  (github-api/pages id secret token (list "user" "orgs") empty)

  ;; xxx find all i have access to
  ;; xxx make sure they are in the right spots in the filesystem
  ;; xxx make sure ones i own have a README, LICENSE, and a reference to the index

  (define rs
    (github-api/pages id secret token 
                      (list "orgs" "get-bonus" "repos")
                      ;; (list "user" "repos")
                      (if #t
                        (list (cons 'type "member"))
                        (list (cons 'type "owner")))))
  (for ([r (in-list rs)]
        [i (in-naturals)])
    (printf "~a. ~a = ~a [~a]\n"
            i
            (hash-ref r 'name)
            (hash-ref r 'description)
            (hash-ref r 'html_url))))
