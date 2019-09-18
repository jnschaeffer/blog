(define-module (basic-theme)
  #:export (basic-theme)
  #:use-module (haunt builder blog)
  #:use-module (haunt post)
  #:use-module (haunt site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19))

(define (format-date date)
  (date->string date "~Y-~m-~d"))

(define (layout site title tree)
  `(html
    (head
     (meta (@ (charset "UTF-8")))
     (title ,(format #f "~a - ~a" title (site-title site)))
     (link (@ (rel "stylesheet") (href "/assets/styles.css"))))
    (body
     (div (@ (class "header"))
          (h1 (@ (class "site-title"))
              (a (@ (href ,(site-domain site))) ,(site-title site))))
     (div (@ (class "container"))
          ,@tree))))

(define (post-template post)
  `((div (@ (class "page-title"))
         (h2 ,(post-ref post 'title))
         (time ,(format-date (post-date post))))
    ,@(post-sxml post)))

(define (make-post-entry site post prefix)
  (let* ((domain (site-domain site))
         (filename (format #f "~a.html" (post-slug post)))
         (href (if prefix
                   (format #f "~a/~a/~a" domain prefix filename)
                   (format #f "~a/~a" domain filename))))
    `(li (@ (class "toc-item"))
         (a (@ (href ,href)),(post-ref post 'title))
	 " "
         (time (@ (class "date"))
               ,(format-date (post-date post)))
         (p (@ (class "summary"))
            ,(post-ref post 'summary)))))

(define (make-link-list site posts prefix)
  (let ((f (lambda (post prev)
             (cons (make-post-entry site post prefix) prev))))
    `(ul (@ (class "toc-items"))
         ,@(fold-right f '() posts))))

(define (collection-template site title posts prefix)
  `((div (@ (class "page-title"))
         (h2 ,title))
    ,(make-link-list site posts prefix))
  )

(define basic-theme (theme #:name "Basic theme"
                           #:layout layout
                           #:post-template post-template
                           #:collection-template collection-template))
