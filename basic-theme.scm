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
     (title ,(format #f "~a - ~a" title (site-title site)))
     (link (@ (rel "stylesheet") (href "/assets/styles.css"))))
    (body
     (div (@ (class "header"))
	  (h1 (a (@ (href ,(site-domain site))) ,(site-title site))))
     (div (@ (class "container"))
	  ,@tree))))

(define (post-template post)
  `((h2 ,(post-ref post 'title))
    (p (@ (class "date")) ,(format-date (post-date post)))
    ,@(post-sxml post)))

(define (make-post-entry site post prefix)
  (let* ((domain (site-domain site))
	 (filename (format #f "~a.html" (post-slug post)))
	 (href (if prefix
		   (format #f "~a/~a/~a" domain prefix filename)
		   (format #f "~a/~a" domain filename))))
    `(li ((p (a (@ (href ,href)) ,(post-ref post 'title)) " - "
	     (span (@ (class "date")) ,(format-date (post-date post))))
	  (p ,(post-ref post 'summary))))))

(define (make-link-list site posts prefix)
  (let ((f (lambda (post prev)
	     (cons (make-post-entry site post prefix) prev))))
    `(ul ,@(fold-right f '() posts))))

(define (collection-template site title posts prefix)
  `((h2 ,title)
    ,(make-link-list site posts prefix))
  )

(define basic-theme (theme #:name "Basic theme"
			   #:layout layout
			   #:post-template post-template
			   #:collection-template collection-template))
