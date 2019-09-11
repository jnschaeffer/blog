(add-to-load-path (getcwd))

(use-modules (basic-theme)
	     (haunt builder assets)
	     (haunt builder blog)
             (haunt post)
	     (haunt reader)
	     (haunt site))

(site #:title "blog.experimental.systems"
      #:domain "http://localhost:8080"
      #:default-metadata
      '((author . "John Schaeffer")
	(email . "john@schaeffer.io"))
      #:readers (list sxml-reader)
      #:builders (list (blog #:theme basic-theme
                             #:collections (list
                                            (list
                                             "Recent posts"
                                             "index.html"
                                             posts/reverse-chronological)))
		       (static-directory "assets")))
