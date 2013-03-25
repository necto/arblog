;;;; simple.lisp

(asdf:operate 'asdf:load-op '#:arblog)
(asdf:operate 'asdf:load-op '#:arblog-systems)

(asdf:operate 'asdf:load-op '#:arblog-datastore-mongodb)

(asdf:operate 'asdf:load-op '#:arblog-markup-rst)
;; (asdf:operate 'asdf:load-op '#:arblog-markup-markdown)

(asdf:operate 'asdf:load-op '#:arblog-theme-isimple)

(asdf:operate 'asdf:load-op '#:gallery)

(restas:define-module #:myblog
  (:use #:cl))

(in-package #:myblog)

(restas:mount-module -gallery- (#:gallery)
  (:url "gal")
  (gallery.internal.render:*render* (make-instance 'gallery.default-render:handler)))

(restas:mount-module -arblog- (#:arblog)
  (arblog:*blog-name* "My blog")
  (arblog:*posts-on-page* 10)
  
  (arblog.internal.datastore:*datastore* (make-instance 'arblog.datastore.mongodb:arblog-mongo-datastore))
  (arblog.internal.markup:*markup* (make-instance 'arblog.markup.rst:arblog-rst-markup))
  ;;(arblog.policy.markup:*markup* (make-instance 'arblog.markup.markdown:arblog-markdown-markup))
  (arblog.internal.theme:*theme* (make-instance 'arblog.theme.isimple:arblog-isimple-theme))
  
  (arblog:*disqus-enabled* nil))

(restas:start '#:myblog :port 8080)
