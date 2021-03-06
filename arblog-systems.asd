;;;; arblog-policies.asd

(defsystem #:arblog-systems)

;;;; datastores

(defsystem #:arblog-datastore-mongodb
  :depends-on (#:arblog #:mongo-cl-driver.usocket #:ironclad)
  :pathname "policies/datastore/"
  :components ((:file "mongodb")))

;;;; markups

(defsystem #:arblog-markup-rst
  :depends-on (#:arblog #:docutils #:colorize #:cl-libxml2)
  :pathname "policies/markup/"
  :components ((:file "rst")))

(defsystem #:arblog-markup-markdown
  :depends-on (#:arblog #:cl-markdown)
  :pathname "policies/markup/"
  :components ((:file "markdown")))

;;;; themes

(defsystem #:arblog-theme-common
    :depends-on (#:arblog)
    :pathname "policies/theme/"
    :components ((:file "common")))

(defsystem #:arblog-theme-mirev
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:arblog #:arblog-theme-common)
  :pathname "policies/theme/mirev/"
  :serial t
  :components ((:module "templates"
                        :components ((:closure-template "page")
                                     (:closure-template "entries")
                                     (:closure-template "tags")
                                     (:closure-template "archive")
                                     (:closure-template "admin")))
               (:file "mirev")))

(defsystem #:arblog-theme-isimple
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:arblog #:arblog-theme-common)
  :pathname "policies/theme/isimple/"
  :serial t
  :components ((:module "templates"
                        :components ((:closure-template "isimple")))
               (:file "isimple")))


(defsystem #:arblog-theme-just-dance
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:arblog #:arblog-theme-common)
  :pathname "policies/theme/just-dance"
  :serial t
  :components ((:module "templates"
                        :components ((:closure-template "all")))
               (:file "just-dance")))

