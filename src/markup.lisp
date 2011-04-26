;;;; markup.lisp

(in-package #:arblog)

(defvar *interpreted-roles*
  (alexandria:copy-hash-table docutils.parser.rst::*interpreted-roles*))

(defvar *directives*
  (alexandria:copy-hash-table docutils.parser.rst::*directives*))

(defmacro with-arblog-markup (&body body)
  `(let ((docutils.parser.rst::*interpreted-roles* *interpreted-roles*)
         (docutils.parser.rst::*directives* *directives*))
     ,@body))


;;;; hypespec-ref

(defclass hyperspec-ref (docutils.nodes:raw)
  ((spec :initarg :spec :reader hyperspec-ref-spec)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node hyperspec-ref))
  (docutils:part-append
   (docutils.writer.html::start-tag node
                                    "a"
                                    (list :href (clhs-lookup:spec-lookup (hyperspec-ref-spec node))
                                          :class "common-lisp-entity"))
   (hyperspec-ref-spec node)
   "</a>"))

(with-arblog-markup
  (docutils.parser.rst:def-role hs (spec)
    (make-instance 'hyperspec-ref
                   :spec spec)))



;;;; code-block

(defclass code-block (docutils.nodes:raw)
  ((lang :initarg :lang :initform nil :reader code-block-lang)
   (code :initarg :code :initform nil :reader code-block-code)))

(defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node code-block))
  (docutils:part-append (docutils.writer.html::start-tag node
                                                         "div"
                                                         '(:class "code")))
  (docutils:part-append (colorize::html-colorization :common-lisp
                                                     (code-block-code node)))
  (docutils:part-append "</div>"))

(with-arblog-markup 
  (docutils.parser.rst:def-directive code-block (parent lang &content content)
    (let ((node (docutils:make-node 'docutils.nodes:paragraph)))
      (docutils:add-child node
                          (make-instance 'code-block
                                         :lang lang
                                         :code (docutils::join-strings content #\Newline)))
      (docutils:add-child parent node))))

;;; generate html

(defun render-arblog-markup (text)
  (with-arblog-markup 
    (let ((doc (docutils:read-rst text))
          (writer (make-instance 'docutils.writer.html:html-writer)))
      (docutils:visit-node writer doc)
      (with-output-to-string (out)
        (iter (for part in  '(docutils.writer.html:body-pre-docinfo 
                              docutils.writer.html:docinfo
                              docutils.writer.html:body))
              (docutils:write-part writer part out))
        (format out "</div>")))))
         
  