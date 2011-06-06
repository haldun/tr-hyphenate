(in-package :common-lisp-user)

(defpackage :tr.gen.hb.hecele
  (:use :common-lisp)
  (:export :hyphenate))

(in-package :tr.gen.hb.hecele)

(defconstant +tests+
  '(("cccvcc" . 5) ("cccvcv" . 4)
    ("cccvc" . 5) ("ccvccc" . 5)
    ("ccvccv" . 4) ("ccvcc" . 5)
    ("ccvcc" .  4) ("ccvcv" . 3)
    ("ccvc" . 4) ("ccvv" . 3)
    ("ccv" . 3) ("cvccc" .  4)
    ("cvccv" . 3) ("cvcc" .  4) ("cvcv" .  2)
    ("cvc" .  3)  ("cv" .  2) ("vccc" .  3)
    ("vccv" .  2) ("vcc" .  3) ("vcvc" .  1)
    ("vcv" .  1)  ("vc" . 2) ("v" . 1)))

(defconstant +vowels+
  '(#\a #\A #\e #\E #\ı #\I #\o #\O #\ö #\Ö #\u #\Ü))

(defun hyphenate (word)
  "Returns a list containing the hyphens of the given word.
Raises an error, if it is not possible to hyphenize the word."
  (if (string-equal word "")
      nil
      (let ((hyphen (first-hyphen word)))
        (if hyphen
            (cons hyphen (hyphenate (subseq word (length hyphen))))
            (error "Cannot hyphenate ~s" word)))))

(defun first-hyphen (word)
  "Determines the first hyphen of the given word due to the
hyphenation tests. Returns NIL if it cannot extract the first
hyphen of the word."
  (let ((cv-word (convert-to-cv word)))
    (dolist (test +tests+)
      (if (starts-with cv-word (car test))
          (return (subseq word 0 (cdr test)))))))

(defun convert-to-cv (word)
  "Converts the given word into a c-v notation word. Eg. haldun => cvccvc"
  (let ((retval ""))
    (do-string (char word)
      (setf retval (concatenate 'string retval (if (vowel-p char) "v" "c"))))
    retval))

(defun vowel-p (char)
  "Returns a non-nil value if CHAR is a vowel."
  (member char +vowels+ :test #'char-equal))

;; utilities
(defun starts-with (str prefix)
  "Returns a non-nil value, if STR starts with PREFIX. Case insensitive."
  (and (>= (length str) (length prefix))
       (string-equal (subseq str 0 (length prefix)) prefix)))

(defmacro with-gensyms ((&rest syms) &body body)
  "A classical with-gensyms macro."
  `(let ,(mapcar #'(lambda (s) (list s '(gensym))) syms)
    ,@body))

(defmacro do-string ((var str) &body body)
  "A macro which enables iteration over a given string. Very similar to dolist."
  (with-gensyms (gvar glength gstr)
    `(let* ((,gstr ,str)
            (,glength (length ,gstr)))
      (do ((,gvar 0 (+ ,gvar 1)))
          ((>= ,gvar ,glength))
        (let ((,var (char ,gstr ,gvar)))
          ,@body)))))
