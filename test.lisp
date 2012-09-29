(cl:in-package :allegretto-regexp.internal)

(def-suite allegretto-regexp)

(in-suite allegretto-regexp)


(defmacro *= (x &rest y)
  `(is (equal (multiple-value-list ,x) (list ,@y))))


(test basic
  (*= (match-re "(abc|def)(.*)" "defabc")
      T
      "defabc"
      "def"
      "abc" )
  (is (equalp (match-re "(abc|def)(.*)" "defabc" :return :match)
              #S(REGULAR-EXPRESSION-MATCH
                 :INDEXES ((0 . 6) (0 . 3) (3 . 6))
                 :INPUT "defabc"
                 :NUM-SUBMATCHES 2
                 :NAMED-SUBMATCHES NIL)))
  (*= (match-re "^[A-E]" "Boo")
      T
      "B" )
  (*= (match-re "i.*s" "mississippi")
      T
      "ississ" )
  (*= (match-re "i.*?s" "mississippi")
      T
      "is" )
  (*= (match-re "i.*?s" "mississippi" :return :index )
      T
      '(1 . 3) )
  (*= (match-re "(abc|def)(.*)" "defabc" :return :index)
      T
      '(0 . 6)
      '(0 . 3)
      '(3 . 6) )
  (is (equalp
       (match-re "(?<Z>abc|def)(?<X>.*)" "defabc" :return :match)
       #S(REGULAR-EXPRESSION-MATCH
          :INDEXES ((0 . 6) (0 . 3) (3 . 6))
          :INPUT "defabc"
          :NUM-SUBMATCHES 2
          :NAMED-SUBMATCHES (("X" 2) ("Z" 1)))))
  )


(test multiple-lines 
  (*= (match-re "^abc$" "abc
abc")
      NIL)
  (*= (match-re "(^abc$)" "abc
abc"
          :multiple-lines t)
      T
      "abc"
      "abc")
  (*= (match-re "." "
"
          :single-line t)
      T
"
")
  (*= (match-re "^abc$" "abc
abc"
                :multiple-lines t)
      T
      "abc")
  (*= (match-re
       "
    (\\d{2}) # 時
    :
    (\\d{2}) # 分
    :
    (\\d{2}) # 秒
"
       "03:02:56" :ignore-whitespace t)
      T
      "03:02:56"
      "03"
      "02"
      "56"))


(defun foo ()
  (funcall (re-lambda "(?<FOO>[a-z]+)(?<BAR>\\d+)" (foo bar)
             (list foo bar))
           "   acl70beta "))


(test foo
  (is (equal (foo) (list "acl" "70"))))


(test re-lambda
  (is (equal (funcall (re-lambda "(abc|def)(.*)" ((a 0) (b 1) (c 2))
                        (list a b c) )
                      "defabc" )
             (list "defabc" "def" "abc") ))
  (is (equal (funcall (re-lambda "(?<FOO>[a-z]+)(?<BAR>\\d+)" (foo bar)
                        (list foo bar) )
                      "   acl70beta " )
             (list "acl" "70"))
      ))


(test re-let
  (is (equal (re-let "(abc|def)(.*)" "defabc"
                     ((a 0) (b 1) (c 2))
               (list a b c))
             (list "defabc" "def" "abc")) ))


;;; サポートしてません
#|(funcall (re-lambda "cde" ((a 0 :before) (b 0) (c 0 :after))
           (list a b c))
         "abcdefg")|#

;=>  ("cde" "cde" "cde")

;;; Allegro: regex2
;=> ("ab" "cde" "fg")


(test re-case 
  (is (equal (re-case "foo the barmy"
               ("foo a (.*)" ((it 1)) (list it))
               ("foo the (.*)" ((it 1)) (list it))
               (t :no-match) )
             (list "barmy") ))
  (is (equal (re-case "foo a barmy"
	       ("foo a (.*)" ((it 1)) (list it))
               ("foo the (.*)" ((it 1)) (list it))
               (t :no-match))
             (list "barmy")))
  (is (equal (re-case "foo xx barmy"
	      ("foo a (.*)" ((it 1)) (list it))
	      ("foo the (.*)" ((it 1)) (list it))
	      (t :no-match))
             :no-match))
  (is (equal 
       (labels ((parse-date (input)
                  (re-case input
                    ("^(\\d{4})([-/])(\\d{1,2})\\2(\\d{1,2})$"
                     ((year 1) (month 3) (day 4))
                     (mapcar #'read-from-string (list year month day)))
                    ("^(\\d{1,2})([-/])(\\d{1,2})\\2(\\d{4})$"
                            ((year 4) (month 1) (day 3))
                            (mapcar #'read-from-string (list year month day)))
                    ("^(\\d{2})([-/])(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\2(\\d{4})$"
                     ((year 4) (month 3) (day 1))
                     (list
                      (read-from-string year)
                      (cdr
                       (assoc month
                              '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3)
                                ("Apr" . 4) ("May" . 5) ("Jun" . 6)
                                ("Jul" . 7) ("Aug" . 8) ("Sep" . 9)
                                ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
                              :test #'equal))
                      (read-from-string day))))))
         (list (parse-date "2004/7/21")
               (parse-date "7-21-2004")
               (parse-date "21-Jul-2004")))
       (copy-list '((2004 7 21) (2004 7 21) (2004 7 21))))))


;;; eof
