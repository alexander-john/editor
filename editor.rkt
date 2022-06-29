;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

; Editor -> Image
; renders (editor-pre ed) to the left of a cursor
; and (editor-post ed) to the right, within an empty scene
(check-expect (render (make-editor "hello" "world"))
              (overlay/align "left" "center"
               (beside
                (text "hello" 16 "black")
                (rectangle 1 20 "solid" "red")
                (text "world" 16 "black"))
               (empty-scene 200 20)))

(define (render ed)
  (overlay/align "left" "center"
               (beside
                (text (editor-pre ed) 16 "black")
                (rectangle 1 20 "solid" "red")
                (text (editor-post ed) 16 "black"))
               (empty-scene 200 20)))

; Editor KeyEvent -> Editor
; adds a single-character, ke, to the left of the cursor
; unless, ke denotes "\b". In that case, delete the character
; immediately to the left of the cursor. Function ignores "\t",
; "\r". The arrow moves the cursor one character
; to the "left" or "right". All other KeyEvents are ignored.
(check-expect (edit (make-editor "hello" "world") "a")
              (make-editor "helloa" "world"))
(check-expect (edit (make-editor "hello" "world") "\b")
              (make-editor "hell" "world"))
(check-expect (edit (make-editor "hello" "world") "\t")
              (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") "\r")
              (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") "left")
              (make-editor "hell" "oworld"))
(check-expect (edit (make-editor "hello" "world") "right")
              (make-editor "hellow" "orld"))
(check-expect (edit (make-editor "hello" "world") " ")
              (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello" "world") "else")
              (make-editor "hello" "world"))

(define (edit ed ke)
  (cond
    [(= (string-length ke) 1)
     (make-editor
      (cond
        [(string=? "\b" ke) (string-remove-last (editor-pre ed))]
        [(string=? "\t" ke) (editor-pre ed)]
        [(string=? "\r" ke) (editor-pre ed)]
        [else (string-append (editor-pre ed) ke)])
      (editor-post ed))]
    [(string=? "left" ke) (make-editor
                           (string-remove-last (editor-pre ed))
                           (string-append
                            (string-last (editor-pre ed))
                            (editor-post ed)))]
    [(string=? "right" ke) (make-editor
                           (string-append (editor-pre ed)
                                          (string-first (editor-post ed)))
                           (string-rest (editor-post ed)))]
    [else (make-editor (editor-pre ed) (editor-post ed))]))

; String -> 1String
; extracts the first 1String from a non-empty string
(check-expect (string-first "hello") "h")
(define (string-first s)
  (string-ith s 0))

; String -> 1String
; extracts the last 1String from a non-empty string
(check-expect (string-last "hello") "o")
(define (string-last s)
  (string-ith s (- (string-length s) 1)))

; String -> String
; produces a string like the given one with the
; last 1String removed
(check-expect (string-remove-last "hello") "hell")
(define (string-remove-last s)
  (substring s 0
             (- (string-length s) 1)))

; String -> String
; produces a string like the given one with the
; first character removed
(check-expect (string-rest "hello") "ello")
(define (string-rest s)
  (substring s 1))

; Editor -> Editor
; simulates a graphical editor
(define (run e)
  (big-bang e
    [to-draw render]
    [on-key edit]))