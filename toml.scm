;; toml.scm - a minimal TOML subset for .helix/tasks.toml
;;
;; Supports the bits a task file actually needs:
;;
;;   [task.build]
;;   cmd = "cargo build"
;;   description = "compile the workspace"
;;   cwd = "crates/app"
;;
;;   [task.run]
;;   cmd = ["cargo", "run", "--", "--help"]
;;
;; - `#` comments (outside of strings)
;; - double quoted strings with \n \t \r \" \\ \uXXXX escapes
;; - single quoted (literal) strings
;; - arrays of strings
;; - sections other than `[task.<name>]` are ignored, together with their keys
;;
;; String keys are lowercased. Unknown keys are preserved but unused.
;;
;; The parser returns a list of tasks, each one being a list of
;;   (name command description cwd)
;; where command/description/cwd are always strings.

(provide tasks-trim
         tasks-downcase
         tasks-parse-string
         tasks-parse-file
         tasks-task-name
         tasks-task-cmd
         tasks-task-description
         tasks-task-cwd)

(define (tasks-trim s)
  (define n (string-length s))
  (define start
    (let loop ([i 0])
      (if (and (< i n) (char-whitespace? (string-ref s i))) (loop (+ i 1)) i)))
  (define end
    (let loop ([i (- n 1)])
      (if (and (>= i start) (char-whitespace? (string-ref s i))) (loop (- i 1)) (+ i 1))))
  (if (>= start end) "" (substring s start end)))

(define (tasks-downcase s)
  (list->string (map char-downcase (string->list s))))

(define (tasks-split-lines s)
  (let loop ([cs (string->list s)] [cur '()] [out '()])
    (cond
      [(null? cs) (reverse (cons (list->string (reverse cur)) out))]
      [(char=? (car cs) #\newline)
       (loop (cdr cs) '() (cons (list->string (reverse cur)) out))]
      [else (loop (cdr cs) (cons (car cs) cur) out)])))

;; Drop everything after a `#` that isn't inside of a string.
(define (tasks-strip-comment s)
  (let loop ([cs (string->list s)] [qch #f] [esc #f] [acc '()])
    (cond
      [(null? cs) (list->string (reverse acc))]
      [esc (loop (cdr cs) qch #f (cons (car cs) acc))]
      [(char=? (car cs) #\\) (loop (cdr cs) qch #t (cons (car cs) acc))]
      [(and qch (char=? (car cs) qch)) (loop (cdr cs) #f #f (cons (car cs) acc))]
      [(and (not qch) (or (char=? (car cs) #\") (char=? (car cs) #\')))
       (loop (cdr cs) (car cs) #f (cons (car cs) acc))]
      [(and (not qch) (char=? (car cs) #\#)) (list->string (reverse acc))]
      [else (loop (cdr cs) qch #f (cons (car cs) acc))])))

;; Index of the first `=` that is not inside of a string, or #f.
(define (tasks-find-eq s)
  (let loop ([cs (string->list s)] [qch #f] [esc #f] [i 0])
    (cond
      [(null? cs) #f]
      [esc (loop (cdr cs) qch #f (+ i 1))]
      [(char=? (car cs) #\\) (loop (cdr cs) qch #t (+ i 1))]
      [(and qch (char=? (car cs) qch)) (loop (cdr cs) #f #f (+ i 1))]
      [(and (not qch) (or (char=? (car cs) #\") (char=? (car cs) #\')))
       (loop (cdr cs) (car cs) #f (+ i 1))]
      [(and (not qch) (char=? (car cs) #\=)) i]
      [else (loop (cdr cs) qch #f (+ i 1))])))

;; Split on commas that are not inside of a string.
(define (tasks-split-commas s)
  (let loop ([cs (string->list s)] [qch #f] [esc #f] [cur '()] [out '()])
    (cond
      [(null? cs) (reverse (cons (list->string (reverse cur)) out))]
      [esc (loop (cdr cs) qch #f (cons (car cs) cur) out)]
      [(char=? (car cs) #\\) (loop (cdr cs) qch #t (cons (car cs) cur) out)]
      [(and qch (char=? (car cs) qch)) (loop (cdr cs) #f #f (cons (car cs) cur) out)]
      [(and (not qch) (or (char=? (car cs) #\") (char=? (car cs) #\')))
       (loop (cdr cs) (car cs) #f (cons (car cs) cur) out)]
      [(and (not qch) (char=? (car cs) #\,))
       (loop (cdr cs) #f #f '() (cons (list->string (reverse cur)) out))]
      [else (loop (cdr cs) qch #f (cons (car cs) cur) out)])))

(define (tasks-unescape s)
  (let loop ([cs (string->list s)] [acc '()])
    (cond
      [(null? cs) (list->string (reverse acc))]
      [(char=? (car cs) #\\)
       (cond
         [(null? (cdr cs)) (loop '() (cons #\\ acc))]
         [else
          (define nxt (car (cdr cs)))
          (cond
            [(char=? nxt #\n) (loop (cddr cs) (cons #\newline acc))]
            [(char=? nxt #\t) (loop (cddr cs) (cons #\tab acc))]
            [(char=? nxt #\r) (loop (cddr cs) (cons #\return acc))]
            [(char=? nxt #\") (loop (cddr cs) (cons #\" acc))]
            [(char=? nxt #\\) (loop (cddr cs) (cons #\\ acc))]
            [(char=? nxt #\u)
             (define rest (cdr cs))
             (if (>= (length rest) 5)
                 (let* ([hex (list->string (take (cdr rest) 4))]
                        [code (with-handler (lambda (_) #f) (string->number hex 16))])
                   (if (number? code)
                       (loop (drop rest 5) (cons (integer->char code) acc))
                       (loop (cdr cs) (cons nxt acc))))
                 (loop (cdr cs) (cons nxt acc)))]
            [else (loop (cdr cs) (cons nxt acc))])])]
      [else (loop (cdr cs) (cons (car cs) acc))])))

(define (tasks-parse-array raw)
  (define n (string-length raw))
  (define inner
    (if (and (>= n 2) (char=? (string-ref raw (- n 1)) #\]))
        (substring raw 1 (- n 1))
        (substring raw 1 n)))
  (filter (lambda (v) (not (equal? v "")))
          (map (lambda (part) (tasks-parse-value (tasks-trim part)))
               (tasks-split-commas inner))))

(define (tasks-parse-value raw)
  (define n (string-length raw))
  (cond
    [(= n 0) ""]
    [(char=? (string-ref raw 0) #\")
     (if (and (> n 1) (char=? (string-ref raw (- n 1)) #\"))
         (tasks-unescape (substring raw 1 (- n 1)))
         (tasks-unescape (substring raw 1 n)))]
    [(char=? (string-ref raw 0) #\')
     (if (and (> n 1) (char=? (string-ref raw (- n 1)) #\'))
         (substring raw 1 (- n 1))
         (substring raw 1 n))]
    [(char=? (string-ref raw 0) #\[) (tasks-parse-array raw)]
    [else raw]))

(define (tasks-add-field entries name key val)
  (cond
    [(null? entries) entries]
    [(equal? (car (car entries)) name)
     (cons (cons name (cons (cons key val) (cdr (car entries))))
           (cdr entries))]
    [else (cons (car entries) (tasks-add-field (cdr entries) name key val))]))

(define (tasks-lookup fields key)
  (cond
    [(null? fields) #f]
    [(equal? (car (car fields)) key) (cdr (car fields))]
    [else (tasks-lookup (cdr fields) key)]))

(define (tasks-field-or fields key fallback)
  (let ([v (tasks-lookup fields key)])
    (if v v fallback)))

(define (tasks-finalize entry)
  (define fields (reverse (cdr entry)))
  (list (car entry)
        (tasks-field-or fields "cmd" "")
        (let ([v (tasks-lookup fields "description")])
          (if v v (tasks-field-or fields "label" "")))
        (tasks-field-or fields "cwd" "")))

(define (tasks-parse-string text)
  (define entries '())
  (define current #f)
  (for-each
   (lambda (raw-line)
     (define line (tasks-trim (tasks-strip-comment raw-line)))
     (unless (equal? line "")
       (cond
         [(and (>= (string-length line) 3)
               (char=? (string-ref line 0) #\[)
               (char=? (string-ref line (- (string-length line) 1)) #\]))
          (define inner (tasks-trim (substring line 1 (- (string-length line) 1))))
          (define prefix "task.")
          (define plen (string-length prefix))
          (if (and (>= (string-length inner) plen)
                   (equal? (tasks-downcase (substring inner 0 plen)) prefix))
              (let ([name (tasks-trim (substring inner plen (string-length inner)))])
                (set! current name)
                (set! entries (cons (cons name '()) entries)))
              (set! current #f))]
         [else
          (when current
            (define eq (tasks-find-eq line))
            (when eq
              (define key (tasks-downcase (tasks-trim (substring line 0 eq))))
              (define val (tasks-parse-value
                           (tasks-trim (substring line (+ eq 1) (string-length line)))))
              (set! entries (tasks-add-field entries current key val))))])))
   (tasks-split-lines text))
  (reverse (map tasks-finalize entries)))

(define (tasks-parse-file path)
  (with-handler
    (lambda (_) #f)
    (let ([port (open-input-file path)])
      (define text (read-port-to-string port))
      (close-input-port port)
      (tasks-parse-string text))))

(define (tasks-task-name task) (list-ref task 0))
(define (tasks-task-cmd task) (list-ref task 1))
(define (tasks-task-description task) (list-ref task 2))
(define (tasks-task-cwd task) (list-ref task 3))
