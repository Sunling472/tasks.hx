;; tasks.scm - project tasks for Helix + Steel
;;
;; Tasks live in `.helix/tasks.toml` (or `tasks.toml`) at the workspace root:
;;
;;   [task.build]
;;   cmd = "cargo build"
;;   description = "compile everything"   # optional
;;   cwd = "crates/app"                    # optional, relative to the root
;;
;; Commands:
;;   :tasks-toggle  - open/close the centered task picker (search as you type)
;;   :tasks-output  - open/close the output panel of the current/last task
;;   :tasks-reload  - re-read tasks.toml and report what was found
;;
;; Keys in the picker:
;;   type to filter, Up/Down (or Ctrl-p/Ctrl-n) move, Enter run,
;;   Ctrl-u clear the query, Esc close.
;;
;; Keys in the output panel:
;;   Esc close (task keeps running), Ctrl-k kill the whole process tree,
;;   r run again, Up/Down/PgUp/PgDn/Home/End scroll.
;;
;; Tasks are executed by the `hx-task` helper (hx-task.c, built with `make`),
;; which owns a pty and the process group, so Ctrl-k takes down the whole
;; tree and quitting helix does not leave tasks behind.

(require "helix/misc.scm")
(require "helix/components.scm")
(require-builtin steel/time)
(require "tasks/toml.scm")

(provide tasks-toggle tasks-output tasks-reload)

(define *TASKS-TICK-MS* 40)
(define *TASKS-MAX-LINES* 5000)

;; ---------------------------------------------------------------------------
;; picker state
;; ---------------------------------------------------------------------------

(define *tasks-active?* #f)
(define *tasks-root* "")
(define *tasks-file* "")
(define *tasks-all* '())
(define *tasks-filtered* '())
(define *tasks-query* "")
(define *tasks-cursor* 0)
(define *tasks-window* 0)
(define *tasks-visible* 8)
(define *tasks-error* #f)

;; ---------------------------------------------------------------------------
;; running task state
;; ---------------------------------------------------------------------------

(define *task-current* #f)
(define *task-name* #f)
(define *task-cmd* "")
(define *task-child* #f)
(define *task-channel* #f)
(define *task-run-id* 0)
(define *task-stdin* #f)
(define *task-started* 0)
(define *task-finished* 0)
(define *task-running?* #f)
(define *task-exit* #f)
(define *task-killed?* #f)
(define *task-killing?* #f)
(define *task-kill-deadline* #f)
(define *task-replace-pending?* #f)
(define *task-lines* '())
(define *task-line-count* 0)
(define *task-scroll* 0)
(define *task-follow?* #t)
(define *task-visible* 8)
(define *task-out-open?* #f)
(define *tasks-tick-scheduled* #f)

;; ---------------------------------------------------------------------------
;; small helpers
;; ---------------------------------------------------------------------------

(define (tasks-take lst n)
  (if (or (null? lst) (<= n 0)) '() (cons (car lst) (tasks-take (cdr lst) (- n 1)))))

(define (tasks-drop lst n)
  (if (or (null? lst) (<= n 0)) lst (tasks-drop (cdr lst) (- n 1))))

(define (tasks-truncate s max-w)
  (cond
    [(<= max-w 0) ""]
    [(<= (string-length s) max-w) s]
    [(<= max-w 1) "…"]
    [else (string-append (substring s 0 (- max-w 1)) "…")]))

(define (tasks-repeat-ch n ch)
  (if (<= n 0) "" (make-string n ch)))

(define (tasks-starts-with? s prefix)
  (define pl (string-length prefix))
  (and (>= (string-length s) pl) (equal? (substring s 0 pl) prefix)))

(define (tasks-chomp s)
  (define n (string-length s))
  (cond
    [(and (> n 0) (char=? (string-ref s (- n 1)) #\newline)) (tasks-chomp (substring s 0 (- n 1)))]
    [(and (> n 0) (char=? (string-ref s (- n 1)) #\return)) (tasks-chomp (substring s 0 (- n 1)))]
    [else s]))

(define (tasks-ms->s ms)
  (define secs (quotient ms 1000))
  (define tenths (quotient (remainder ms 1000) 100))
  (string-append (number->string secs) "." (number->string tenths) "s"))

(define (tasks-joined-cmd task)
  (define cmd (tasks-task-cmd task))
  (if (string? cmd) cmd (string-join cmd " ")))

(define (tasks-cwd-for task)
  (define root (if (equal? *tasks-root* "") "." *tasks-root*))
  (define cwd (tasks-task-cwd task))
  (cond
    [(and (string? cwd) (not (equal? cwd "")))
     (if (tasks-starts-with? cwd "/") cwd (string-append root "/" cwd))]
    [else root]))

(define (tasks-workspace-root)
  (with-handler (lambda (_) ".") (helix-find-workspace)))

(define (tasks-file-exists? path)
  (with-handler
    (lambda (_) #f)
    (let ([port (open-input-file path)])
      (close-input-port port)
      #t)))

(define (tasks-env-var name)
  (with-handler
    (lambda (_) "")
    (let* ([port (open-input-file "/proc/self/environ")]
           [raw (read-port-to-string port)])
      (close-input-port port)
      (let loop ([start 0])
        (cond
          [(>= start (string-length raw)) ""]
          [else
           (define limit
             (let scan ([i start])
               (if (or (>= i (string-length raw))
                       (= (char->integer (string-ref raw i)) 0))
                   i
                   (scan (+ i 1)))))
           (define entry (substring raw start limit))
           (define prefix (string-append name "="))
           (if (tasks-starts-with? entry prefix)
               (substring entry (string-length prefix) (string-length entry))
               (loop (+ limit 1)))])))))

(define (tasks-helper-path)
  (define override (tasks-env-var "HELIX_TASKS_HELPER"))
  (if (not (equal? override ""))
      override
      (string-append (tasks-env-var "HOME") "/.config/helix/tasks/bin/hx-task")))

(define (tasks-task-file root)
  (define candidates (list (string-append root "/.helix/tasks.toml")
                           (string-append root "/tasks.toml")))
  (let loop ([cs candidates])
    (cond
      [(null? cs) #f]
      [(tasks-file-exists? (car cs)) (car cs)]
      [else (loop (cdr cs))])))

;; ---------------------------------------------------------------------------
;; loading and filtering
;; ---------------------------------------------------------------------------

(define (tasks-load!)
  (define root (tasks-workspace-root))
  (set! *tasks-root* root)
  (define file (tasks-task-file root))
  (set! *tasks-file* (if file file ""))
  (cond
    [file
     (define parsed (tasks-parse-file file))
     (cond
       [(not parsed)
        (set! *tasks-all* '())
        (set! *tasks-error* (string-append "cannot read " file))]
       [else
        (set! *tasks-all*
              (filter (lambda (t) (not (equal? (tasks-joined-cmd t) ""))) parsed))
        (set! *tasks-error* (if (null? *tasks-all*) "no tasks with a cmd in this file" #f))])]
    [else
     (set! *tasks-all* '())
     (set! *tasks-error* (string-append "no .helix/tasks.toml in " root))])
  (set! *tasks-query* "")
  (set! *tasks-cursor* 0)
  (set! *tasks-window* 0)
  (tasks-refilter!))

;; Number of characters skipped while matching `needle` as a subsequence of
;; `hay`, or #f when it doesn't match at all. Lower is a tighter match.
(define (tasks-fuzzy-skip hay needle)
  (let loop ([h (string->list hay)] [n (string->list needle)] [skip 0])
    (cond
      [(null? n) skip]
      [(null? h) #f]
      [(char=? (car h) (car n)) (loop (cdr h) (cdr n) skip)]
      [else (loop (cdr h) n (+ skip 1))])))

(define (tasks-score task q)
  (define name (tasks-downcase (tasks-task-name task)))
  (define cmd (tasks-downcase (tasks-joined-cmd task)))
  (define desc (tasks-downcase (tasks-task-description task)))
  (define fuzzy (tasks-fuzzy-skip name q))
  (cond
    [(tasks-starts-with? name q) 0]
    [(string-contains? name q) 1]
    [fuzzy (+ 2 (min fuzzy 50))]
    [(or (string-contains? cmd q) (string-contains? desc q)) 100]
    [else #f]))

(define (tasks-before? a b)
  (define sa (car a))
  (define sb (car b))
  (cond
    [(< sa sb) #t]
    [(> sa sb) #f]
    [else
     (define na (tasks-task-name (list-ref a 1)))
     (define nb (tasks-task-name (list-ref b 1)))
     (if (equal? na nb) (string<? (tasks-joined-cmd (list-ref a 1)) (tasks-joined-cmd (list-ref b 1)))
         (string<? na nb))]))

(define (tasks-sort-by-score items)
  (sort items tasks-before?))

(define (tasks-refilter!)
  (define q (tasks-downcase *tasks-query*))
  (define items
    (if (equal? q "")
        (map (lambda (t) (list 0 t)) *tasks-all*)
        (let loop ([ts *tasks-all*] [acc '()])
          (cond
            [(null? ts) acc]
            [else
             (define s (tasks-score (car ts) q))
             (loop (cdr ts) (if s (cons (list s (car ts)) acc) acc))]))))
  (set! *tasks-filtered* (map (lambda (item) (list-ref item 1)) (tasks-sort-by-score items)))
  (when (> *tasks-cursor* (- (length *tasks-filtered*) 1))
    (set! *tasks-cursor* (max 0 (- (length *tasks-filtered*) 1))))
  (when (< *tasks-cursor* 0) (set! *tasks-cursor* 0)))

(define (tasks-move! delta)
  (define n (length *tasks-filtered*))
  (when (> n 0)
    (set! *tasks-cursor* (max 0 (min (- n 1) (+ *tasks-cursor* delta))))
    (tasks-keep-cursor-visible!)))

(define (tasks-keep-cursor-visible!)
  (define visible (max 1 *tasks-visible*))
  (when (< *tasks-cursor* *tasks-window*)
    (set! *tasks-window* *tasks-cursor*))
  (when (>= *tasks-cursor* (+ *tasks-window* visible))
    (set! *tasks-window* (- *tasks-cursor* visible -1))))

(define (tasks-type! ch)
  (set! *tasks-query* (string-append *tasks-query* (string ch)))
  (tasks-refilter!)
  (set! *tasks-cursor* 0)
  (set! *tasks-window* 0))

(define (tasks-backspace!)
  (define n (string-length *tasks-query*))
  (when (> n 0)
    (set! *tasks-query* (substring *tasks-query* 0 (- n 1)))
    (tasks-refilter!)
    (set! *tasks-cursor* 0)
    (set! *tasks-window* 0)))

;; ---------------------------------------------------------------------------
;; picker component
;; ---------------------------------------------------------------------------

(struct TasksListState ())

(define (tasks-list-metrics rect)
  (define tw (area-width rect))
  (define th (area-height rect))
  (define w (min (- tw 2) (max 24 (min (- tw 4) 84))))
  (define items (max 3 (min 12 (length *tasks-all*))))
  (define h (min (- th 2) (max 7 (+ items 5))))
  (list (max 0 (quotient (- tw w) 2))
        (max 0 (quotient (- th h) 2))
        w h))

(define (tasks-list-render _state rect frame)
  (define metrics (tasks-list-metrics rect))
  (define x (list-ref metrics 0))
  (define y (list-ref metrics 1))
  (define w (list-ref metrics 2))
  (define h (list-ref metrics 3))
  (define inner (max 1 (- w 2)))
  (define content-x (+ x 1))
  (define visible (max 1 (- h 5)))
  (set! *tasks-visible* visible)
  (define bg (theme-scope-ref "ui.background"))
  (define text (theme-scope-ref "ui.text"))
  (define sel (theme-scope-ref "ui.menu.selected"))
  (define dim (style-with-dim text))
  (define bold (style-with-bold text))
  (define sel-bold (style-with-bold sel))
  (define box (area x y w h))

  (buffer/clear-with frame box bg)
  (block/render frame box (make-block bg text "all" "rounded"))

  (define counter (string-append (number->string (length *tasks-filtered*))
                                 "/"
                                 (number->string (length *tasks-all*))))
  (define counter-w (string-length counter))
  (define prompt "› ")
  (define query-w (max 1 (- inner counter-w (string-length prompt) 1)))
  (frame-set-string! frame content-x (+ y 1)
                     (string-append prompt (tasks-truncate *tasks-query* query-w))
                     bold)
  (frame-set-string! frame (max content-x (- (+ x w) 1 counter-w)) (+ y 1) counter dim)
  (frame-set-string! frame content-x (+ y 2) (tasks-repeat-ch inner #\─) dim)

  (define name-w (max 14 (min 32 (quotient (* inner 2) 5))))
  (define items-start (+ y 3))

  (if (null? *tasks-filtered*)
      (frame-set-string! frame content-x items-start
                         (tasks-truncate (if *tasks-error*
                                             *tasks-error*
                                             (if (equal? *tasks-query* "") "no tasks" "no matches"))
                                         (max 1 (- inner 2)))
                         dim)
      (let loop ([row 0])
        (when (< row visible)
          (define idx (+ *tasks-window* row))
          (when (< idx (length *tasks-filtered*))
            (define task (list-ref *tasks-filtered* idx))
            (define selected? (= idx *tasks-cursor*))
            (define row-y (+ items-start row))
            (define row-style (if selected? sel text))
            (define name-style (if selected? sel-bold bold))
            (define running? (and *task-running?* (equal? (tasks-task-name task) *task-name*)))
            (when selected?
              (frame-set-string! frame content-x row-y (tasks-repeat-ch inner #\space) sel))
            (frame-set-string! frame content-x row-y (if selected? "❯ " "  ") row-style)
            (frame-set-string! frame (+ content-x 2) row-y (if running? "● " "  ")
                               (if running? bold row-style))
            (frame-set-string! frame (+ content-x 4) row-y
                               (tasks-truncate (tasks-task-name task) (max 1 (- name-w 4)))
                               name-style)
            (define desc (tasks-task-description task))
            (define detail (if (and (string? desc) (not (equal? desc "")))
                               desc
                               (tasks-joined-cmd task)))
            (frame-set-string! frame (+ content-x name-w) row-y
                               (tasks-truncate detail (max 0 (- inner name-w)))
                               dim))
          (loop (+ row 1)))))

  (define footer-y (+ y (- h 2)))
  (define hints (if *task-running?*
                    (string-append "● " (if (string? *task-name*) *task-name* "")
                                   " running · Esc close")
                    "↑/↓ move · Enter run · Esc close"))
  (frame-set-string! frame content-x footer-y
                     (tasks-truncate hints (max 1 (- inner 2))) dim))

(define (tasks-list-cursor _state rect)
  (define metrics (tasks-list-metrics rect))
  (define x (list-ref metrics 0))
  (define y (list-ref metrics 1))
  (position (+ y 1) (+ x 3 (string-length *tasks-query*))))

(define (tasks-mod-bits event)
  (define m (key-event-modifier event))
  (if (number? m) m 0))

(define (tasks-has-mod? event bit)
  (not (= 0 (bitwise-and (tasks-mod-bits event) bit))))

(define (tasks-plain-char? event ch)
  (and (char? ch)
       (not (tasks-has-mod? event key-modifier-ctrl))
       (not (tasks-has-mod? event key-modifier-alt))
       (not (tasks-has-mod? event key-modifier-super))))

(define (tasks-list-handle-event _state event)
  (define ch (key-event-char event))
  (define ctrl? (tasks-has-mod? event key-modifier-ctrl))
  (cond
    [(key-event-escape? event)
     (set! *tasks-active?* #f)
     event-result/close]
    [(key-event-enter? event) (tasks-activate!)]
    [(or (key-event-down? event) (and ctrl? (char? ch) (char=? ch #\n)))
     (tasks-move! 1)
     event-result/consume]
    [(or (key-event-up? event) (and ctrl? (char? ch) (char=? ch #\p)))
     (tasks-move! -1)
     event-result/consume]
    [(key-event-page-down? event) (tasks-move! *tasks-visible*) event-result/consume]
    [(key-event-page-up? event) (tasks-move! (- *tasks-visible*)) event-result/consume]
    [(and ctrl? (char? ch) (char=? ch #\u))
     (set! *tasks-query* "")
     (tasks-refilter!)
     event-result/consume]
    [(key-event-backspace? event) (tasks-backspace!) event-result/consume]
    [(paste-event? event) (tasks-paste! (paste-event-string event)) event-result/consume]
    [(tasks-plain-char? event ch) (tasks-type! ch) event-result/consume]
    [else event-result/consume]))

(define (tasks-paste! s)
  (set! *tasks-query* (string-append *tasks-query* s))
  (tasks-refilter!)
  (set! *tasks-cursor* 0)
  (set! *tasks-window* 0))

(define (tasks-list-component)
  (new-component! "tasks-list"
                  (TasksListState)
                  tasks-list-render
                  (hash "handle_event" tasks-list-handle-event
                        "cursor" tasks-list-cursor)))

;; ---------------------------------------------------------------------------
;; output component
;; ---------------------------------------------------------------------------

(struct TasksOutState ())

(define (tasks-out-metrics rect)
  (define tw (area-width rect))
  (define th (area-height rect))
  (define w (min (- tw 2) (max 30 (min (- tw 4) 110))))
  (define h (min (- th 2) (max 9 (min (- th 4) (quotient (* th 4) 5)))))
  (list (max 0 (quotient (- tw w) 2))
        (max 0 (quotient (- th h) 2))
        w h))

(define (tasks-lines-label n)
  (if (= n 1) "1 line" (string-append (number->string n) " lines")))

(define (tasks-exit-label code)
  (cond
    [(number? code) (string-append "exit " (number->string code))]
    [(and (list? code) (equal? (car code) 'sig))
     (string-append "signal " (number->string (list-ref code 1)))]
    [else "stopped"]))

(define (tasks-status-text)
  (define elapsed (if *task-running?*
                      (- (current-milliseconds) *task-started*)
                      (- *task-finished* *task-started*)))
  (define lines (tasks-lines-label *task-line-count*))
  (define tail (string-append (tasks-ms->s elapsed) " · " lines))
  (cond
    [(not *task-name*) "no task has run yet"]
    [*task-running?*
     (if *task-killing?*
         (string-append "killing… · " tail)
         (string-append "running " tail))]
    [*task-killed?*
     (string-append "killed · " tail)]
    [else (string-append (tasks-exit-label *task-exit*) " · " tail)]))

(define (tasks-out-render _state rect frame)
  (define metrics (tasks-out-metrics rect))
  (define x (list-ref metrics 0))
  (define y (list-ref metrics 1))
  (define w (list-ref metrics 2))
  (define h (list-ref metrics 3))
  (define inner (max 1 (- w 2)))
  (define content-x (+ x 1))
  (define visible (max 1 (- h 5)))
  (set! *task-visible* visible)
  (define bg (theme-scope-ref "ui.background"))
  (define text (theme-scope-ref "ui.text"))
  (define dim (style-with-dim text))
  (define bold (style-with-bold text))
  (define box (area x y w h))

  (buffer/clear-with frame box bg)
  (block/render frame box (make-block bg text "all" "rounded"))

  (define all (reverse *task-lines*))
  (define total (length all))
  (when *task-follow?*
    (set! *task-scroll* (max 0 (- total visible))))
  (define top (max 0 (min *task-scroll* (max 0 (- total visible)))))
  (define win (tasks-take (tasks-drop all top) visible))

  (define title (if (string? *task-name*) *task-name* "task"))
  (define title-text (string-append "▶ " title))
  (frame-set-string! frame content-x (+ y 1)
                     (tasks-truncate title-text (max 1 (- inner 2)))
                     bold)
  (define cmd-space (max 0 (- inner (string-length title-text) 4)))
  (when (> cmd-space 4)
    (define shown (tasks-truncate *task-cmd* cmd-space))
    (frame-set-string! frame (max content-x (- (+ x w) 1 (string-length shown)))
                       (+ y 1) shown dim))
  (frame-set-string! frame content-x (+ y 2) (tasks-repeat-ch inner #\─) dim)

  (let loop ([row 0])
    (when (< row visible)
      (when (< row (length win))
        (define line (list-ref win row))
        (define err? (car line))
        (define body (cdr line))
        (frame-set-string! frame content-x (+ y 3 row)
                           (tasks-truncate (tasks-chomp body) (max 1 (- inner 2)))
                           (if err? dim text)))
      (loop (+ row 1))))

  (define status (tasks-status-text))
  (frame-set-string! frame content-x (+ y (- h 2))
                     (tasks-truncate status (max 1 (- inner 2))) dim)
  (define hints (if *task-running?* "ctrl-k kill · Esc close" "r rerun · Esc close"))
  (frame-set-string! frame (max content-x (- (+ x w) 1 (string-length hints)))
                     (+ y (- h 2)) hints dim))

(define (tasks-scroll! delta)
  (set! *task-follow?* #f)
  (define total (length *task-lines*))
  (set! *task-scroll*
        (max 0 (min (max 0 (- total *task-visible*)) (+ *task-scroll* delta)))))

(define (tasks-out-handle-event _state event)
  (define ch (key-event-char event))
  (define ctrl? (tasks-has-mod? event key-modifier-ctrl))
  (cond
    [(key-event-escape? event)
     (set! *task-out-open?* #f)
     event-result/close]
    [(and ctrl? (char? ch) (char=? ch #\k))
     (when *task-running?* (tasks-kill!))
     event-result/consume]
    [(and (char? ch) (char=? ch #\r) (not ctrl?) (not *task-running?*))
     (tasks-rerun!)
     event-result/consume]
    [(key-event-down? event) (tasks-scroll! 1) event-result/consume]
    [(key-event-up? event) (tasks-scroll! -1) event-result/consume]
    [(key-event-page-down? event) (tasks-scroll! *task-visible*) event-result/consume]
    [(key-event-page-up? event) (tasks-scroll! (- *task-visible*)) event-result/consume]
    [(key-event-home? event) (set! *task-scroll* 0) (set! *task-follow?* #f) event-result/consume]
    [(key-event-end? event) (set! *task-follow?* #t) event-result/consume]
    [else event-result/consume]))

(define (tasks-out-component)
  (new-component! "tasks-output"
                  (TasksOutState)
                  tasks-out-render
                  (hash "handle_event" tasks-out-handle-event)))

;; ---------------------------------------------------------------------------
;; running tasks
;; ---------------------------------------------------------------------------

(define (tasks-strip-ansi s)
  (let loop ([cs (string->list s)] [state 'text] [acc '()])
    (cond
      [(null? cs) (list->string (reverse acc))]
      [(equal? state 'text)
       (cond
         [(char=? (car cs) (integer->char 27)) (loop (cdr cs) 'esc acc)]
         [(char=? (car cs) #\return) (loop (cdr cs) 'text acc)]
         [(< (char->integer (car cs)) 32)
          (if (char=? (car cs) #\tab)
              (loop (cdr cs) 'text (cons #\space (cons #\space (cons #\space (cons #\space acc)))))
              (loop (cdr cs) 'text acc))]
         [else (loop (cdr cs) 'text (cons (car cs) acc))])]
      [(equal? state 'esc)
       (cond
         [(char=? (car cs) #\[) (loop (cdr cs) 'csi acc)]
         [(char=? (car cs) #\]) (loop (cdr cs) 'osc acc)]
         [else (loop (cdr cs) 'text acc)])]
      [(equal? state 'csi)
       (if (and (>= (char->integer (car cs)) 64) (<= (char->integer (car cs)) 126))
           (loop (cdr cs) 'text acc)
           (loop (cdr cs) 'csi acc))]
      [(equal? state 'osc)
       (cond
         [(= (char->integer (car cs)) 7) (loop (cdr cs) 'text acc)]
         [(char=? (car cs) (integer->char 27)) (loop (cdr cs) 'osc-esc acc)]
         [else (loop (cdr cs) 'osc acc)])]
      [(equal? state 'osc-esc)
       (loop (cdr cs) (if (char=? (car cs) #\\) 'text 'osc) acc)]
      [else (loop (cdr cs) 'text acc)])))

(define (tasks-push-line! err? text)
  (set! *task-lines* (cons (cons err? (tasks-strip-ansi text)) *task-lines*))
  (set! *task-line-count* (+ *task-line-count* 1))
  (set! *task-replace-pending?* #f)
  (when (> *task-line-count* *TASKS-MAX-LINES*)
    (set! *task-lines* (tasks-take *task-lines* *TASKS-MAX-LINES*))
    (set! *task-line-count* *TASKS-MAX-LINES*)))

;; Progress bars redraw the same line with a bare \r; hx-task reports those
;; as `R` events, so replace the last line instead of appending a new one.
(define (tasks-replace-line! err? text)
  (define entry (cons err? (tasks-strip-ansi text)))
  (if (and *task-replace-pending?* (not (null? *task-lines*)))
      (set! *task-lines* (cons entry (cdr *task-lines*)))
      (begin
        (set! *task-lines* (cons entry *task-lines*))
        (set! *task-line-count* (+ *task-line-count* 1))))
  (set! *task-replace-pending?* #t))

(define (tasks-parse-exit-code raw)
  (cond
    [(tasks-starts-with? raw "sig:")
     (define n (with-handler (lambda (_) #f)
                 (string->number (substring raw 4 (string-length raw)))))
     (if (number? n) (list 'sig n) #f)]
    [else (with-handler (lambda (_) #f) (string->number raw))]))

;; hx-task speaks a line protocol on its stdout:
;;   L <text>  a complete line, R <text> replaces the previous one,
;;   X <code>  the child is gone ("sig:N" for signal deaths).
(define (tasks-reader-loop port sender run-id)
  (let loop ()
    (define line (with-handler (lambda (_) 'eof) (read-line-from-port port)))
    (cond
      [(not (string? line)) (channel/send sender (list 'eof run-id))]
      [(and (>= (string-length line) 2)
            (char=? (string-ref line 0) #\L)
            (char=? (string-ref line 1) #\space))
       (channel/send sender (list 'line run-id (tasks-chomp (substring line 2 (string-length line)))))
       (loop)]
      [(and (>= (string-length line) 2)
            (char=? (string-ref line 0) #\R)
            (char=? (string-ref line 1) #\space))
       (channel/send sender (list 'replace run-id (tasks-chomp (substring line 2 (string-length line)))))
       (loop)]
      [(and (>= (string-length line) 2)
            (char=? (string-ref line 0) #\X)
            (char=? (string-ref line 1) #\space))
       (channel/send sender
                     (list 'exit run-id
                           (tasks-parse-exit-code (tasks-chomp (substring line 2 (string-length line))))))
       (loop)]
      [else (channel/send sender (list 'line run-id line)) (loop)])))

(define (tasks-err-loop port sender run-id)
  (let loop ()
    (define line (with-handler (lambda (_) 'eof) (read-line-from-port port)))
    (cond
      [(string? line)
       (channel/send sender (list 'line run-id (string-append "hx-task: " line)))
       (loop)]
      [else #f])))

(define (tasks-finish! code)
  (when *task-running?*
    (set! *task-running?* #f)
    (set! *task-killing?* #f)
    (set! *task-kill-deadline* #f)
    (set! *task-exit* code)
    (set! *task-finished* (current-milliseconds))
    (set-status!
     (string-append "tasks: " (if (string? *task-name*) *task-name* "task")
                    (cond
                      [*task-killed?*
                       (string-append " killed (" (tasks-exit-label code) ")")]
                      [(not code) " stopped"]
                      [else (string-append " finished (" (tasks-exit-label code) ")")])))))

(define (tasks-drain!)
  (define ch *task-channel*)
  (when ch
    (let loop ()
      (define v (channel/try-recv (channels-receiver ch)))
      (cond
        [(empty-channel-object? v) #f]
        [(disconnected-channel-object? v) #f]
        [(not (list? v)) (loop)]
        [(not (equal? (list-ref v 1) *task-run-id*)) (loop)]
        [(equal? (car v) 'line) (tasks-push-line! #f (list-ref v 2)) (loop)]
        [(equal? (car v) 'replace) (tasks-replace-line! #f (list-ref v 2)) (loop)]
        [(equal? (car v) 'exit) (tasks-finish! (list-ref v 2)) (loop)]
        [(equal? (car v) 'eof) (tasks-finish! #f) (loop)]
        [else (loop)]))))

(define (tasks-schedule-tick!)
  (unless *tasks-tick-scheduled*
    (set! *tasks-tick-scheduled* #t)
    (enqueue-thread-local-callback-with-delay
     *TASKS-TICK-MS*
     (lambda ()
       (set! *tasks-tick-scheduled* #f)
       (tasks-drain!)
       ;; hx-task escalates TERM -> KILL by itself; this is only a last resort
       ;; should the helper itself wedge.
       (when (and *task-running?* *task-killing?* *task-kill-deadline*
                  (> (current-milliseconds) *task-kill-deadline*))
         (with-handler (lambda (_) #f) (subprocess-kill *task-child*))
         (tasks-finish! (list 'sig 9)))
       (when *task-running?* (tasks-schedule-tick!))))))

(define (tasks-send-kill! byte)
  (and *task-stdin*
       (with-handler
         (lambda (_) #f)
         (begin (write-byte byte *task-stdin*)
                (flush-output-port *task-stdin*)
                #t))))

(define (tasks-kill!)
  (when (and *task-running?* (not *task-killing?*))
    (set! *task-killing?* #t)
    (set! *task-killed?* #t)
    (set! *task-kill-deadline* (+ (current-milliseconds) 2500))
    (unless (tasks-send-kill! 116) ; 't': SIGTERM the group, KILL after 400ms
      (with-handler (lambda (_) #f) (subprocess-kill *task-child*))
      (tasks-finish! #f))))

(define (tasks-stop-previous!)
  (when *task-stdin*
    (with-handler (lambda (_) #f) (close-output-port *task-stdin*)))
  (set! *task-stdin* #f)
  (set! *task-child* #f)
  (set! *task-running?* #f)
  (set! *task-killing?* #f)
  (set! *task-kill-deadline* #f))

(define (tasks-channel)
  (unless *task-channel* (set! *task-channel* (channels/new)))
  *task-channel*)

(define (tasks-run! task)
  (tasks-stop-previous!)
  (set! *task-current* task)
  (set! *task-name* (tasks-task-name task))
  (set! *task-cmd* (tasks-joined-cmd task))
  (set! *task-lines* '())
  (set! *task-line-count* 0)
  (set! *task-scroll* 0)
  (set! *task-follow?* #t)
  (set! *task-exit* #f)
  (set! *task-killed?* #f)
  (set! *task-killing?* #f)
  (set! *task-kill-deadline* #f)
  (set! *task-replace-pending?* #f)
  (set! *task-started* (current-milliseconds))
  (set! *task-finished* *task-started*)
  (define helper (tasks-helper-path))
  (cond
    [(not (tasks-file-exists? helper))
     (set! *task-running?* #f)
     (tasks-push-line! #t (string-append "hx-task helper not found: " helper))
     (tasks-push-line! #t "build it with: make -C ~/.config/helix/tasks")
     (set-error! "tasks: helper is not built")]
    [else
     (define builder
       (~> (command helper (list "--cols" "120" "--rows" "40" "--" "sh" "-c" *task-cmd*))
           with-stdout-piped
           with-stderr-piped
           with-stdin-piped))
     (define cwd (tasks-cwd-for task))
     (define builder2
       (if (and (string? cwd) (> (string-length cwd) 0))
           (with-handler (lambda (_) builder) (with-current-dir builder cwd))
           builder))
     (define res (with-handler (lambda (_) #f) (spawn-process builder2)))
     (cond
       [(and res (Ok? res))
        (define child (Ok->value res))
        (define ch (tasks-channel))
        (set! *task-child* child)
        (set! *task-run-id* (+ *task-run-id* 1))
        (define run-id *task-run-id*)
        (define helper-stdin (with-handler (lambda (_) #f) (child-stdin child)))
        (set! *task-stdin* helper-stdin)
        (set! *task-running?* #t)
        (define sender (channels-sender ch))
        (define helper-stdout (child-stdout child))
        (define helper-stderr (child-stderr child))
        (spawn-native-thread
         (lambda () (tasks-reader-loop helper-stdout sender run-id)))
        (spawn-native-thread
         (lambda () (tasks-err-loop helper-stderr sender run-id)))
        (tasks-schedule-tick!)]
       [else
        (set! *task-running?* #f)
        (tasks-push-line! #t (string-append "failed to spawn: " helper))
        (set-error! "tasks: failed to spawn the helper")])]))

(define (tasks-rerun!)
  (when *task-current* (tasks-run! *task-current*)))

;; ---------------------------------------------------------------------------
;; opening / closing
;; ---------------------------------------------------------------------------

(define (tasks-open-list!)
  (tasks-load!)
  (set! *tasks-active?* #t)
  (push-component! (tasks-list-component)))

(define (tasks-close-list!)
  (set! *tasks-active?* #f)
  (with-handler (lambda (_) #f) (pop-last-component-by-name! "tasks-list")))

(define (tasks-open-output!)
  (if (not (string? *task-name*))
      (set-status! "tasks: no task has run yet")
      (begin
        (set! *task-out-open?* #t)
        (push-component! (tasks-out-component)))))

(define (tasks-close-output!)
  (set! *task-out-open?* #f)
  (with-handler (lambda (_) #f) (pop-last-component-by-name! "tasks-output")))

(define (tasks-activate!)
  (define idx *tasks-cursor*)
  (if (or (< idx 0) (>= idx (length *tasks-filtered*)))
      event-result/consume
      (let ([task (list-ref *tasks-filtered* idx)])
        (set! *tasks-active?* #f)
        (if (and *task-running?* (equal? (tasks-task-name task) *task-name*))
            (enqueue-thread-local-callback (lambda () (tasks-open-output!)))
            (begin
              (tasks-run! task)
              (enqueue-thread-local-callback (lambda () (tasks-open-output!)))))
        event-result/close)))

;; ---------------------------------------------------------------------------
;; commands
;; ---------------------------------------------------------------------------

;;@doc
;; Toggle the task picker: a centered list of the tasks from .helix/tasks.toml
;; with fuzzy filtering while you type.
(define (tasks-toggle)
  (cond
    [*tasks-active?* (tasks-close-list!)]
    [*task-out-open?* (tasks-close-output!)]
    [else (tasks-open-list!)]))

;;@doc
;; Toggle the output panel of the currently running (or last) task.
(define (tasks-output)
  (if *task-out-open?* (tasks-close-output!) (tasks-open-output!)))

;;@doc
;; Reload the task file and report how many tasks were found.
(define (tasks-reload)
  (tasks-load!)
  (cond
    [*tasks-error* (set-error! (string-append "tasks: " *tasks-error*))]
    [else
     (set-status! (string-append "tasks: loaded "
                                 (number->string (length *tasks-all*))
                                 " task(s) from " *tasks-file*))]))
