\version "2.19.27"

%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNCTIONS TO INCLUDE %%%%%%%%%%%%%%%%%%%%%%%%

%% CUSTOM GROB PROPERTIES

% Taken from http://www.mail-archive.com/lilypond-user%40gnu.org/msg97663.html
% (Paul Morris)

% function from "scm/define-grob-properties.scm" (modified)
#(define (cn-define-grob-property symbol type?)
   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc "custom grob property")
   symbol)

% For internal use.
#(cn-define-grob-property 'text-spanner-stencils list?)

% user interface
#(cn-define-grob-property 'text-spanner-line-count number-list?)

% How much space between line and object to left and right?
% Default is '(0.0 . 0.0).
#(cn-define-grob-property 'line-X-offset number-pair?)

% Vertical shift of connector line, independenf of texts.
#(cn-define-grob-property 'line-Y-offset number?)

#(define (get-text-distribution text-list line-extents)
   ;; Given a list of texts and a list of line extents, attempt to
   ;; find a decent line distribution.  The goal is to put more texts
   ;; on longer lines, while ensuring that first and last lines are texted.
   ;; TODO: ideally, we should consider extents of text, rather than
   ;; simply their number.
   (let* ((line-count (length line-extents))
          (text-count (length text-list))
          (line-lengths
           (map (lambda (line) (interval-length line))
             line-extents))
          (total-line-len (apply + line-lengths))
          (exact-per-line
           (map (lambda (line-len)
                  (* text-count (/ line-len total-line-len)))
             line-lengths))
          ;; First and last lines can't be untexted.
          (adjusted
           (let loop ((epl exact-per-line) (idx 0) (result '()))
             (if (null? epl)
                 (reverse! result)
                 (if (and (or (= idx 0)
                              (= idx (1- line-count)))
                          (< (car epl) 1.0))
                     (loop (cdr epl) (1+ idx)
                       (cons 1.0 result))
                     (loop (cdr epl) (1+ idx)
                       (cons (car epl) result)))))))
     ;; The idea is to raise the "most roundable" line's count, then the
     ;; "next most roundable," and so forth, until we account for all texts.
     ;; Everything else is rounded down (except those lines which need to be
     ;; bumped up to get the minimum of one text), so we shouldn't exceed our
     ;; total number of texts.
     ;; TODO: Need a promote-demote-until-flush to be safe, unless this is
     ;; mathematically sound!
     (define (promote-until-flush result)
       (let* ((floored (map floor result))
              (total (apply + floored)))

         (if (>= total text-count)
             (begin
              ;(format #t "guess: ~a~%~%~%" result)
              floored)
             (let* ((decimal-amount
                     (map (lambda (x) (- x (floor x))) result))
                    (maximum (apply max decimal-amount))
                    (max-location
                     (list-index
                      (lambda (x) (= x maximum))
                      decimal-amount))
                    (item-to-bump (list-ref result max-location)))
               ;(format #t "guess: ~a~%" result)
               (list-set! result max-location (ceiling item-to-bump))
               (promote-until-flush result)))))

     (let ((result (map inexact->exact
                     (promote-until-flush adjusted))))
       (if (not (= (apply + result) text-count))
           ;; If this doesn't work, discard, triggering crude
           ;; distribution elsewhere.
           '()
           result))))

#(define (get-broken-connectors grob text-distribution connectors)
   "Modify @var{connectors} to reflect line breaks.  Return a list
of lists of booleans representing whether to draw a connecting line
between successive texts."
   ;; The variable 'connectors' holds a list of booleans representing whether
   ;; a line will be drawn between two successive texts.  This function
   ;; transforms the list of booleans into a list of lists of booleans
   ;; which reflects line breaks and the additional lines which must be drawn.
   ;;
   ;; Given an input of '(#t #t #f)
   ;;
   ;;    '((#t        #t            #f))
   ;; one_ _ _ _two_ _ _ _ _three        four  (one line)
   ;;
   ;;     '((#t       #t)
   ;; one_ _ _ _two_ _ _ _ _                   (two lines)
   ;;   (#t         #f))
   ;; _ _ _ _three     four
   ;;
   ;;     '((#t)
   ;; one_ _ _ _                               (four lines/blank)
   ;; (#t       #t)
   ;; _ _ _two_ _ _
   ;;      (#t)
   ;; _ _ _ _ _ _ _
   ;; (#t      #f))
   ;; _ _three    four
   (let ((text-distribution (vector->list text-distribution)))
     (if (pair? connectors)
         (let loop ((td text-distribution)
                    (joins connectors)
                    (result '()))
           (if (null? td)
               (reverse! result)
               (let inner ((texts (car td))
                           (bools joins)
                           (inner-result '()))
                 (cond
                  ((null? (cdr texts))
                   (loop (cdr td) bools
                     (cons (reverse! inner-result) result)))
                  ;; Ignore spacers since they don't represent a new line.
                  ((equal? "" (cadr texts))
                   (inner (cdr texts) bools inner-result))
                  ((equal? (cadr texts) #{ \markup \null #})
                   (inner (cdr texts) bools
                     (cons (car bools) inner-result)))
                  (else
                   (inner (cdr texts) (cdr bools)
                     (cons (car bools) inner-result)))))))

         connectors)))

#(define (get-line-arrangement grob-or-siblings extents texts)
   "Given a list of spanner extents and texts, return a vector of lists
of the texts to be used for each line."
   (let ((gs-len (length grob-or-siblings)))
     (if (= gs-len 1)
         ;; only one line...
         (make-vector 1 texts)
         (let* ((texts-len (length texts))
                (text-counts
                 (ly:grob-property
                  (car grob-or-siblings) 'text-spanner-line-count))
                (text-counts
                 (cond
                  ((pair? text-counts) text-counts) ; manual override
                  ((= 1 gs-len) '())
                  (else (get-text-distribution texts extents))))
                (text-counts
                 (if (and (pair? text-counts)
                          (not (= (apply + text-counts) texts-len)))
                     (begin
                      (ly:warning "Count doesn't match number of texts.")
                      '())
                     text-counts))
                (text-lines (make-vector gs-len 0))
                ;; If user hasn't specified a count elsewhere, or
                ;; 'get-text-distribution' failed, we have this method.
                ;; Populate vector in a simple way: with two lines,
                ;; give one text to the first line, one to the second,
                ;; a second for the first, and second for the second--
                ;; and so forth, until all texts have been exhausted.  So
                ;; for 3 lines and 7 texts we would get this arrangement:
                ;; 3, 2, 2.
                (text-counts
                 (cond
                  ((null? text-counts)
                   (let loop ((txts texts) (idx 0))
                     (cond
                      ((null? txts) text-lines)
                      ;; We need to ensure that the last line has text.
                      ;; This may require skipping lines.
                      ((and (null? (cdr txts))
                            (< idx (1- gs-len))
                            (= 0 (vector-ref text-lines (1- gs-len))))
                       (vector-set! text-lines (1- gs-len) 1)
                       text-lines)
                      (else
                       (vector-set! text-lines idx
                         (1+ (vector-ref text-lines idx)))
                       (loop (cdr txts)
                         (if (= idx (1- gs-len)) 0 (1+ idx)))))))
                  (else (set! text-lines (list->vector text-counts)))))
                ;; read texts into vector
                (texts-by-line
                 (let loop ((idx 0) (texts texts))
                   (if (= idx gs-len)
                       text-lines
                       (let ((num (vector-ref text-lines idx)))
                         (vector-set! text-lines idx
                           (list-head texts num))
                         (loop (1+ idx)
                           (list-tail texts num)))))))

           text-lines))))

#(define (add-markers text-lines)
   ;; Markers are added to the broken edges of spanners to serve as anchors
   ;; for connector lines beginning and ending systems.
   ;; Add null-markup at the beginning of lines 2...n.
   ;; Add null-markup at the end of lines 1...(n-1).
   ;; Note: this modifies the vector 'text-lines'.
   (let loop ((idx 0))
     (if (= idx (vector-length text-lines))
         text-lines
         (begin
          (if (> idx 0)
              (vector-set! text-lines idx
                (cons #{ \markup \null #}
                  (vector-ref text-lines idx))))
          (if (< idx (1- (vector-length text-lines)))
              (vector-set! text-lines idx
                (append (vector-ref text-lines idx)
                  (list #{ \markup \null #}))))
          (loop (1+ idx))))))

%% Adapted from 'justify-line-helper' in scm/define-markup-commands.scm.
#(define (markup-list->stencils-and-extents-for-line grob texts extent)
   "Given a list of markups @var{texts}, return a list of stencils and extents
spread along an extent @var{extent}, such that the intervening spaces are
equal."
   (let* ((orig-stencils
           (map (lambda (a) (grob-interpret-markup grob a)) texts))
          (line-contents
           (map (lambda (stc)
                  (if (ly:stencil-empty? stc X)
                      (ly:make-stencil (ly:stencil-expr stc)
                        '(0 . 0) (ly:stencil-extent stc Y))
                      stc))
             orig-stencils))
          (line-width (interval-length extent))
          (text-extents
           (map (lambda (stc) (ly:stencil-extent stc X))
             line-contents))
          (text-lengths
           (map (lambda (te) (interval-length te)) text-extents))
          (total-text-length
           (apply + (map (lambda (te) (interval-length te))
                      text-extents)))
          (total-fill-space (- line-width total-text-length))
          (word-count (length line-contents))
          (padding (/ (- line-width total-text-length) (1- word-count)))
          ;; How much shift is necessary to align left edge of first
          ;; stencil with extent?  Apply this shift to all stencils.
          (text-extents
           (map (lambda (stc)
                  (coord-translate
                   stc
                   (- (car extent) (caar text-extents))))
             text-extents))
          ;; Make a list of stencils and their extents, such that they
          ;; are spread across the line with equal space ('padding') in
          ;; between.
          (stencils-shifted-extents-list
           (let loop ((contents line-contents) (exts text-extents)
                       (lengths text-lengths)
                       (shift 0.0) (result '()))
             (if (null? contents)
                 (reverse! result)
                 (loop
                  (cdr contents) (cdr exts) (cdr lengths)
                  (+ shift (car lengths) padding)
                  (cons
                   (cons
                    (car contents)
                    (coord-translate (car exts) shift))
                   result)))))
          ;; Remove non-marker spacers from list of extents.  This is done
          ;; so that a single line is drawn to cover the total gap rather
          ;; than several. (A single line is needed since successive dashed
          ;; lines will not connect properly.)
          (stencils-extents-list-no-spacers
           (let loop ((orig stencils-shifted-extents-list) (idx 0) (result '()))
             (cond
              ((= idx (length stencils-shifted-extents-list))
               (reverse! result))
              ;; Ignore first and last stencils, which--if point stencil--
              ;; will be markers.
              ((or (= idx 0)
                   (= idx (1- (length stencils-shifted-extents-list))))
               (loop (cdr orig) (1+ idx)
                 (cons (car orig) result)))
              ;; Remove spacers.  Better way to identify them than comparing
              ;; left and right extents?
              ((= (cadar orig) (cddar orig))
               (loop (cdr orig) (1+ idx) result))
              ;; Keep any visible stencil.
              (else (loop (cdr orig) (1+ idx)
                      (cons (car orig) result)))))))

     stencils-extents-list-no-spacers))

#(define (check-for-overlaps stil-extent-list)
   (let* ((collision
           (lambda (line)
             (let loop ((exts (map cdr line)) (result '()))
               (if (null? (cdr exts))
                   (reverse! result)
                   (loop (cdr exts)
                     (cons
                      (not (interval-empty?
                            (interval-intersection
                             (car exts) (cadr exts))))
                      result))))))
          ;; ==> list of lists of booleans comparing first element to second,
          ;; second to third, etc., for each line.  #f = no collision
          (all-successive-collisions
           (map (lambda (line) (collision line))
             stil-extent-list)))

     ;; For now, just print a warning and return #t if any collision anywhere.
     ;; Returned boolean is not used elsewhere, but keep it in case.
     (let loop ((lines all-successive-collisions) (idx 0) (collisions? #f))
       (cond
        ((null? lines) collisions?)
        ((any (lambda (p) (eq? p #t)) (car lines))
         (ly:warning
          "overlap(s) found on line ~a; redistribute manually"
          (1+ idx))
         (loop (cdr lines) (1+ idx) #t))
        (else
         (loop (cdr lines) (1+ idx) collisions?))))))


#(define (make-distributed-line-stencil grob stil-stil-extent-list connectors)
   "Take a list of stencils and arbitrary extents and return a combined
stencil conforming to the given extents.  Lines are drawn/not drawn between
stencils if specified by @code{#t} or @code{#f} in @var{connectors}."
   (let* (;; First create a stencil consisting of text items.
           (line-contents
            (map (lambda (elem)
                   (ly:stencil-translate-axis
                    (car elem)
                    (- (cadr elem) (car (ly:stencil-extent (car elem) X)))
                    X))
              stil-stil-extent-list))
           (line-contents (apply ly:stencil-add line-contents))
           ;; Now make the connectors.
           ;; To handle overrides of line starts and ends, we modify
           ;; the extents from 'stil-stil-extent-list.
           (extents (map cdr stil-stil-extent-list))
           (padding (ly:grob-property grob 'line-X-offset (cons 0.0 0.0)))
           (padding-L (car padding))
           (padding-R (cdr padding))
           ;; Offsets to line endpoints depend on what the line connects.
           (padded-extents-list
            (let loop ((orig extents) (idx 0) (result '()))
              (cond
               ((= idx (length extents))
                (reverse! result))
               ;; Don't widen line markers.  Recognition is based on extent,
               ;; which is not ideal.
               ((= (caar orig) (cdar orig))
                (loop (cdr orig) (1+ idx)
                  (cons (car orig) result)))
               ;; A connector drawn to an object beginning a line will only
               ;; be padded on the right.
               ((= idx 0)
                (loop (cdr orig) (1+ idx)
                  (cons
                   (coord-translate
                    (car orig) (cons 0 padding-R))
                   result)))
               ;; A connector drawn from the last object on a line will only
               ;; be padded on the left.
               ((= idx (1- (length extents)))
                (loop (cdr orig) (1+ idx)
                  (cons
                   (coord-translate
                    (car orig) (cons (- padding-L) 0.0))
                   result)))
               ;; Lines joining objects on both sides are padded at both ends.
               (else
                (loop (cdr orig) (1+ idx)
                  (cons
                   (coord-translate
                    (car orig)
                    (cons (- padding-L)
                      padding-R))
                   result))))))
           ;; Read connector extents from modified list of extents.
           ;; ((1-L . 1-R) (2-L . 2-R) (3-L . 3-R)) ;; padded extents of texts
           ;; ==> ((1-R . 2-L) (2-R . 3-L)) ;; extents of connector lines
           (spaces
            (if (> (length padded-extents-list) 1)
                (let loop ((orig padded-extents-list)
                           (result '()))
                  (if (null? (cdr orig))
                      (reverse! result)
                      (loop
                       (cdr orig)
                       (cons
                        (cons
                         (cdr (first orig))
                         (car (second orig)))
                        result))))
                '()))
           ;; By default, lines are drawn between all texts
           (join-all (or (null? connectors)
                         (eq? #t connectors)))
           (offset-Y (ly:grob-property grob 'line-Y-offset 0.0))
           (connector-stils
            (append-map
             (lambda (sps joins)
               (if (and
                    ;; space too short for line
                    (not (interval-empty? sps))
                    (or join-all joins))
                   (list (ly:line-interface::line grob
                           (car sps) offset-Y
                           (cdr sps) offset-Y))
                   '()))
             spaces connectors))
           (connector-stil (apply ly:stencil-add connector-stils))
           (line-contents (ly:stencil-add connector-stil line-contents)))

     line-contents))

#(define (make-stencils grob-or-siblings stil-extent-list connectors)
   ;; entry point for stencil construction
   ;; connectors is a list of lists, for example:
   ;; '((#t #t)) or '((#t #t) (#t #f))
   (map (lambda (gs sel cs)
          (make-distributed-line-stencil
           gs sel cs))
     grob-or-siblings stil-extent-list connectors))

extractLyricEventInfo =
#(define-scheme-function (lst) (ly:music?)
   "Given a music expression @var{lst}, return a list of pairs.  The
@code{car} of each pair is the text of any @code{LyricEvent}, and the
@code{cdr} is a boolean representing presence or absence of a hyphen
associated with that @code{LyricEvent}."
   ;; TODO: include duration info, skips?
   (map (lambda (elt)
          (let* ((text (ly:music-property elt 'text))
                 (hyphen (extract-named-music elt 'HyphenEvent))
                 (hyphen? (pair? hyphen)))
            (cons text hyphen?)))
     (extract-named-music lst 'LyricEvent)))

%% Based on addTextSpannerText, by Thomas Morley.  See
%% http://www.mail-archive.com/lilypond-user%40gnu.org/msg81685.html
addTextSpannerText =
#(define-music-function (arg) (ly:music?)
   (let* ((texts-and-connectors (extractLyricEventInfo arg))
          (texts (map car texts-and-connectors)))
     (if (< (length texts) 2)
         (begin
          (ly:warning "At least two texts required for `addTextSpannerText'.")
          (make-music 'Music))

         #{
           % The following overrides of 'bound-details are needed to give the
           % correct length to the default spanner we replace.
           \once \override TextSpanner.bound-details.left.text = #(car texts)
           \once \override TextSpanner.bound-details.left-broken.text = ##f
           \once \override TextSpanner.bound-details.right.text = #(last texts)
           \once \override TextSpanner.bound-details.right-broken.text = ##f

           \once \override TextSpanner.stencil =
           #(lambda (grob)
              (let* (;; have we been split?
                      (orig (ly:grob-original grob))
                      ;; if yes, get the split pieces (our siblings)
                      (siblings (if (ly:grob? orig)
                                    (ly:spanner-broken-into orig)
                                    '()))
                      (grob-or-siblings
                       (if (null? siblings)
                           (list grob)
                           siblings))
                      (stils (ly:grob-property grob 'text-spanner-stencils)))
                ;; If stencils haven't been calculated, calculate them.  Once
                ;; we have results prompted by one sibling, no need to go
                ;; through elaborate calculation (stencils, collisions, ideal
                ;; line contents...) for remaining pieces.
                (if (null? stils)
                    (let* ((line-stils
                            (map (lambda (gs) (ly:line-spanner::print gs))
                              grob-or-siblings))
                           (line-extents
                            (map (lambda (s) (ly:stencil-extent s X))
                              line-stils)))

                      (define (get-stil-extent-list text-distrib)
                        (map (lambda (gs td exts)
                               (markup-list->stencils-and-extents-for-line
                                gs td exts))
                          grob-or-siblings
                          (vector->list text-distrib)
                          line-extents))

                      (let*
                       (;; vector which gives the text for unbroken spanner
                         ;; or for siblings.  This is a preliminary
                         ;; arrangement, to be tweaked below.
                         (text-distribution
                          (get-line-arrangement grob-or-siblings line-extents texts))
                         (text-distribution (add-markers text-distribution))
                         (connectors (map cdr texts-and-connectors))
                         (connectors
                          (get-broken-connectors grob text-distribution connectors))
                         (all-stils-and-extents
                          (get-stil-extent-list text-distribution))
                         ;; warning printed
                         (overlaps (check-for-overlaps all-stils-and-extents))
                         ;; convert stencil/extent list into finished stencil
                         (line-stils
                          (make-stencils
                           grob-or-siblings all-stils-and-extents connectors)))

                       (for-each
                        (lambda (gs)
                          (set!
                           (ly:grob-property gs 'text-spanner-stencils)
                           line-stils))
                        grob-or-siblings)

                       (set! stils line-stils))))

                ;; Return our stencil
                (list-ref
                 stils
                 (list-index (lambda (x) (eq? x grob)) grob-or-siblings))))
         #})))

%%%%%%%%%%%%%%%%%%%%%%%%%%% END FUNCTIONS TO INCLUDE %%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\markup \bold "Default (no inner text possible)"
%%{
\relative c'' {
  %\override TextSpanner.thickness = 5
  \override TextSpanner.bound-details.left.text = "ral"
  \override TextSpanner.bound-details.left-broken.text = ##f
  \override TextSpanner.bound-details.right.text = "do"
  \override TextSpanner.bound-details.right-broken.text = ##f
  c,1\startTextSpan
  \break
  d'1\stopTextSpan
}
\markup \bold "All on one line"

\relative c' {
  \addTextSpannerText \lyricmode { ral -- len -- tan -- do }
  c1\startTextSpan
  d'1\stopTextSpan
}
%}

\markup \bold "Broken"

\relative c' {
  %% to show collision detection
  %\override TextSpanner.text-spanner-line-count = #'(2 2)
  \addTextSpannerText \lyricmode { ral -- "" -- len -- tan -- do }
  c1\startTextSpan
  \break
  d'1\stopTextSpan
}
%%{
\markup \bold "Empty line/manual distribution"

\relative c' {
  \override TextSpanner.text-spanner-line-count = #'(1 0 1 1)
  \addTextSpannerText \lyricmode { one -- two -- three }
  c1~\startTextSpan
  \break
  c1~
  \break
  c1~
  \break
  c1\stopTextSpan
}

\markup \bold "Changes of ends"

\relative c' {
  \addTextSpannerText \lyricmode { one -- two -- three }
  c1\startTextSpan
  c1\stopTextSpan
  \once \override TextSpanner.bound-details.left.padding = #-2
  \once \override TextSpanner.bound-details.right.padding = #-5
  \addTextSpannerText \lyricmode { one -- two -- three }
  c1\startTextSpan
  c1\stopTextSpan
}

\markup \bold "Markups"

\relative c' {
  \addTextSpannerText \lyricmode {
    \markup one -- \markup two -- \markup three
  }
  c1\startTextSpan
  c1\stopTextSpan
  \addTextSpannerText \lyricmode {
    \markup one --
    \markup \with-color #red \translate #'(-3 . 0) two --
    \markup three
  }
  c1\startTextSpan
  c1\stopTextSpan
  \override TextSpanner.style = #'dotted-line
  \override TextSpanner.dash-period = #0.5
  \addTextSpannerText \lyricmode {
    \markup \right-align one --
    two --
    \markup \center-align three --
  }
  c1\startTextSpan
  c1\stopTextSpan
}

\relative c'' {
  \override TextSpanner.style = #'zigzag
  \override TextSpanner.line-X-offset = #'(0.5 . 0.5)
  \addTextSpannerText \lyricmode
  {
    \markup \draw-circle #1 #0.2 ##f --
    \markup \with-color #grey \draw-circle #1 #0.2 ##t --
    \markup \draw-circle #1 #0.2 ##t --
    \markup \with-color #grey \draw-circle #1 #0.2 ##t --
    \markup \draw-circle #1 #0.2 ##f --
  }
  c1\startTextSpan
  %\break
  d'1 d\stopTextSpan
}

\markup \bold "Showing/hiding connectors"

\relative c' {
  c1
  \override TextSpanner.padding = 3

  \override TextSpanner.text-spanner-line-count = #'(4 0 1)
  \textSpannerDown
  \addTextSpannerText \lyricmode {
    poco a poco dim. -- \markup \dynamic mf
  }
  c1\startTextSpan
  c1 c1
  \break
  c1 c1 c1 c1
  \break
  c1 c1 c1
  c1\stopTextSpan
}

\markup \bold "Raising/lowering of connector line"

\relative c' {
  \override TextSpanner.line-X-offset = #'(1 . 1)
  \override TextSpanner.line-Y-offset = 0.5
  \addTextSpannerText \lyricmode { ral -- len -- tan -- do }
  c1\startTextSpan
  d'1\stopTextSpan
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% See http://www.lilypond.org/doc/v2.19/Documentation/notation/opera-and-stage-musicals#dialogue-over-music
music = \relative {
  \override TextSpanner.text-spanner-line-count = #'(8 5)
  \addTextSpannerText \lyricmode {
    \markup \fontsize #1 \upright \smallCaps Abe:
    Say this over measures one and two
    and this over measure three
  }
  a'4\startTextSpan a a a
  a4 a a a
  \break
  a4 a a a\stopTextSpan
}

\new Staff {
  \music
}
%}
\layout {
  indent = 0
  ragged-right = ##f
}
