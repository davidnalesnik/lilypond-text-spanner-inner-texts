\version "2.19.46"

#(define (offset-subtract a b)
   (cons (- (car b) (car a))
     (- (cdr b) (cdr a))))

% Rewrite of Offset::direction in LilyPond source file 'flower/offset.cc'
#(define (offset-direction o)
   (cond
    ((and (inf? (car o)) (not (inf? (cdr o))))
     (cons (if (> (car o) 0.0) 1.0 -1.0)
       0.0))
    ((inf? (cdr o))
     (cons 0.0
       (if (> (cdr o) 0.0) 1.0 -1.0)))
    ((and (= (car o) 0.0) (= (cdr o) 0.0))
     o)
    (else
     (let ((len (sqrt (+ (* (car o)(car o)) (* (cdr o)(cdr o))))))
       (cons (/ (car o) len) (/ (cdr o) len))))))

%% CUSTOM GROB PROPERTIES

% Taken from http://www.mail-archive.com/lilypond-user%40gnu.org/msg97663.html
% (Paul Morris)

% function from "scm/define-grob-properties.scm" (modified)
#(define (cn-define-grob-property symbol type?)
   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc "custom grob property")
   symbol)

% Count of _inner_ texts per line; does not include bound-details.left.text
% and bound-details.right.text.  This is set automatically but may be
% overridden.
#(cn-define-grob-property 'inner-text-count-by-line number-list?)

% How much space between line and object to left and right?
% Default is '(0.0 . 0.0).
#(cn-define-grob-property 'line-X-offset number-pair?)

% Vertical shift of connector line, independenf of texts.
#(cn-define-grob-property 'line-Y-offset number?)

% Turn a list of lengths into contiguous extents
% '(10 20 5) -> '((0 . 10) (10 . 30) (30 . 35))
% Used with line lengths to decide where to place an inner text
#(define (lengths->cumulative-extent-list ls)
   (let inner ((ls ls) (prev 0) (result '()))
     (if (null? ls)
         (reverse result)
         (inner
          (cdr ls)
          (+ prev (car ls))
          (cons (cons prev (+ (car ls) prev))
            result)))))

% Place a text by x coordinate within a list of contiguous extents
% representing lines
#(define (assign-line my-X line-exts)
   (let inner ((line-exts line-exts) (idx 0))
     (cond
      ((null? line-exts) #f)
      ((and (<= (caar line-exts) my-X)
            (<= my-X (cdar line-exts)))
       idx)
      (else (inner (cdr line-exts) (1+ idx))))))

% count adjacent duplicates.  Used to determine count of the inner texts assigned to
% each line of our spanner.
#(define (text-count-per-line ls line-count)
   (define (inner ls counter lines result)
     (cond
      ((null? lines) (reverse result))
      ((null? ls) (inner ls 0 (cdr lines) (cons counter result)))
      ((eq? (car ls) (car lines))
       (inner (cdr ls) (1+ counter) lines result))
      (else
       (inner ls 0 (cdr lines) (cons counter result)))))
   (if (null? ls)
       '()
       (inner ls 0 (iota line-count) '())))

% Spread texts evenly over available lines taking into account line lengths.  Return
% a list showing number of texts per line.
% TODO: take into account the extent of texts, rather than assuming them to be
% dimensionless
#(define (get-text-distribution text-list line-extents)
   (let ((line-count (length line-extents))
         (text-count (length text-list)))
     (if (= 0 text-count)
         (make-list line-count 0)
         (let* ((line-lengths
                 (map (lambda (line) (interval-length line))
                   line-extents))
                (total-line-length (apply + line-lengths))
                (space-between (/ total-line-length (1+ text-count)))
                (positions
                 (map (lambda (e) (* e space-between))
                   (iota text-count 1)))
                (segment-exts (lengths->cumulative-extent-list line-lengths))
                (line-assignments
                 (map (lambda (x) (assign-line x segment-exts))
                   positions))
                (count-per-line (text-count-per-line line-assignments line-count)))

           count-per-line))))

%% Adapted from 'justify-line-helper' in scm/define-markup-commands.scm.
#(define (inner-texts->stencils grob texts extent)
   "Given a list of markups @var{texts}, return a list of stencils spread along an extent
@var{extent}, such that the intervening spaces are equal."
   (if (null? texts)
       '()
       (let* ((line-contents
               (map (lambda (t) (grob-interpret-markup grob t)) texts))
              (text-extents
               (map (lambda (stc) (ly:stencil-extent stc X))
                 line-contents))
              ;; spacers ("") create empty-stencils, which have X-extent '(+inf.0 . -inf.0)
              ;; Change to (0.0 . 0.0) so calculations work
              (text-extents
               (map (lambda (te) (if (interval-empty? te) (cons 0.0 0.0) te))
                 text-extents))
              (text-lengths (map interval-length text-extents))
              (total-text-length (apply + text-lengths))
              (line-width (interval-length extent))
              (total-fill-space (- line-width total-text-length))
              (word-count (length line-contents))
              (padding (/ (- line-width total-text-length) (1+ word-count)))
              (distributed-stils
               (let loop ((contents line-contents) (exts text-extents)
                           (lengths text-lengths)
                           (shift (- padding (caar text-extents))) (result '()))
                 (if (null? contents)
                     (reverse result)
                     (loop
                      (cdr contents) (cdr exts) (cdr lengths)
                      (+ shift (car lengths) padding)
                      (cons
                       (ly:stencil-translate-axis
                        (car contents)
                        (+ (caar exts) shift)
                        X)
                       result)))))
              (distributed-stils
               (map (lambda (s) (ly:stencil-translate-axis s (car extent) X))
                 distributed-stils)))

         distributed-stils)))

% Create sublists in a list according to a count list.
% Given '("this" "is" "a" "list" "of" "texts") and '(1 0 3 2)
% -> ((this) () (is a list) (of texts))
#(define (nest-list-by-count text-list line-count-list)
   (let inner ((line-count-list line-count-list) (text-list text-list) (result '()))
     (if (null? line-count-list)
         (reverse result)
         (inner
          (cdr line-count-list)
          (list-tail text-list (car line-count-list))
          (cons (list-head text-list (car line-count-list)) result)))))

% Return a list of texts assigned to a line.
#(define (get-texts grob gs text-list)
   (let* ((tslc (ly:grob-property grob 'inner-text-count-by-line))
          (my-count (list-index (lambda (x) (eq? grob x)) gs))
          (text-list (nest-list-by-count text-list tslc)))
     (list-ref text-list my-count)))

% Given a list of connectors (lines drawn between texts) expressed as
% booleans, return a list with a sublist for each line.
%
% Given an input of '(#t #t #f)
%
%    '((#t        #t            #f))
%  one_ _ _ _two_ _ _ _ _three        four  (one line)
%
%       '((#t       #t)
%   one_ _ _ _two_ _ _ _ _                   (two lines)
%    (#t         #f))
%   _ _ _ _three     four
%
%       '((#t)
%    one_ _ _ _                               (four lines/blank)
%    (#t       #t)
%    _ _ _two_ _ _
%         (#t)
%    _ _ _ _ _ _ _
%   (#t      #f))
%    _ _three    four


% TODO: deal with superfluous boolean at the end of input list (not
% represented in diagram above).
#(define (get-connectors grob gs connector-list)
   (let* ((tslc (ly:grob-property grob 'inner-text-count-by-line))
          ;; Accommodate left.text and right.text.  Ugh.
          (tslc (cons (1+ (car tslc)) (cdr tslc)))
          (tslc (append (list-head tslc (1- (length tslc)))
                  (list (1+ (last tslc)))))
          (boolean-list (nest-list-by-count connector-list tslc))
          ;; copy last boolean of sublist to head of following sublist
          (boolean-list
           (let loop ((bl boolean-list)
                      (carry '())
                      (result '()))
             (cond
              ((null? bl) (reverse result))
              (else
               (loop (cdr bl)
                 (if (pair? (car bl))
                     (last (car bl))
                     carry)
                 (if (boolean? carry)
                     (cons (cons carry (car bl)) result)
                     (cons (car bl) result)))))))
          (my-count (list-index (lambda (x) (eq? grob x)) gs))
          (my-connectors (list-ref boolean-list my-count)))

     my-connectors))

% Return a stencil for a line of a text spanner including text and connector stencils.
#(define (build-line-stencil grob gs stils connectors)
   (let* (;; Remove spacers so that a single line is drawn to cover the total gap
           ;; rather than several. (Successive dashed lines will not connect properly.)
           ;; This leaves null markups which produce point stencils
           (stils (remove ly:stencil-empty? stils))
           (line-contents (apply ly:stencil-add stils))
           (extents (map (lambda (s) (ly:stencil-extent s X))
                      stils))
           (my-connectors (get-connectors grob gs connectors))
           ;; Read connector extents from list of text extents.
           ;; ((1-L . 1-R) (2-L . 2-R) (3-L . 3-R)) ;; extents of texts
           ;; ==> ((1-R . 2-L) (2-R . 3-L)) ;; extents of connector lines
           (spaces
            (if (pair? extents)
                (let loop ((orig extents)
                           (result '()))
                  (if (null? (cdr orig))
                      (reverse result)
                      (loop
                       (cdr orig)
                       (cons
                        (cons
                         (cdr (first orig))
                         (car (second orig)))
                        result))))
                '()))
           (padding (ly:grob-property grob 'line-X-offset (cons 0.0 0.0)))
           (padding-L (car padding))
           (padding-R (cdr padding))
           ;; incorporate padding
           (spaces
            (let loop ((orig spaces) (result '()))
              (cond
               ((null? orig) (reverse result))
               (else
                (loop (cdr orig)
                  (cons
                   (coord-translate
                    (car orig)
                    (cons padding-L (- padding-R)))
                   result))))))
           (offset-Y (ly:grob-property grob 'line-Y-offset 0.0))
           (connector-stils
            (append-map
             (lambda (sps joins)
               (if (and
                    ;; space too short for line
                    (not (interval-empty? sps))
                    joins)
                   (list (ly:line-interface::line grob
                           (car sps) offset-Y
                           (cdr sps) offset-Y))
                   '()))
             spaces my-connectors))
           (connector-stil (apply ly:stencil-add connector-stils))
           (line-contents (ly:stencil-add connector-stil line-contents)))

     line-contents))

% Thanks to David Kastrup for suggesting lyricmode entry for 'textSpannerInnerTexts'
% and coding advice (see http://www.mail-archive.com/lilypond-user%40gnu.org/msg105119.html)
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

% Initally based on 'addTextSpannerText,' by Thomas Morley.  See
% http://www.mail-archive.com/lilypond-user%40gnu.org/msg81685.html
% Stencil function is now an augmented rewrite in Scheme of C++ code
% found in the source file 'lily/line-spanner.cc'
textSpannerInnerTexts =
#(define-music-function (arg) (ly:music?)
   "Create a @code{TextSpanner} with end and optional inner texts.  Entry is a
music expression written in @code{lyricmode}.  Lines joining texts are drawn where
hyphens are specified.  Non-empty texts are required at beginning and end.
Empty strings (\"\") may be used in inner positions as spacers.  Use
@code{\\markup\\null} for blank beginnings or ends."
   (let* ((texts-and-connectors (extractLyricEventInfo arg))
          (texts (map car texts-and-connectors)))
     (if (or (< (length texts) 2)
             (or (eq? "" (car texts))
                 (eq? "" (last texts))
                 (equal? #{ \markup "" #} (car texts))
                 (equal? #{ \markup "" #} (last texts))))
         (begin
          (ly:warning "Beginning and ending texts required for `textSpannerInnerTexts'.")
          (make-music 'Music))

         #{
           \once \override TextSpanner.bound-details.left.text = #(car texts)
           \once \override TextSpanner.bound-details.left-broken.text = \markup \null
           \once \override TextSpanner.bound-details.right.text = #(last texts)
           \once \override TextSpanner.bound-details.right-broken.text = \markup \null

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
                      ; Triggers simple-Y calculations
                      (simple-y
                       (and (eq? #t (ly:grob-property grob 'simple-Y))
                            (not (eq? #t (ly:grob-property grob 'cross-staff)))))
                      (bound-info-L (ly:grob-property grob 'left-bound-info))
                      (bound-info-R (ly:grob-property grob 'right-bound-info))
                      (common-X (ly:grob-common-refpoint
                                 (ly:spanner-bound grob LEFT)
                                 (ly:spanner-bound grob RIGHT)
                                 X))
                      (common-X (ly:grob-common-refpoint grob common-X X))
                      (span-points
                       (list
                        (cons (assoc-get 'X bound-info-L 0.0)
                          (assoc-get 'Y bound-info-L 0.0))
                        (cons (assoc-get 'X bound-info-R 0.0)
                          (assoc-get 'Y bound-info-R 0.0))))
                      ; For scaling of 'padding and 'stencil-offset
                      (magstep (expt 2 (/ (ly:grob-property grob 'font-size 0.0) 6)))
                      ; confusingly called 'gaps' in C++ source
                      (bound-padding
                       (cons (assoc-get 'padding bound-info-L 0.0)
                         (assoc-get 'padding bound-info-R 0.0)))
                      ;; arrows not supported in Scheme yet
                      (arrows
                       (cons (assoc-get 'arrow bound-info-L #f)
                         (assoc-get 'arrow bound-info-R #f)))
                      (stencils
                       (cons (assoc-get 'stencil bound-info-L)
                         (assoc-get 'stencil bound-info-R)))
                      (common-Y
                       (cons (assoc-get 'common-Y bound-info-L grob)
                         (assoc-get 'common-Y bound-info-R grob)))
                      (my-common-Y
                       (ly:grob-common-refpoint (car common-Y) (cdr common-Y) Y))
                      (span-points
                       (if (not simple-y)
                           (list
                            (cons (caar span-points)
                              (+ (cdar span-points)
                                (ly:grob-relative-coordinate
                                 (car common-Y) my-common-Y Y)))
                            (cons (caadr span-points)
                              (+ (cdadr span-points)
                                (ly:grob-relative-coordinate
                                 (cdr common-Y) my-common-Y Y))))
                           span-points))
                      (normalized-endpoints
                       (ly:grob-property grob 'normalized-endpoints (cons 0 1)))
                      (Y-length (- (cdadr span-points) (cdar span-points)))
                      (span-points
                       (list
                        (cons (caar span-points)
                          (+ (cdar span-points)
                            (* (car normalized-endpoints) Y-length)))
                        (cons (caadr span-points)
                          (- (cdadr span-points)
                            (* (- 1 (cdr normalized-endpoints)) Y-length)))))
                      (dz (offset-subtract (car span-points) (cadr span-points)))
                      (dz-dir (offset-direction dz)))
                ;; Draw nothing if total padding is larger than line's length
                (if (> (+ (car bound-padding) (cdr bound-padding))
                       (sqrt (+ (* (car dz) (car dz)) (* (cdr dz) (cdr dz)))))
                    '()
                    (let* ((line-stencil empty-stencil)
                           ;; adjust endpoints for padding
                           (span-points
                            (list
                             (coord-translate (car span-points)
                               (coord-scale dz-dir (* (car bound-padding) magstep)))
                             (coord-translate (cadr span-points)
                               (coord-scale dz-dir (* -1 (cdr bound-padding) magstep)))))
                           (left-stencil
                            (if (car stencils)
                                (ly:stencil-translate (car stencils) (car span-points))
                                #f))
                           (left-align (assoc-get 'stencil-align-dir-y bound-info-L #f))
                           (left-off (assoc-get 'stencil-offset bound-info-L #f))
                           (left-stencil
                            (if (and left-stencil (number? left-align))
                                (ly:stencil-aligned-to left-stencil Y left-align)
                                left-stencil))
                           (left-stencil
                            (if (and left-stencil (number-pair? left-off))
                                (ly:stencil-translate left-stencil (offset-scale left-off magstep))
                                left-stencil))
                           (line-stencil
                            (if left-stencil
                                (ly:stencil-add line-stencil left-stencil)
                                line-stencil))
                           (right-stencil
                            (if (cdr stencils)
                                (ly:stencil-translate (cdr stencils) (cadr span-points))
                                #f))
                           (right-align (assoc-get 'stencil-align-dir-y bound-info-R #f))
                           (right-off (assoc-get 'stencil-offset bound-info-R #f))
                           (right-stencil
                            (if (and right-stencil (number? right-align))
                                (ly:stencil-aligned-to right-stencil Y right-align)
                                right-stencil))
                           (right-stencil
                            (if (and right-stencil (number-pair? right-off))
                                (ly:stencil-translate right-stencil (offset-scale right-off magstep))
                                right-stencil))
                           (line-stencil
                            (if right-stencil
                                (ly:stencil-add line-stencil right-stencil)
                                line-stencil))
                           ;; Adjust endpoints to clear stencils
                           (span-points
                            (list
                             (if (ly:stencil? (car stencils))
                                 (coord-translate (car span-points)
                                   (offset-scale
                                    dz-dir
                                    (/ (cdr (ly:stencil-extent (car stencils) X))
                                      (car dz-dir))))
                                 (car span-points))
                             (if (ly:stencil? (cdr stencils))
                                 (coord-translate (cadr span-points)
                                   (offset-scale
                                    dz-dir
                                    (/ (car (ly:stencil-extent (cdr stencils) X))
                                      (car dz-dir))))
                                 (cadr span-points))))
                           ;; for arrow
                           (adjust (offset-scale dz-dir
                                     (ly:staff-symbol-staff-space grob)))
                           (line-left (car span-points)) ; TODO: support arrow
                           (line-right (cadr span-points)) ; TODO: support arrow
                           (inner-texts (cdr (list-head texts (1- (length texts))))))

                      ;; get and cache line distribution
                      (if (null? (ly:grob-property grob 'inner-text-count-by-line))
                          (let* ((line-exts
                                  (map (lambda (gs) (ly:spanner::bounds-width gs))
                                    grob-or-siblings))
                                 (tslc (get-text-distribution inner-texts line-exts)))
                            (for-each
                             (lambda (sp)
                               (set! (ly:grob-property sp 'inner-text-count-by-line)
                                     tslc))
                             grob-or-siblings)))

                      (let* ((my-inner-texts (get-texts grob grob-or-siblings inner-texts))
                             (inner-text-stencils
                              (inner-texts->stencils grob my-inner-texts
                                (cons (car line-left) (car line-right))))
                             (all-text-stencils (cons left-stencil inner-text-stencils))
                             (all-text-stencils (append all-text-stencils (list right-stencil)))
                             (connector-list (map cdr texts-and-connectors))
                             (line-stencil
                              (ly:stencil-add line-stencil
                                (build-line-stencil grob grob-or-siblings all-text-stencils
                                  connector-list)))
                             (line-stencil
                              (ly:stencil-translate line-stencil
                                (cons
                                 (- (ly:grob-relative-coordinate
                                     grob common-X X))
                                 (if simple-y
                                     0.0
                                     (- (ly:grob-relative-coordinate
                                         grob my-common-Y Y)))))))

                        line-stencil)))))
         #})))

%%%%%%%%%%%%%%%%%%%%%%%%%%% END FUNCTIONS TO INCLUDE %%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\markup \bold "Default (no inner text possible)"

{
  \override TextSpanner.bound-details.left.text = "ral"
  \override TextSpanner.bound-details.left-broken.text = ##f
  \override TextSpanner.bound-details.right.text = "do"
  \override TextSpanner.bound-details.right-broken.text = ##f
  c'1\startTextSpan
  \break
  c'1\stopTextSpan
}

\markup \bold "All on one line"

{
  \textSpannerInnerTexts \lyricmode { ral -- len -- tan -- do }
  c'1\startTextSpan
  c'1\stopTextSpan
}

\markup \bold "Broken"

{
  \textSpannerInnerTexts \lyricmode { ral -- len -- tan -- do }
  c'1\startTextSpan
  \break
  %c1\break
  c'1\stopTextSpan
}

\markup \bold "Empty line/manual distribution"

{
  \override TextSpanner.inner-text-count-by-line = #'(0 0 1 0)
  \textSpannerInnerTexts \lyricmode { one -- two -- three }
  c'1~\startTextSpan
  \break
  c'1~
  \break
  c'1~
  \break
  c'1\stopTextSpan
}

\markup \bold "Changes of ends"

{
  \textSpannerInnerTexts \lyricmode { one -- two -- three }
  c'1\startTextSpan
  c'1\stopTextSpan
  \once \override TextSpanner.bound-details.left.padding = #-2
  \once \override TextSpanner.bound-details.right.padding = #-5
  \textSpannerInnerTexts \lyricmode { one -- two -- three }
  c'1\startTextSpan
  c'1\stopTextSpan
}

\markup \bold "Markups"

{
  \textSpannerInnerTexts \lyricmode {
    \markup one -- \markup two -- \markup three
  }
  c'1\startTextSpan
  c'1\stopTextSpan
  \textSpannerInnerTexts \lyricmode {
    \markup one --
    \markup \with-color #red \translate #'(-3 . 0) two --
    \markup three
  }
  c'1\startTextSpan
  c'1\stopTextSpan
  \override TextSpanner.style = #'dotted-line
  \override TextSpanner.dash-period = #0.5
  \textSpannerInnerTexts \lyricmode {
    \markup \right-align one --
    two --
    \markup \center-align three --
  }
  c'1\startTextSpan
  c'1\stopTextSpan
}

{
  \override TextSpanner.style = #'zigzag
  \override TextSpanner.line-X-offset = #'(0.5 . 0.5)
  \textSpannerInnerTexts \lyricmode
  {
    \markup \draw-circle #1 #0.2 ##f --
    \markup \halign #LEFT \with-color #grey \draw-circle #1 #0.2 ##t --
    \markup \halign #LEFT \draw-circle #1 #0.2 ##t --
    \markup \halign #LEFT \with-color #grey \draw-circle #1 #0.2 ##t --
    \markup \draw-circle #1 #0.2 ##f
  }
  c''1~\startTextSpan
  c''4\stopTextSpan r r2
}

\markup \bold "Showing/hiding connectors"

{
  c'1
  \override TextSpanner.padding = 3
  \override TextSpanner.inner-text-count-by-line = #'(2 0 1)
  \textSpannerDown
  \textSpannerInnerTexts \lyricmode {
    poco a poco dim. -- \markup \dynamic mf
  }
  c'1\startTextSpan
  c'1 c'
  \break
  c'1 c' c' c'
  \break
  c'1 c' c'
  c'1\stopTextSpan
}

\markup \bold "Raising/lowering of connector line"

{
  \override TextSpanner.line-X-offset = #'(1 . 1)
  \override TextSpanner.line-Y-offset = 0.5
  \textSpannerInnerTexts \lyricmode { ral -- len -- tan -- do }
  c'1\startTextSpan
  c'1\stopTextSpan
}

\layout {
  indent = 0
  ragged-right = ##f
}
