\version "2.19.46"

\include "text-spanner-inner-texts.ily"

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

\markup \bold "Spacers"

{
  \textSpannerInnerTexts \lyricmode { ral -- "" -- len -- tan -- do }
  c'1\startTextSpan
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

\markup \bold "Markups/line styles"

{
  \textSpannerInnerTexts \lyricmode {
    \markup one -- \markup two -- \markup three
  }
  c'1\startTextSpan
  c'1\stopTextSpan
  \override TextSpanner.style = ##f
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
  c'1\ff
  \override TextSpanner.padding = 3
  \override TextSpanner.inner-text-count-by-line = #'(3 0)
  \textSpannerDown
  \textSpannerInnerTexts \lyricmode {
    poco a poco dim. -- \markup \dynamic mf
  }
  c'1\startTextSpan
  c'1 c'
  \break
  c'1 c' c' c'
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

\markup \bold "No text at ends"

{
  \textSpannerInnerTexts \lyricmode {
    \markup \null -- foo -- bar -- \markup \null
  }
  c'1\startTextSpan
  c'1\stopTextSpan
}

\markup \bold "Bracket"

{
  \textSpannerInnerTexts \lyricmode {
    \markup \draw-line #'(0 . -1) -- foo -- bar -- \markup \draw-line #'(0 . -1)
  }
  c'1\startTextSpan
  c'1\stopTextSpan
}

\layout {
  indent = 0
  ragged-right = ##f
}
