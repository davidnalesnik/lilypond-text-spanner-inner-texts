LilyPond Text Spanners with Inner Text
======================================

## What's this?

A "text spanner" is GNU LilyPond parlance for a text indication which
stretches over a portion of a musical score, often with a dashed or dotted
line connecting the text segments.  LilyPond supports indications
for which there is a beginning and/or ending text, but does not provide
an automated means of creating spanners with additional texts (as when
a word such as *rallentando* appears as *ral - - len - - tan - - do*).

## How is it used?

To make this tool available to your LilyPond file, you can either

1. copy the code in `text-spanner-inner-texts.ily` directly into your `ly` file, or
2. add `\include "text-spanner-inner-texts.ily"` to the head (assuming
that both files are in the same directory).

The two methods do the same thing, but using `include` is a good way to keep your `.ly`
file uncluttered.

Text spanners are created the usual way.  See [here](http://lilypond.org/doc/v2.19/Documentation/notation/writing-text#text-spanners) for more information.

For enhanced functionality involving inner texts, the command `textSpannerInnerTexts`
should be used before the spanner is begun with `startTextSpan`.

The new command takes a single argument, a music expression written in `lyricmode`.
Texts and markups are entered as usual, and connecting lines are represented
through hyphen syntax (namely, `--`).  Currently, string\markup and hyphen entry
are the only features of `lyricmode` supported.

A (non-empty) first and last text must be present.

Empty strings (`""`) can be used as spacers to modify the relative distances
between texts.  Spacers may not be used for a blank text in first or last
position; instead, `\markup \null` should be used.  Null-markups should
not be used as spacers.

An even distribution will be calculated automatically for spanners which stretch
over line breaks.  You may specify an alternate distribution through an
override of the property `inner-text-line-count`.  Perhaps confusingly, this
property counts *inner* texts, and as such does not include starting and ending
texts.

More space between connecting lines and texts may be created by overriding
the `line-X-offset` property.

The connecting lines may be raised or lowered in relation to texts through
the `line-Y-offset` property.

## Examples

The file `example.ly` contains illustrations of usage.
