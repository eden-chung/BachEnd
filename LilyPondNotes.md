
### Installation instructions
https://lilypond.org/doc/v2.24/Documentation/learning/installing

Then run `lilypond file.ly` which will then produce a PDF output.


LilyPond syntax
```
{ c' d' e' f' g' a' b' c''
  g c' e' g' c'' e'' g'' c''' }
```
Ticks represent the pitch

Number after the letter represents the duration
```
\relative {
  a'1
  a2 a4 a8 a
  a16 a a a a32 a a a a64 a a a a a a a a2
}
```