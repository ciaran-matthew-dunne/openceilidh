
# Summary

`opencelilidh` aims to provide tools for humans who interact with traditional Scottish music. 
The core representation used for musical score is
  [MusicXML 4.0](https://www.w3.org/2021/06/musicxml40/musicxml-reference/).

![Pixel art of robots playing together against the backdrop of a Scottish village.](robots.webp)

Internally, we use the Haskell [`Text.XML`](https://hackage.haskell.org/package/xml-conduit-1.9.1.3/docs/Text-XML.html) module to work with MusicXML documents[^1].


[^1]: There are two Haskell libraries that provide datatypes specifically for 
  MusicXML documents (
    [`musicxml`](https://hackage.haskell.org/package/musicxml2), 
    [`musicxml2`](https://hackage.haskell.org/package/musicxml2)
  ), but unfortunately these led me to [dependency hell](https://xkcd.com/1579/).
  Maybe I'll try and fix this at some point.  

## Basics

Informally, a "tune" is a musical form that is comprised of one or more "sections".
 
A "section" is a sequence of measures labelled by an uppercase Latin letter, 
possibly preceded by a special measure called an "anacrusis". 

A tune execution is a composition of sections (e.g., "ABBA").

Because of the presence of [repeat signs](https://en.wikipedia.org/wiki/Repeat_sign)
in sequences of measures, the musical content of a section may be repeated.
We refer to iteration `n` of the musical content in section `S` as `Sₙ`.

A "ceilidh set" is a sequence of tune executions. Hereinafter, we call these 
"seata" (the Gaelic word for "set") to avoid a name-clash with the mathematical concept of [set](https://en.wikipedia.org/wiki/Set_(mathematics)).

- A *measure* has type `Node` (imported by `Text.XML`). 

- A *section* is a pair `(mx, xs)`, where `mx ∷ Maybe Node` and `xs ∷ [Node]`.
  That is, `xs` is a list of measures, and either `mx = Just x` for some measure `x`, or `mx = Nothing`.

- A *tune* has a `title ∷ String`, a time signature `(i,j)` (where `i,j > 0`),
  and one or more sections indexed by `Char`.

- A *seata* has a `title ∷ String` and list `ts ∷ [(Tune, String)]`.
  Each member of `ts` specifies a tune execution. 
  For example, the pair `(t, "ABA")` specifies the execution of tune `t` by playing
  the `A` part, followed by the `B` part, then finishing on the `A` part.   

## Limitations

Given tunes `T₁` and `T₂`, ....


# Disclaimer for those concerned about Western hegemony on music theory  

This project aims to provide tooling and support for Scottish traditional music.
Within the context of `openceilidh`, "Western" musical notation and concepts are
used. This is not a design choice, but a result of the author playing in a ceilidh band
that use these notations and concepts. 
[ABC notation](https://en.wikipedia.org/wiki/ABC_notation) 
  (which is much more computer-friendly) will be supported in the future.

Alternative "non-Western" techniques for representing music may be deemed more preferable
by others, but such approaches are outside the scope of this project.