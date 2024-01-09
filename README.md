# Scratch Pad

Having fun while implementing various ideas, from scratch.

Scribbles:
  - Parser Combinator & Parsing JSON into an AST - [ParserCombinators.scala](modules/scratch-pad-2/src/main/scala/io/svranesevic/scratchpad/ParserCombinators.scala)
    - Type-level concepts related to [combinator with convenient zip semantics](modules/scratch-pad-2/src/main/scala/io/svranesevic/scratchpad/ParserCombinators.scala#L232-L242): [Zippable.scala](modules/scratch-pad-2/src/main/scala/io/svranesevic/scratchpad/Zippable.scala), and it's Scala 3 variant [Zipped.scala](modules/scratch-pad-3/src/main/scala/io/svranesevic/scratchpad/Zipped.scala).
  - Composable lens - [Lens.scala](modules/scratch-pad-2/src/main/scala/io/svranesevic/scratchpad/Lens.scala)
  - State monad - [State.scala](modules/scratch-pad-2/src/main/scala/io/svranesevic/scratchpad/State.scala)
  - Writer monad - [Writer.scala](modules/scratch-pad-2/src/main/scala/io/svranesevic/scratchpad/Writer.scala)
  - Take on [There is no Fork: an Abstraction for Efficient, Concurrent, and Concise Data Access](http://simonmar.github.io/bib/papers/haxl-icfp14.pdf) implementation - [Fetch.scala](modules/scratch-pad-3/src/main/scala/io/svranesevic/scratchpad/Fetch.scala)
  - Parsing [Coralogix DataPrime Query Language](https://coralogix.com/docs/dataprime-query-language/) into an AST using parboiled2 - [DataPrimeQueryLanguage.sc](Worksheets/DataPrimeQueryLanguage.sc)
