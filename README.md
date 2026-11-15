[![lishogi.org](https://img.shields.io/badge/☗_lishogi.org-Play_shogi-black)](https://lishogi.org)
[![Build and test](https://github.com/WandererXII/scalashogi/actions/workflows/build.yml/badge.svg)](https://github.com/WandererXII/scalashogi/actions/workflows/build.yml)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.wandererxii/scalashogi_2.13.svg?label=Maven%20Central)](https://search.maven.org/search?q=%22io.github.wandererxii.scalashogi_2.13%22)

# scalashogi

A shogi rules engine and notation library for Scala. It powers [lishogi.org](https://lishogi.org).

It is entirely functional, immutable and free of side effects. Originally forked from
[scalachess](https://github.com/lichess-org/scalachess) and rewritten for shogi.

## Features

- **Full rules** – legal moves and drops, promotion (optional and forced), check, checkmate,
  stalemate, illegal drops (nifu, uchifuzume), repetition (sennichite), perpetual check, impasse and variant-specific endings.
- **Variants** – Standard, Minishogi, Chushogi, Annan shogi, Kyoto shogi, Checkshogi and Dobutsu.
- **Handicaps** – all common handicap setups, with Japanese and English names.
- **SFEN** – read and write positions.
- **USI** – parse and render moves and drops, plus a compact binary encoding for storage.
- **KIF** – parse and export, including variations, comments, clocks and handicap headers.
- **CSA** – parse and export, including comments, clocks and initial positions.
- **Replay** – play a list of moves from any position and get every intermediate position,
  or the last valid position and the first error.
- **Position hashing** – compact position hashes used for repetition detection.

## Installation

scalashogi is published to Maven Central for Scala 2.13:

```scala
libraryDependencies += "io.github.wandererxii" %% "scalashogi" % "<version>"
```

See the badge above for the latest version, and [CHANGELOG.md](CHANGELOG.md) for what changed.

## Examples

All examples assume:

```scala
import shogi._
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi
import shogi.variant.Standard
```

### Playing moves

Every move returns a new `Position`, wrapped in a cats `Validated` that carries an error
message for illegal moves.

```scala
val start = Position(Standard)

val result = for {
  p1 <- start(Usi("7g7f").get)
  p2 <- p1(Usi("3c3d").get)
  p3 <- p2(Usi("8h2b+").get) // bishop takes bishop and promotes
} yield p3

result.map(_.toSfen)
// Valid(lnsgkg1nl/1r5+B1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL w B 4)

start(Usi("5i5g").get)
// Invalid(Piece on 5i cannot move to 5g)
```

### Inspecting a position

```scala
val pos = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL w - 2")
  .toPosition(Standard)
  .get

pos.color                           // Gote
pos.moveDestsFrom(Square.SQ8C)      // Some(List(8d))
pos.check                           // false
pos.status                          // None – the game goes on
pos.hands(Sente).isEmpty            // true
println(pos.visual)                 // ASCII board
```

### Game end

```scala
val mate = Sfen("4k4/4G4/4P4/9/9/9/9/9/4K4 w - 1").toPosition(Standard).get

mate.status // Some(Mate)
mate.winner // Some(Sente)
```

### Replaying a game

```scala
val usis = Usi.readList("7g7f 3c3d 8h2b+ 3a2b").get

Replay.fromUsi(usis, None, Standard) match {
  case Replay.Result.Complete(position)            => position.toSfen
  case Replay.Result.Incomplete(position, failure) => s"stopped at ${position.plies}: $failure"
  case Replay.Result.InvalidSfen(failure)          => failure
}

// every position along the way, starting with the setup
val (positions, error) = Replay.positionsWhileValid(usis, None, Standard)
```

### Variants and handicaps

```scala
import shogi.variant.{ Chushogi, Minishogi, Variant }

Position(Minishogi).toSfen          // Sfen(rbsgk/4p/5/P4/KGSBR b - 1)
Variant("chushogi")                 // Some(Variant(Chushogi))
Position(Chushogi).hasDestinations // true

val handicaps = Handicap.allByVariant(Standard)
handicaps.map(_.japanese)           // 香落ち, 右香落ち, 両香落ち, 角落ち, ...
Position(Some(handicaps.head.sfen), Standard) // lance handicap, Gote to move
```

### KIF and CSA

Parse a game record, then replay it to get the resulting position:

```scala
import shogi.format.kif.KifParser
import shogi.format.csa.CsaParser

val kif = """先手：Sente
            |後手：Gote
            |手数----指手---------消費時間--
            |   1 ７六歩(77)
            |   2 ３四歩(33)
            |   3 投了""".stripMargin

for {
  parsed <- KifParser.full(kif)
  pos    <- Replay.fromParsedNotation(parsed, identity).valid
} yield (parsed.tags, pos.toSfen)
// Valid((Sente：Sente, Gote：Gote, Result：gote, ...), lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3)

CsaParser.full("PI\n+\n+7776FU\n-3334FU\n%TORYO")
```

Export with `Kif(...).render` or `Csa(...).render`, built from a list of `NotationStep`s:

```scala
import shogi.format.{ NotationStep, Tag, Tags }
import shogi.format.kif.Kif

val steps = List(NotationStep(1, Usi.WithRole(Usi("7g7f").get, Pawn)))

Kif(steps, None, Standard, tags = Tags(List(Tag(_.Sente, "Habu")))).render
// 先手：Habu
// ...
//    1   ７六歩(77)
```

### Binary USI encoding

```scala
import shogi.format.usi.Binary

val bytes = Binary.encode(usis, Standard)
Binary.decode(bytes.toList, Standard, usis.size) == usis.toVector // true
```

## Development

```sh
sbt compile
sbt test
```

This repository uses [scalafmt](https://scalameta.org/scalafmt/). Please
[set it up in your editor](https://scalameta.org/scalafmt/docs/installation.html),
or run `sbt scalafmtAll` before committing.

To try local changes in lishogi before a release, see [doc/LOCAL_PUBLISH.md](doc/LOCAL_PUBLISH.md).

## License

[MIT](LICENSE)
