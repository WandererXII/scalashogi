Shogi API forked from [scalachess](https://github.com/lichess-org/scalachess) and rewritten for shogi.
Used on [lishogi.org](https://lishogi.org).

It is entirely functional, immutable, and free of side effects.

## INSTALL

Clone scalashogi

    git clone git://github.com/WandererXII/scalashogi

Get latest sbt on http://www.scala-sbt.org/download.html

Start sbt in scalashogi directory

    sbt

In the sbt shell, to compile scalashogi, run

    compile

To run the tests (with coverage):

    clean coverage test
    coverageReport

### Code formatting

This repository uses [scalafmt](https://scalameta.org/scalafmt/).

Please [install it for your code editor](https://scalameta.org/scalafmt/docs/installation.html)
if you're going to contribute to this project.

If you don't install it, please run `scalafmtAll` in the sbt console before committing.
