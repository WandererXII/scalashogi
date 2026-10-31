package shogi

import shogi.format.InitialPosition
import shogi.format.ParsedNotation
import shogi.format.ParsedSteps
import shogi.format.Tag
import shogi.format.Tags
import shogi.format.csa.CsaParser
import shogi.format.forsyth.Sfen
import shogi.format.kif.KifParser
import shogi.format.usi.Usi
import shogi.variant.Standard

class ReplayClockTest extends ShogiTest {
  private val tags = Tags(List(Tag(_.TimeControl, "10分")))

  private def read(parsed: ParsedNotation) = Replay.fromParsedNotation(parsed, identity).valid

  private def notation(times: List[Option[Int]], clockTags: Tags = tags) =
    ParsedNotation(
      CsaParser
        .objSteps(
          List("+7776FU", "-3334FU", "+2726FU").zip(times).map { case (move, seconds) =>
            CsaParser.StrStep(move, Nil, seconds.map(Centis.ofSeconds))
          },
        )
        .toOption
        .get,
      None,
      Standard,
      InitialPosition(Nil),
      clockTags,
    )

  "imported clocks" should {
    "return the initial configured game when no moves are selected" in {
      var calls  = 0
      val parsed = notation(List(Some(30), Some(10)))
      Replay
        .fromParsedNotation(
          parsed,
          _ => {
            calls += 1
            ParsedSteps.empty
          },
        )
        .valid must beValid.like { case game =>
        game.toSfen must_== Game(Standard).toSfen
        game.usis must beEmpty
        game.clock must beSome.like { case clock =>
          clock.players(Sente).remaining must_== Centis.ofSeconds(600)
          clock.players(Gote).remaining must_== Centis.ofSeconds(600)
          clock.color must_== Sente
          clock.isRunning must beFalse
        }
      }
      calls must_== 1
    }

    "use recorded KIF times and stay stopped" in {
      val parsed = KifParser
        .full("""持ち時間：10分
手数----指手---------消費時間--
1 ７六歩(77) (0:30/00:00:30)
2 ３四歩(33) (0:10/00:00:10)
""").toOption
        .get
      read(parsed) must beValid.like { case game =>
        game.clock must beSome.like { case clock =>
          clock.players(Sente).remaining must_== Centis.ofSeconds(570)
          clock.players(Gote).remaining must_== Centis.ofSeconds(590)
          clock.isRunning must beFalse
          clock.color must_== game.color
          clock.lag(Sente) must_== LagTracker.init(clock.config)
          clock.lag(Gote) must_== LagTracker.init(clock.config)
        }
      }
    }

    "use recorded CSA times for every move, including the opening moves" in {
      val parsed = CsaParser
        .full("""$TIME_LIMIT:00:10+0
PI
+
+7776FU
T30
-3334FU
T10
+2726FU
T20
""").toOption
        .get
      read(parsed) must beValid.like { case game =>
        game.clock must beSome.like { case clock =>
          clock.players(Sente).remaining must_== Centis.ofSeconds(550)
          clock.players(Gote).remaining must_== Centis.ofSeconds(590)
          clock.lastTimeOf(Sente) must_== Centis.ofSeconds(20)
          clock.isRunning must beFalse
        }
      }
    }

    "keep USI imports stopped without inventing elapsed times" in {
      Replay.fromUsi(Usi.readList("7g7f 3c3d 2g2f").get, None, Standard, tags).valid must
        beValid.like { case game =>
          game.clock must beSome.like { case clock =>
            clock.players(Sente).remaining must_== Centis.ofSeconds(600)
            clock.players(Gote).remaining must_== Centis.ofSeconds(600)
            clock.color must_== Gote
            clock.isRunning must beFalse
          }
        }
    }

    "distinguish missing move times from recorded zero when applying increment" in {
      read(notation(List(None, Some(0), Some(30)), Tags(List(Tag(_.TimeControl, "10分|0+5"))))) must
        beValid.like { case game =>
          game.clock must beSome.like { case clock =>
            clock.players(Sente).remaining must_== Centis.ofSeconds(575)
            clock.players(Gote).remaining must_== Centis.ofSeconds(605)
            clock.isRunning must beFalse
          }
        }
    }

    "apply byoyomi periods using recorded durations" in {
      read(notation(List(Some(75), Some(30)), Tags(List(Tag(_.TimeControl, "1分|10秒(3)"))))) must
        beValid.like { case game =>
          game.clock must beSome.like { case clock =>
            clock.players(Sente).remaining must_== Centis.ofSeconds(10)
            clock.spentPeriodsOf(Sente) must_== 2
            clock.players(Gote).remaining must_== Centis.ofSeconds(30)
            clock.isRunning must beFalse
          }
        }
    }

    "charge the starting player in a Gote-to-move position" in {
      val parsed    = notation(List(Some(30), Some(10)))
      val goteFirst = parsed.copy(
        initialSfen = Some(Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1")),
        parsedSteps = ParsedSteps(parsed.parsedSteps.value.reverse),
      )
      read(goteFirst) must beValid.like { case game =>
        game.clock must beSome.like { case clock =>
          clock.players(Gote).remaining must_== Centis.ofSeconds(590)
          clock.players(Sente).remaining must_== Centis.ofSeconds(570)
          clock.color must_== Gote
          clock.isRunning must beFalse
        }
      }
    }

    "reload byoyomi on later moves without consuming another period" in {
      read(
        notation(
          List(Some(75), Some(30), Some(4)),
          Tags(List(Tag(_.TimeControl, "1分|10秒(3)"))),
        ),
      ) must beValid.like { case game =>
        game.clock must beSome.like { case clock =>
          clock.players(Sente).remaining must_== Centis.ofSeconds(10)
          clock.spentPeriodsOf(Sente) must_== 2
          clock.lastTimeOf(Sente) must_== Centis.ofSeconds(4)
          clock.isRunning must beFalse
        }
      }
    }

    "match unchanged live-clock accounting for either player and terminal positions" in {
      val configs = List(
        Clock.Config(60, 0, 0, 0),
        Clock.Config(60, 5, 0, 0),
        Clock.Config(60, 0, 10, 3),
        Clock.Config(0, 0, 10, 3),
        Clock.Config(0, 5, 0, 0),
      )
      val cases = for {
        config   <- configs
        color    <- List(Sente, Gote)
        terminal <- List(false, true)
        elapsed  <- List(0, 1, 999, 1000, 5999, 6000, 6001, 6999, 7000, 7999, 8000, 9000)
      } yield (config, color, terminal, Centis(elapsed))
      forall(cases) { case (config, color, terminal, elapsed) =>
        val (sfen, move) = (terminal, color) match {
          case (true, Sente) =>
            (Some(Sfen("4k4/9/4G4/4R4/9/9/9/9/4K4 b - 1")), "+5352KI")
          case (true, Gote) =>
            (Some(Sfen("4k4/9/9/9/9/4r4/4g4/9/4K4 w - 1")), "-5758KI")
          case (false, Sente) => (None, "+7776FU")
          case (false, Gote)  =>
            (
              Some(Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1")),
              "-3334FU",
            )
        }
        val timeControl =
          s"${config.limitSeconds}秒|${config.byoyomiSeconds}秒(${config.periodsTotal})+${config.incrementSeconds}秒"
        val parsed = ParsedNotation(
          CsaParser.objSteps(List(CsaParser.StrStep(move, Nil, Some(elapsed)))).toOption.get,
          sfen,
          Standard,
          InitialPosition(Nil),
          Tags(List(Tag(_.TimeControl, timeControl))),
        )
        val live = Clock(config, color)
          .copy(
            timer = Some(Timestamp(0)),
            timestamper = new Timestamper {
              val now = Timestamp(elapsed.millis)
            },
          )
          .step(gameActive = !terminal)
        read(parsed) must beValid.like { case game =>
          game.situation.status.isDefined must_== terminal
          game.clock must beSome.like { case clock =>
            clock.config must_== config
            clock.color must_== live.color
            clock.isRunning must beFalse
            clock.players.map(_.remaining) must_== live.players.map(_.remaining)
            clock.players.map(_.spentPeriods) must_== live.players.map(_.spentPeriods)
            clock.players.map(_.lastTime) must_== live.players.map(_.lastTime)
            clock.lag(Sente) must_== LagTracker.init(config)
            clock.lag(Gote) must_== LagTracker.init(config)
          }
        }
      }
    }

    "retain a stopped clock at the last valid move on an incomplete import" in {
      val parsed = notation(List(Some(30), Some(10)))
      Replay.fromParsedNotation(
        parsed.copy(parsedSteps =
          ParsedSteps(parsed.parsedSteps.value :+ parsed.parsedSteps.value.head),
        ),
        identity,
      ) must beLike { case Replay.Result.Incomplete(game, _) =>
        game.playedPlies must_== 2
        game.clock.get.players(Sente).remaining must_== Centis.ofSeconds(570)
        game.clock.get.isRunning must beFalse
      }
    }

    "leave the setup intact when the first parsed move fails" in {
      val parsed = notation(List(Some(30), Some(10)))
      val result = Replay.fromParsedNotation(
        parsed.copy(parsedSteps = ParsedSteps(parsed.parsedSteps.value.reverse)),
        identity,
      )
      result must beLike { case Replay.Result.Incomplete(game, _) =>
        game.usis must beEmpty
        game.playedPlies must_== 0
        game.clock.get.players(Sente).remaining must_== Centis.ofSeconds(600)
        game.clock.get.players(Gote).remaining must_== Centis.ofSeconds(600)
        game.clock.get.isRunning must beFalse
      }
      result.valid must beInvalid
    }

    "leave records without a clock configuration clockless" in {
      read(notation(List(Some(30), Some(10)), Tags.empty)) must beValid.like { case game =>
        game.playedPlies must_== 2
        game.clock must beNone
      }
    }

    "use the resulting situation to withhold increment on a terminal move" in {
      val parsed = notation(List(Some(10), Some(20)), Tags(List(Tag(_.TimeControl, "1分|0+5"))))
        .copy(
          initialSfen = Some(Sfen("9/3gk4/9/2b6/9/6B2/9/4KG3/9 b - 12")),
          variant = shogi.variant.Checkshogi,
          parsedSteps = CsaParser
            .objSteps(
              List(
                CsaParser.StrStep("+4847KI", Nil, Some(Centis.ofSeconds(10))),
                CsaParser.StrStep("-7485KA", Nil, Some(Centis.ofSeconds(20))),
              ),
            )
            .toOption
            .get,
        )
      read(parsed) must beValid.like { case game =>
        game.situation.status must_== Some(Status.Check)
        game.situation.winner must_== Some(Gote)
        game.startedAtStep must_== 12
        game.stepNumber must_== 14
        game.clock must beSome.like { case clock =>
          clock.players(Sente).remaining must_== Centis.ofSeconds(55)
          clock.players(Gote).remaining must_== Centis.ofSeconds(40)
          clock.color must_== game.color
          clock.isRunning must beFalse
        }
      }
    }
  }
}
