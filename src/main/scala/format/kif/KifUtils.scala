package shogi
package format
package kif

import cats.data.NonEmptyList
import cats.syntax.option._

import shogi.format.csa.CsaUtils
import shogi.format.forsyth.SfenUtils
import shogi.variant._

object KifUtils {

  def parseKifPos(str: String): Option[Pos] = {
    Pos.fromKey(str) orElse {
      for {
        numStr <- if (str.sizeIs == 2) str.map(toDigit).some else none
        x      <- numStr.take(1).toIntOption
        y      <- numStr.drop(1).toIntOption
        pos    <- Pos.at(x - 1, y - 1)
      } yield pos
    }
  }

  // supporting max 999
  def kanjiToInt(str: String): Int = {
    def orderHelper(ord: String): Int = {
      if (ord == "") 1
      else if (ord.contains('十')) 10 * orderHelper(ord.filterNot(_ == '十'))
      else if (ord.contains('百')) 100 * orderHelper(ord.filterNot(_ == '百'))
      else ord.map(toDigit _).toIntOption | 0
    }
    str.split("""(?<=(百|十))""").foldLeft(0) { (acc, cur) =>
      acc + orderHelper(cur)
    }
  }

  // supporting max 999, min 1
  def intToKanji(num: Int): String = {
    if (num >= 200) intToKanji(num / 100) + "百" + intToKanji(num % 100)
    else if (num >= 100) "百" + intToKanji(num % 100)
    else if (num >= 20) intToKanji(num / 10) + "十" + intToKanji(num % 10)
    else if (num >= 10) "十" + intToKanji(num % 10)
    else if (num == 0) ""
    else num.toString.map(toKanjiDigit _)
  }

  def intToFullWidth(num: Int): String =
    num.toString.map { d =>
      (d + 0xfee0).toChar.toString
    }.mkString

  private[kif] def toDigit(c: Char): Char = {
    c match {
      case '１' | '一' | 'a' => '1'
      case '２' | '二' | 'b' => '2'
      case '３' | '三' | 'c' => '3'
      case '４' | '四' | 'd' => '4'
      case '５' | '五' | 'e' => '5'
      case '６' | '六' | 'f' => '6'
      case '７' | '七' | 'g' => '7'
      case '８' | '八' | 'h' => '8'
      case '９' | '九' | 'i' => '9'
      case _               => c
    }
  }

  private[kif] def toKanjiDigit(c: Char): Char = {
    c match {
      case '1' => '一'
      case '2' => '二'
      case '3' => '三'
      case '4' => '四'
      case '5' => '五'
      case '6' => '六'
      case '7' => '七'
      case '8' => '八'
      case '9' => '九'
      case _   => c
    }
  }

  // head used in kif model
  val defaultHandicaps: Map[Variant, NonEmptyList[String]] = Map(
    Minishogi  -> NonEmptyList.of("5五将棋", "五々将棋", "５五将棋", "5五", "五々", "５五", "minishogi"),
    Standard   -> NonEmptyList.of("平手"),
    Chushogi   -> NonEmptyList.of("平手", "中将棋", "chushogi", "chuushogi"),
    Annanshogi -> NonEmptyList.of("安南将棋", "安南", "annanshogi"),
    Kyotoshogi -> NonEmptyList.of("京都将棋", "京都", "kyotoshogi"),
    Checkshogi -> NonEmptyList.of("王手将棋", "王手", "checkshogi"),
  )

  def toKif(role: Role, variant: Variant): Option[NonEmptyList[String]] =
    variant match {
      case Standard | Annanshogi | Checkshogi =>
        toKifStandard get role
      case Minishogi =>
        toKifMinishogi get role
      case Chushogi =>
        toKifChushogi get role
      case Kyotoshogi =>
        toKifKyotoshogi get role
    }

  def toRole(str: String, variant: Variant): Option[NonEmptyList[Role]] =
    variant match {
      case Standard | Annanshogi | Checkshogi =>
        toRoleStandard get str
      case Minishogi =>
        toRoleMinishogi get str
      case Chushogi =>
        toRoleChushogi get str
      case Kyotoshogi =>
        toRoleKyotoshogi get str
    }

  def anyToRole(str: String, variant: Variant): Option[NonEmptyList[Role]] =
    toRole(str, variant) orElse
      (SfenUtils.toRole(str.toLowerCase, variant) orElse
        CsaUtils.toRole(str).filter(variant.allRoles contains _)).map(NonEmptyList.one(_))

  private val toKifStandard: Map[Role, NonEmptyList[String]] = Map(
    King           -> NonEmptyList.of("玉", "王", "玉将", "王将"),
    Pawn           -> NonEmptyList.of("歩", "兵", "歩兵"),
    Lance          -> NonEmptyList.of("香", "香車"),
    Knight         -> NonEmptyList.of("桂", "桂馬"),
    Silver         -> NonEmptyList.of("銀", "銀将"),
    Gold           -> NonEmptyList.of("金", "金将"),
    Bishop         -> NonEmptyList.of("角", "角行"),
    Rook           -> NonEmptyList.of("飛", "飛車"),
    Tokin          -> NonEmptyList.of("と", "と金", "个"),
    PromotedLance  -> NonEmptyList.of("成香", "杏", "仝"),
    PromotedKnight -> NonEmptyList.of("成桂", "圭", "今"),
    PromotedSilver -> NonEmptyList.of("成銀", "全"),
    Horse          -> NonEmptyList.of("馬", "龍馬", "竜馬"),
    Dragon         -> NonEmptyList.of("龍", "龍王", "龍玉", "竜", "竜王", "竜玉"),
  )

  private val toRoleStandard: Map[String, NonEmptyList[Role]] = toKifStandard flatMap {
    case (r, kifs) =>
      kifs.toList map { k =>
        (k, NonEmptyList.one(r))
      }
  } toMap

  private val toKifMinishogi: Map[Role, NonEmptyList[String]] =
    toKifStandard filter { case (k, _) => Minishogi.allRoles contains k }

  private val toRoleMinishogi: Map[String, NonEmptyList[Role]] =
    toRoleStandard filter { case (_, v) => Minishogi.allRoles contains v.head }

  private[kif] val toKifChushogi: Map[Role, NonEmptyList[String]] =
    Map(
      Bishop           -> NonEmptyList.of("角行", "角"),
      BishopPromoted   -> NonEmptyList.of("小角", "角行", "角", "成角"),
      Boar             -> NonEmptyList.of("奔猪", "猪"),
      Chariot          -> NonEmptyList.of("反車", "反"),
      Copper           -> NonEmptyList.of("銅将", "銅"),
      Dragon           -> NonEmptyList.of("龍王", "龍玉", "竜", "竜王", "竜玉", "龍"),
      DragonPromoted   -> NonEmptyList.of("龍王", "龍玉", "竜", "竜王", "竜玉", "龍", "成竜", "成龍"),
      Eagle            -> NonEmptyList.of("飛鷲", "鷲"),
      Elephant         -> NonEmptyList.of("醉象", "象"),
      ElephantPromoted -> NonEmptyList.of("醉象", "象", "成象"),
      Falcon           -> NonEmptyList.of("角鷹", "鷹"),
      GoBetween        -> NonEmptyList.of("仲人", "仲"),
      Gold             -> NonEmptyList.of("金将", "金"),
      Horse            -> NonEmptyList.of("龍馬", "竜馬", "馬"),
      HorsePromoted    -> NonEmptyList.of("龍馬", "竜馬", "馬", "成馬"),
      King             -> NonEmptyList.of("玉将", "王将", "玉", "王"),
      Kirin            -> NonEmptyList.of("麒麟", "麒"),
      Lance            -> NonEmptyList.of("香車", "香"),
      Leopard          -> NonEmptyList.of("猛豹", "豹"),
      Lion             -> NonEmptyList.of("獅子", "獅", "師"),
      LionPromoted     -> NonEmptyList.of("獅子", "獅", "師", "成獅", "成師"),
      Ox               -> NonEmptyList.of("飛牛", "牛"),
      Pawn             -> NonEmptyList.of("歩兵", "歩", "兵"),
      Prince           -> NonEmptyList.of("太子", "太"),
      Phoenix          -> NonEmptyList.of("鳳凰", "鳳"),
      PromotedPawn     -> NonEmptyList.of(
        "金将",
        "金",
        "と",
        "と金",
        "个",
        "杏",
        "仝",
        "成歩",
        "成兵",
      ), // in chushogi promoted pawn is not really tokin
      Queen                 -> NonEmptyList.of("奔王", "奔"),
      QueenPromoted         -> NonEmptyList.of("奔王", "奔", "成奔"),
      Rook                  -> NonEmptyList.of("飛車", "飛"),
      RookPromoted          -> NonEmptyList.of("金飛車", "飛車", "飛", "成飛"),
      SideMover             -> NonEmptyList.of("横行", "横"),
      SideMoverPromoted     -> NonEmptyList.of("横行", "横", "成横"),
      Silver                -> NonEmptyList.of("銀将", "銀"),
      Stag                  -> NonEmptyList.of("飛鹿", "鹿"),
      Tiger                 -> NonEmptyList.of("盲虎", "虎"),
      VerticalMover         -> NonEmptyList.of("竪行", "竪"),
      VerticalMoverPromoted -> NonEmptyList.of("竪行", "竪", "成竪"),
      Whale                 -> NonEmptyList.of("鯨鯢", "鯨"),
      WhiteHorse            -> NonEmptyList.of("白駒", "駒"),
    )

  private def toRoleChushogiAlt(str: String): Option[NonEmptyList[Role]] = str match {
    case "龍" | "龍王" | "龍玉" | "竜" | "竜王" | "竜玉" => NonEmptyList.of(Dragon, DragonPromoted).some
    case "角" | "角行"                            => NonEmptyList.of(Bishop, BishopPromoted).some
    case "飛" | "飛車"                            => NonEmptyList.of(Rook, RookPromoted).some
    case "竪" | "竪行"        => NonEmptyList.of(VerticalMover, VerticalMoverPromoted).some
    case "横" | "横行"        => NonEmptyList.of(SideMover, SideMoverPromoted).some
    case "奔" | "奔王"        => NonEmptyList.of(Queen, QueenPromoted).some
    case "獅" | "師" | "獅子"  => NonEmptyList.of(Lion, LionPromoted).some
    case "金"               => NonEmptyList.of(Gold, PromotedPawn).some
    case "象" | "醉象"        => NonEmptyList.of(Elephant, ElephantPromoted).some
    case "馬" | "龍馬" | "竜馬" => NonEmptyList.of(Horse, HorsePromoted).some
    case _                 => None
  }

  private val toRoleChushogi: Map[String, NonEmptyList[Role]] =
    toKifChushogi flatMap { case (r, kifs) =>
      kifs.toList map { k =>
        (k, toRoleChushogiAlt(k) | NonEmptyList.of(r))
      }
    } toMap

  private val toKifKyotoshogi: Map[Role, NonEmptyList[String]] =
    toKifStandard filter { case (k, _) => Kyotoshogi.allRoles contains k }

  private val toRoleKyotoshogi: Map[String, NonEmptyList[Role]] =
    toRoleStandard filter { case (_, v) => Kyotoshogi.allRoles contains v.head }

  def toKifBoard(piece: Piece, variant: Variant): Option[String] =
    toKifBoard(piece.role, variant) map { k =>
      if (piece.color.sente) k else s"v$k"
    }

  def toPieceBoard(str: String, variant: Variant): Option[Piece] = {
    val gote    = str.toLowerCase.startsWith("v")
    val roleStr = if (gote) str.drop(1) else str
    anyToRole(roleStr, variant) map { rs =>
      Piece(Color.fromSente(!gote), rs.head)
    }
  }

  private def toKifBoard(role: Role, variant: Variant): Option[String] =
    variant match {
      case Standard | Annanshogi | Checkshogi =>
        toKifBoardStandard get role
      case Minishogi =>
        toKifBoardMinishogi get role
      case Chushogi =>
        toKifBoardChushogi get role
      case Kyotoshogi =>
        toKifBoardKyotoshogi get role
    }

  private val toKifBoardStandard: Map[Role, String] = Map(
    King           -> "玉",
    Pawn           -> "歩",
    Lance          -> "香",
    Knight         -> "桂",
    Silver         -> "銀",
    Gold           -> "金",
    Bishop         -> "角",
    Rook           -> "飛",
    Tokin          -> "と",
    PromotedLance  -> "杏",
    PromotedKnight -> "圭",
    PromotedSilver -> "全",
    Horse          -> "馬",
    Dragon         -> "龍",
  )

  private val toKifBoardMinishogi: Map[Role, String] =
    toKifBoardStandard filter { case (k, _) => Minishogi.allRoles contains k }

  private val toKifBoardChushogi: Map[Role, String] = toKifBoardStandard ++ Map(
    BishopPromoted        -> "成角",
    Boar                  -> "猪",
    Chariot               -> "反",
    Copper                -> "銅",
    DragonPromoted        -> "成龍",
    Eagle                 -> "鷲",
    Elephant              -> "象",
    ElephantPromoted      -> "成象",
    Falcon                -> "鷹",
    GoBetween             -> "仲",
    HorsePromoted         -> "成馬",
    Kirin                 -> "麒",
    Leopard               -> "豹",
    Lion                  -> "獅",
    LionPromoted          -> "成獅",
    Ox                    -> "牛",
    Phoenix               -> "鳳",
    Queen                 -> "奔",
    QueenPromoted         -> "成奔",
    RookPromoted          -> "成飛",
    Prince                -> "太",
    PromotedPawn          -> "杏",
    SideMover             -> "横",
    SideMoverPromoted     -> "成横",
    Stag                  -> "鹿",
    Tiger                 -> "虎",
    VerticalMover         -> "竪",
    VerticalMoverPromoted -> "成竪",
    Whale                 -> "鯨",
    WhiteHorse            -> "駒",
  )

  private val toKifBoardKyotoshogi: Map[Role, String] =
    toKifBoardStandard filter { case (k, _) => Kyotoshogi.allRoles contains k }

  val allKif: List[String] = (Role.all flatMap { r: Role =>
    Variant.all flatMap { v =>
      toKif(r, v).fold[List[String]](Nil)(_.toList)
    }
  }).distinct.sortWith(_.length > _.length)

  val allKifDroppable: List[String] = (Role.allDroppable flatMap { r: Role =>
    Variant.all.withFilter(_.supportsDrops) flatMap { v =>
      toKif(r, v).fold[List[String]](Nil)(_.toList)
    }
  }).distinct.sortWith(_.length > _.length)

}
