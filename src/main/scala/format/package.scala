package shogi

package object format {

  private[format] val fieldSeparator = """\s+""".r

  private[format] val noDoubleLineBreakRegex = "(?:\r?\n){2,}+".r

}
