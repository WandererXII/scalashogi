package shogi

case class Piece(color: Color, role: Role) {

  def is(c: Color)   = c == color
  def is(r: Role)    = r == role
  def isNot(r: Role) = r != role

  def directDirs     = if (color.sente) role.senteDirectDirs else role.goteDirectDirs
  def projectionDirs = if (color.sente) role.senteProjectionDirs else role.goteProjectionDirs

  def eyes(from: Pos, to: Pos) =
    if (color.sente) role.senteEyes(from, to) else role.goteEyes(from, to)

  def updateRole(f: Role => Option[Role]): Option[Piece] =
    f(role).map(r => copy(role = r))

  def switch = copy(color = !color)

  override def toString = s"$color-$role".toLowerCase
}
