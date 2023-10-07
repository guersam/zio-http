package zio.http.docs

import zio.{Chunk, Config}

import scala.collection.View

case class Row(name: String, tpe: String, description: Option[String] = None, optional: Boolean = false)

case class Table(rows: Chunk[Row]) {
  def toDocusaurusMarkdown: String = {
    val header = Chunk("Field", "Type")

    val body: Chunk[Chunk[String]] = rows.map { r =>
      val tpe = r.tpe match {
        case t: String => t
      }
      Chunk(r.name, tpe)
    }

    val columns = (header +: body).transpose.map { col =>
      val len = col.view.map(_.length).max
      val sep = s"-${"-" * (len + 1)}"
      col.view
        .map(c => s" ${c.padTo(len, ' ')} ")
        .patch(1, Chunk(sep), 0)
        .to(Chunk)
    }

    columns.transpose
      .map(_.mkString("|", "|", "|"))
      .mkString("\n")
  }
}

trait Cursor {
  def config: Config[_]

  def history: List[Cursor]

  def downFields: Chunk[Cursor.FieldCursor] =
    config match {
      case Config.Nested(name, c)        => Chunk(Cursor.FieldCursor(name, c, history))
      case Config.Lazy(thunk)            => Cursor.GenericCursor(thunk(), this :: history).downFields // TODO recursive
      case Config.MapOrFail(c, _)        => Cursor.GenericCursor(c, this :: history).downFields
      case Config.Described(c, _)        => Cursor.GenericCursor(c, this :: history).downFields
      case Config.Optional(c)            => Cursor.GenericCursor(c, this :: history).downFields
      case Config.Zipped(left, right, _) =>
        Cursor.GenericCursor(left, this :: history).downFields ++
          Cursor.GenericCursor(right, this :: history).downFields

      case _: Config.Primitive[_] => Chunk.empty
      case _                      => Chunk.empty
    }

  def showConfig: String =
    config.toString
      .replaceAll("""zio\.[a-zA-Z0-9.$]+\$Lambda[a-z0-9/@$]+""", "Lambda(...)")
      .replaceAll("""zio\.ZippableLowPriority\d*\$\$(Lambda|anon)[a-z0-9/@$]+""", "Zippable(...)")

  def downPrimitive: Option[Cursor.PrimitiveCursor] = {
//    println(s"downPrimitive: $show")
    config match {
      case p: Config.Primitive[_] => Some(Cursor.PrimitiveCursor(p, history))
      case Config.Lazy(thunk)     => Cursor.GenericCursor(thunk(), this :: history).downPrimitive // TODO recursive
      case Config.MapOrFail(c, _) => Cursor.GenericCursor(c, this :: history).downPrimitive
      case _                      => None
    }
  }

  def isZipped: Boolean = config match {
    case Config.Zipped(_, _, _) => true
    case _                      => false
  }

  def isOptional: Boolean = config match {
    case Config.Optional(_) => true
    case _                  => false
  }

  def upUntilSiblings: View[Cursor] =
    history.view.takeWhile(!_.isZipped)

  def descriptions: Chunk[String] =
    upUntilSiblings
      .map(_.config)
      .collect { case Config.Described(_, desc) =>
        desc
      }
      .to(Chunk)

  def toTable: Table = Table(downFields.map(_.toRow))
}

object Cursor {

  case class GenericCursor(config: Config[_], history: List[Cursor]) extends Cursor

  case class FieldCursor(name: String, config: Config[_], history: List[Cursor]) extends Cursor {

    def toRow: Row = {
      val optional = upUntilSiblings.exists(_.isOptional)

      Row(name = name, tpe = tpe, description = descriptions.headOption, optional = optional)
    }

    def tpe: String =
      this.downPrimitive match {
        case Some(p) => p.tpe
        case None    => "Unknown"
      }

  }

  case class PrimitiveCursor(config: Config.Primitive[_], history: List[Cursor]) extends Cursor {
    def tpe: String = config match {
      case Config.Constant(_) => "Constant"
      case other              => other.toString.replaceAll("Type$", "")
    }
  }

  def fromConfig(config: Config[_]): Cursor = GenericCursor(config, Nil)

}

object GenConfigDocs2 {

  def gen(config: Config[_]): Table = {
    Cursor.fromConfig(config).toTable
  }

}
