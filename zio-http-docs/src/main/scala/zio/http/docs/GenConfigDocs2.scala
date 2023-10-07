package zio.http.docs

import zio.{Chunk, Config}

import scala.collection.View

case class Row(
  name: String,
  tpe: String,
  description: Option[String] = None,
  optional: Boolean = false,
  default: Option[Any] = None,
)

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

  def toTable: Table = Table(downFields.map(_.toRow))

  def downFields: Chunk[Cursor.FieldCursor] =
    config match {
      case Config.Nested(name, c)        => Chunk(Cursor.FieldCursor(name, c, history))
      case Config.Lazy(thunk)            => down(thunk()).downFields // TODO recursive
      case Config.MapOrFail(c, _)        => down(c).downFields
      case Config.Described(c, _)        => down(c).downFields
      case Config.Optional(c)            => down(c).downFields
      case Config.Zipped(left, right, _) => down(left).downFields ++ down(right).downFields

      case f: Config.Fallback[_] =>
        // Second will be matched by `upFallback` later
        down(f.first).downFields

      case _: Config.Primitive[_] => Chunk.empty
      case _                      => Chunk.empty
    }

  def downPrimitive: Option[Cursor.PrimitiveCursor] = {
//    println(s"downPrimitive: $show")
    config match {
      case p: Config.Primitive[_] => Some(Cursor.PrimitiveCursor(p, history))
      case Config.Lazy(thunk)     => down(thunk()).downPrimitive // TODO recursive
      case Config.MapOrFail(c, _) => down(c).downPrimitive
      case _                      => None
    }
  }

  def upUntilSiblings: View[Cursor] =
    history.view.takeWhile(!_.isZipped)

  def upFallback: Option[Cursor.FallbackCursor] =
    upUntilSiblings.collectFirst {
      case c if c.config.isInstanceOf[Config.Fallback[_]] =>
        Cursor.FallbackCursor(c.config.asInstanceOf[Config.Fallback[_]], c.history)
    }

  def downConstant: Option[Cursor.ConstantCursor] =
    config match {
      case c: Config.Constant[_] => Some(Cursor.ConstantCursor(c, history))
      case Config.Lazy(thunk)    => down(thunk()).downConstant
      case _                     => None
    }

  protected def down(c: Config[_]): Cursor =
    Cursor.GenericCursor(c, this :: history)

  def getDescriptions: Chunk[String] =
    upUntilSiblings
      .map(_.config)
      .collect { case Config.Described(_, desc) =>
        desc
      }
      .to(Chunk)

  def isZipped: Boolean = config match {
    case Config.Zipped(_, _, _) => true
    case _                      => false
  }

  def isOptional: Boolean = config match {
    case Config.Optional(_) => true
    case _                  => false
  }

  def showConfig: String =
    config.toString
      .replaceAll("""zio\.[a-zA-Z0-9.$]+\$Lambda[a-z0-9/@$]+""", "Lambda(...)")
      .replaceAll("""zio\.ZippableLowPriority\d*\$\$(Lambda|anon)[a-z0-9/@$]+""", "Zippable(...)")

}

object Cursor {

  case class GenericCursor(config: Config[_], history: List[Cursor]) extends Cursor

  case class FieldCursor(name: String, config: Config[_], history: List[Cursor]) extends Cursor {

    def toRow: Row = {
      val optional = upUntilSiblings.exists(_.isOptional)
      val default  = upFallback.flatMap(_.getDefault)
      val tpe      = downPrimitive.map(_.getType).getOrElse("Unknown")

      Row(name = name, tpe = tpe, description = getDescriptions.headOption, optional = optional, default = default)
    }

  }

  case class PrimitiveCursor(config: Config.Primitive[_], history: List[Cursor]) extends Cursor {
    def getType: String = config match {
      case Config.Constant(_) => "Constant"
      case other              => other.toString.replaceAll("Type$", "")
    }
  }

  case class FallbackCursor(config: Config.Fallback[_], history: List[Cursor]) extends Cursor {

    def getDefault: Option[Any] =
      config match {
        case Config.FallbackWith(_, v, cond) if cond(missingOnly) =>
          Cursor.fromConfig(v).downConstant.map(_.getValue)

        case _ => None
      }

    private val missingOnly = Config.Error.MissingData(Chunk.empty, "")
  }

  case class ConstantCursor(config: Config.Constant[_], history: List[Cursor]) extends Cursor {
    def getValue: Any = config.value
  }

  def fromConfig(config: Config[_]): Cursor = GenericCursor(config, Nil)

}

object GenConfigDocs2 {

  def gen(config: Config[_]): Table = {
    Cursor.fromConfig(config).toTable
  }

}
