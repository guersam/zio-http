package zio.http.docs

import zio.{Chunk, Config, NonEmptyChunk}

import scala.collection.View

case class Row(
  name: String,
  tpe: FieldType,
  description: Option[String] = None,
  optional: Boolean = false,
  default: Option[Any] = None,
)

trait ConfigDoc {

  def toDocusaurusMarkdown: String
}

object ConfigDoc {

  def product(rows: Row*): Product         = Product(Chunk.from(rows))
  def variants(docs: ConfigDoc*): Variants = Variants(Chunk.from(docs))
  case class Product(rows: Chunk[Row]) extends ConfigDoc {

    def toDocusaurusMarkdown: String = {
      val header = Chunk("Field", "Type", "Default", "Description")

      val body: Chunk[Chunk[String]] = rows.map { r =>
        def show(ft: FieldType): String =
          ft match {
            case FieldType.Primitive(t) => t
            case FieldType.Nested(t)    => s"[${r.name}](#${r.name})"
            case FieldType.Sequence(t)  => s"Sequence of ${show(t)}"
            case other                  => other.toString
          }
        val tpe                         = show(r.tpe)

        Chunk(r.name, tpe, r.default.map(_.toString).getOrElse(""), r.description.getOrElse(""))
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
        .mkString("\n") +
        "\n\n" +
        rows.flatMap { r =>
          r.tpe match {
            case FieldType.Nested(t) =>
              Some(s"""### ${r.name}
                      ^
                      ^${t.toDocusaurusMarkdown}
                      ^""".stripMargin('^'))

            case _ => None
          }

        }.mkString("\n\n")
    }
  }
  case class Variants(docs: Chunk[ConfigDoc]) extends ConfigDoc {
    override def toDocusaurusMarkdown: String =
      s"""One of:
         ^
         ^${docs.map(_.toDocusaurusMarkdown).mkString("\n\n")}
         ^""".stripMargin('^')
  }
}

sealed trait FieldType
object FieldType {
  case class Primitive(tpe: String)                       extends FieldType
  case class Enum(tpe: FieldType, mapping: Map[Any, Any]) extends FieldType
  case class Sequence(tpe: FieldType)                     extends FieldType

  case class Constant(value: Any) extends FieldType

  case class Nested(value: ConfigDoc) extends FieldType

}

trait Cursor {
  def config: Config[_]

  def history: List[Cursor]

  def toDocs: ConfigDoc =
    downFallback match {
      case Some(c) => ConfigDoc.Variants(c.getVariants)
      case None    => ConfigDoc.Product(downFields.map(_.toRow))
    }

  def downFallback: Option[Cursor.FallbackCursor] =
    config match {
      case t: Config.Fallback[_]  => Some(Cursor.FallbackCursor(t, history))
      case Config.Lazy(thunk)     => down(thunk()).downFallback
      case Config.MapOrFail(c, _) => down(c).downFallback
      case _                      => None
    }

  def downFields: Chunk[Cursor.FieldCursor] =
    config match {
      case Config.Nested(name, c)        => Chunk(Cursor.FieldCursor(name, c, history))
      case Config.Lazy(thunk)            => down(thunk()).downFields // TODO recursive
      case Config.MapOrFail(c, _)        => down(c).downFields
      case Config.Described(c, _)        => down(c).downFields
      case Config.Optional(c)            => down(c).downFields
      case Config.Zipped(left, right, _) => down(left).downFields ++ down(right).downFields
      case Config.Switch(c, _)           => down(c).downFields

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
      case Config.Switch(c, _)    => down(c).downPrimitive
      case _                      => None
    }
  }

  def downSwitch: Option[Cursor.SwitchCursor] =
    config match {
      case c: Config.Switch[_, _] => Some(Cursor.SwitchCursor(c, history))
      case Config.Lazy(thunk)     => down(thunk()).downSwitch // TODO recursive
      case Config.MapOrFail(c, _) => down(c).downSwitch
      case _                      => None
    }

  def upUntilSiblings: View[Cursor] =
    history.view.takeWhile(_.config match {
      case _: Config.Zipped[_, _, _] => false
      case _: Config.Nested[_]       => false
      case _                         => true
    })

  def upFallback: Option[Cursor.FallbackCursor] =
    upUntilSiblings.collectFirst {
      case c if c.config.isInstanceOf[Config.Fallback[_]] =>
        Cursor.FallbackCursor(c.config.asInstanceOf[Config.Fallback[_]], c.history)
    }

  def upSwitch: Option[Cursor.SwitchCursor] =
    upUntilSiblings.collectFirst {
      case c if c.config.isInstanceOf[Config.Switch[_, _]] =>
        Cursor.SwitchCursor(c.config.asInstanceOf[Config.Switch[_, _]], c.history)
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

      val tpe =
        upSwitch
          .flatMap(_.getEnum(Some(config)))
          .orElse(downSwitch.flatMap(_.getEnum(None)))
          .orElse(downPrimitive.map(_.getType))
          .getOrElse(FieldType.Nested(this.toDocs))

      Row(name = name, tpe = tpe, description = getDescriptions.headOption, optional = optional, default = default)
    }

  }

  case class PrimitiveCursor(config: Config.Primitive[_], history: List[Cursor]) extends Cursor {
    def getType: FieldType = config match {
      case Config.Constant(v) => FieldType.Constant(v)
      case other              => FieldType.Primitive(other.toString.replaceAll("Type$", ""))
    }
  }

  case class ConstantCursor(config: Config.Constant[_], history: List[Cursor]) extends Cursor {
    def getValue: Any = config.value
  }

  case class FallbackCursor(config: Config.Fallback[_], history: List[Cursor]) extends Cursor {

    def getVariants: Chunk[ConfigDoc] =
      Chunk(down(config.first).toDocs, down(config.second).toDocs).flatMap {
        case ConfigDoc.Variants(docs) => docs
        case doc                      => Chunk.single(doc)
      }

    def getDefault: Option[Any] =
      config match {
        case Config.FallbackWith(_, v, cond) if cond(missingOnly) =>
          Cursor.fromConfig(v).downConstant.map(_.getValue)

        case _ => None
      }

    private val missingOnly = Config.Error.MissingData(Chunk.empty, "")
  }

  case class SwitchCursor(switch: Config.Switch[_, _], history: List[Cursor]) extends Cursor {

    // Compiler cannot figure out `Config.Switch[_, _]` is `Config[_]`
    override def config: Config[_] = switch

    def getEnum(overrideConfig: Option[Config[_]]): Option[FieldType.Enum] =
      down(overrideConfig.getOrElse(switch.config)).downPrimitive.flatMap { c =>
        val constMap = switch.map.view
          .mapValues(down(_).downConstant)
          .collect { case (k, Some(const)) =>
            k -> const.getValue
          }
          .toMap

        if (constMap.size == switch.map.size)
          Some(FieldType.Enum(c.getType, constMap))
        else
          None
      }
  }

  def fromConfig(config: Config[_]): Cursor = GenericCursor(config, Nil)

}

object GenConfigDocs2 {

  def gen(config: Config[_]): ConfigDoc = {
    Cursor.fromConfig(config).toDocs
  }

}
