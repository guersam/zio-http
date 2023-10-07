package zio.http.docs

import zio.{Chunk, Config, NonEmptyChunk}

import scala.annotation.tailrec

object GenConfigDocs extends App {

  def allConfigs = {
    val tables =
      Chunk(
        "Server"       -> zio.http.Server.Config.config,
        "Client"       -> zio.http.Client.Config.config,
        "Netty"        -> zio.http.netty.NettyConfig.config,
        "DNS Resolver" -> zio.http.DnsResolver.Config.config,
      ).map { case (title, config) =>
        val rows = genTableRows(config)
        val id   = title.toLowerCase.replaceAll("\\s+", "-")
        Table(config, 2, title, id, rows)
      }

    var subtableMap = Map.empty[Config[_], Table]

    tables.view.map { t =>
      s"""${t.render}
         ^
         ^${t.subtables.view.map { st =>
          if (subtableMap.contains(st.rootConfig)) {
            s"""${st.renderTitleOnly}
               |
               |See [${st.title}](#${st.id})
               |""".stripMargin
          } else {
            subtableMap = subtableMap.updated(st.rootConfig, st)
            st.render
          }
        }.mkString("\n")}
         ^""".stripMargin('^')
    }.mkString("\n\n")
  }

  def serverConfigTable =
    genTable(zio.http.Server.Config.config)

  case class Field(name: String, variants: NonEmptyChunk[Config[_]]) {
    def isPrimitive: Boolean =
      variants.forall { c =>
        getPrimitiveOrConstantLeaf(c).isDefined
      }
  }

  def getPrimitiveOrConstantLeaf(config: Config[_]): Option[Config[_]] =
    config match {
      case p: Config.Primitive[_]    => Some(p)
      case c: Config.Constant[_]     => Some(c)
      case Config.Lazy(thunk)        => getPrimitiveOrConstantLeaf(thunk())
      case Config.MapOrFail(orig, _) => getPrimitiveOrConstantLeaf(orig)
      case Config.Nested(_, c)       => getPrimitiveOrConstantLeaf(c)
      case Config.Switch(c, map) if map.forall { case (_, c) =>
            getPrimitiveOrConstantLeaf(c).exists(_.isInstanceOf[Config.Constant[_]])
          } =>
        getPrimitiveOrConstantLeaf(c)
      case _                         => None
    }

  def getFields(config: Config[_], variants: Chunk[Config[_]]): Chunk[Field] = {
    config match {
      case Config.Nested(name, conf) =>
        val innerVariants = NonEmptyChunk.fromChunk(findVariants(conf)).getOrElse(NonEmptyChunk.single(conf))
        val outerVariants = variants.flatMap(findVariants)
        Chunk.single(Field(name, innerVariants ++ outerVariants))

      case _: Config.Primitive[_]           => Chunk.empty
      case _: Config.Sequence[_]            => Chunk.empty
      case _: Config.Table[_]               => Chunk.empty
      case Config.Zipped(left, right, _)    => getFields(left, Chunk.empty) ++ getFields(right, Chunk.empty)
      case Config.Lazy(thunk)               => getFields(thunk(), variants)
      case Config.Optional(conf)            => getFields(conf, variants)
      case Config.MapOrFail(orig, _)        => getFields(orig, variants)
      case Config.Fallback(fst, snd)        => getFields(fst, variants :+ snd)
      case Config.FallbackWith(fst, snd, _) => getFields(fst, variants :+ snd)
      case Config.Switch(c, map)            =>
        unlazy(c, None) match {
          case (Config.Nested(name, inner), _) =>
            val innerSwitch   = Config.Switch(inner, map)
            val innerVariants =
              NonEmptyChunk
                .fromChunk(findVariants(innerSwitch))
                .getOrElse(NonEmptyChunk.single(innerSwitch))

            val outerVariants = variants.flatMap(findVariants)
            Chunk.single(Field(name, innerVariants ++ outerVariants))

          case _ => getFields(c, variants)
        }
    }
  }

  def findVariants(config: Config[_]): Chunk[Config[_]] =
    config match {
      case p: Config.Primitive[_]           => Chunk.single(p)
      case c: Config.Constant[_]            => Chunk.single(c)
      case s: Config.Switch[_, _]           => Chunk.single(s)
      case n: Config.Nested[_]              => Chunk.single(n)
      case s: Config.Sequence[_]            => Chunk.single(s)
      case Config.Zipped(_, _, _)           => Chunk.empty
      case Config.Fallback(fst, snd)        => findVariants(fst) ++ findVariants(snd)
      case Config.FallbackWith(fst, snd, _) => findVariants(fst) ++ findVariants(snd)
      case Config.Described(c, _)           => findVariants(c)
      case Config.Lazy(thunk)               => findVariants(thunk())
      case Config.MapOrFail(orig, _)        => findVariants(orig)
    }

  @tailrec
  def unlazy(c: Config[_], desc: Option[String]): (Config[_], Option[String]) =
    c match {
      case Config.Described(c, d)    => unlazy(c, desc.orElse(Some(d)))
      case Config.Lazy(thunk)        => unlazy(thunk(), desc)
      case Config.MapOrFail(orig, _) => unlazy(orig, desc)
      case _                         => (c, desc)
    }

  private val missing = Config.Error.MissingData(message = "dummy")

  @tailrec
  def default(config: Config[_]): Option[Any] = {
    config match {
      case Config.Constant(Some(v))                           => Some(v)
      case Config.Constant(v)                                 => Some(v)
      case Config.Lazy(thunk)                                 => default(thunk())
      case Config.MapOrFail(orig, _)                          => default(orig)
      case Config.Described(c, _)                             => default(c)
      case Config.Fallback(_, snd)                            => default(snd)
      case Config.FallbackWith(_, snd, cond) if cond(missing) => default(snd)
      case Config.Switch(c, _)                                => default(c)
      case _                                                  => None
    }
  }

  @tailrec
  def findConstantValue(config: Config[_]): Option[Any] =
    config match {
      case Config.Constant(v)     => Some(v)
      case Config.Described(c, _) => findConstantValue(c)
      case Config.Lazy(thunk)     => findConstantValue(thunk())
      case _                      => None
    }

  case class TableRow(name: String, tpe: FieldType, default: Option[String], description: Option[String]) {
    def subtables: Chunk[Table] =
      tpe match {
        case FieldType.Primitive(_) => Chunk.empty
        case FieldType.Reference(t) => t +: t.subtables
      }
  }

  sealed trait FieldType
  case object FieldType {
    case class Primitive(str: String) extends FieldType

    // TODO recursive
    case class Reference(other: Table) extends FieldType
  }

  case class Table(rootConfig: Config[_], level: Int, title: String, id: String, rows: Chunk[TableRow]) {

    def subtables: Chunk[Table] = {
      rows.flatMap(_.subtables)
    }

    def renderTitleOnly: String = s"${"#" * level} $title"

    def render: String =
      s"""$renderTitleOnly {#$id}
         ^
         ^${renderTableRows(rows)}
         ^""".stripMargin('^')
  }

  def renderTableRows(rows: Chunk[TableRow]): String = {
    val header = Chunk("Field", "Type", "Default", "Description")

    val body: Chunk[Chunk[String]] = rows.map { r =>
      val tpe = r.tpe match {
        case FieldType.Primitive(t) => t
        case FieldType.Reference(l) =>
          s"[${l.id}](#${l.id})"
      }
      Chunk(r.name, tpe, r.default.getOrElse(""), r.description.getOrElse(""))
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

  def genTable(value: Config[_]): String =
    renderTableRows(genTableRows(value))

  def genTableRows(value: Config[_]): Chunk[TableRow] = {
    val orig =
      value match {
        case c: Config.Zipped[_, _, _]                         => c
        case Config.MapOrFail(orig: Config.Zipped[_, _, _], _) => orig
        case _                                                 => value
      }

    val fields = getFields(orig, Chunk.empty)
//        fields.foreach { f =>
//          println(f.name)
//          println(f.variants.map("  " + _).mkString("\n"))
//        }

    fields.map { f =>
      val (tpe, desc) =
        unlazy(f.variants.head, None) match {
          case (p: Config.Primitive[_], maybeDesc) =>
            val t = p.getClass.getSimpleName
              .replaceAll("(Type)?(\\$)?$", "")

            (FieldType.Primitive(t), maybeDesc)

          case (c, _) =>
            val rows     = genTableRows(c)
            val subtable = Table(c, 3, f.name, f.name, rows)
            println(subtable.render)
            (FieldType.Reference(subtable), None)
        }

//          println(s"-----${f.name}------")
      val defaultValue: Option[String] =
        if (f.isPrimitive) {
          f.variants.tail.lastOption.flatMap(default) match {
            case Some(dv) =>
              val str = f.variants.head match {
                case Config.Switch(_, map) =>
                  map.collectFirst {
                    case (key, c) if findConstantValue(c).contains(dv) =>
                      key.toString
                  }.getOrElse(dv.toString)

                case _ => dv.toString
              }
              Some(s"`$str`")

            case None => None
          }
        } else None

      TableRow(f.name, tpe, defaultValue, desc)
    }
  }

}
