package zio.http.docs

import zio.{Chunk, Config, NonEmptyChunk}

import scala.annotation.tailrec

object GenConfigDocs extends App {

  def serverConfigTable =
    genTable(zio.http.Server.Config.config)

  case class Field(name: String, variants: NonEmptyChunk[Config[_]]) {
    def isPrimitive: Boolean =
      variants.forall { c =>
        getLeaf(c).isDefined
      }
  }

  def getLeaf(c: Config[_]): Option[Config[_]] =
    c match {
      case p: Config.Primitive[_]    => Some(p)
      case c: Config.Constant[_]     => Some(c)
      case Config.Lazy(thunk)        => getLeaf(thunk())
      case Config.MapOrFail(orig, _) => getLeaf(orig)
      case Config.Switch(c, map) if map.forall { case (_, c) =>
            getLeaf(c).isDefined
          } =>
        getLeaf(c)
      case _                         => None
    }

  def getFields(c: Config[_], variants: Chunk[Config[_]]): Chunk[Field] = {
    c match {
      case Config.Nested(name, conf) =>
        val innerVariants = NonEmptyChunk.fromChunk(findVariants(conf)).getOrElse(NonEmptyChunk.single(conf))
        val outerVariants = variants.flatMap(findVariants)
        Chunk.single(Field(name, innerVariants ++ outerVariants))

      case Config.Constant(_)               => Chunk.empty
      case Config.Zipped(left, right, _)    => getFields(left, Chunk.empty) ++ getFields(right, Chunk.empty)
      case Config.Lazy(thunk)               => getFields(thunk(), variants)
      case Config.Optional(conf)            => getFields(conf, variants)
      case Config.MapOrFail(orig, _)        => getFields(orig, variants)
      case Config.Fallback(fst, snd)        => getFields(fst, variants :+ snd)
      case Config.FallbackWith(fst, snd, _) => getFields(fst, variants :+ snd)
      case Config.Switch(c, _)              => getFields(c, variants)
    }
  }

  def findVariants(config: Config[_]): Chunk[Config[_]] =
    config match {
      case p: Config.Primitive[_]           => Chunk.single(p)
      case c: Config.Constant[_]            => Chunk.single(c)
      case s: Config.Switch[_, _]           => Chunk.single(s)
      case n: Config.Nested[_]              => Chunk.single(n)
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

  def genTable(value: Config[_]): String = {
    value match {
      case Config.MapOrFail(orig: Config.Zipped[_, _, _], _) =>
        val fields = getFields(orig, Chunk.empty)
//        fields.foreach { f =>
//          println(f.name)
//          println(f.variants.map("  " + _).mkString("\n"))
//        }

        val header = Chunk("Field", "Type", "Default", "Description")
        val body   = fields.map { f =>
          val (tpe, desc) =
            unlazy(f.variants.head, None) match {
              case (p: Config.Primitive[_], maybeDesc) =>
                val t = p.getClass.getSimpleName
                  .replaceAll("(Type)?(\\$)?$", "")

                (t, maybeDesc.getOrElse(""))

              case _ =>
                ("", "")
            }

//          println(s"-----${f.name}------")
          val defaultValue = {
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
                  s"`$str`"

                case None => ""
              }
            } else ""
          }

          Chunk(f.name, tpe, defaultValue, desc)
        }

        val columns = (header +: body).transpose.map { col =>
          val len = col.view.map(_.length).max
          val sep = s"-${"-" * (len + 1)}"
          col.view
            .map(c => s" ${c.padTo(len, ' ')} ")
            .patch(1, Chunk(sep), 0)
            .to(Chunk)
        }

        columns.transpose.view
          .map(_.mkString("|", "|", "|"))
          .mkString("\n")
    }
  }

  zio.config.generateDocs(
    zio.http.Client.Config.config,
  )

  zio.http.netty.NettyConfig.config

  zio.http.DnsResolver.Config.config

}
