package zio.http.docs
import mdoc.Reporter

import scala.meta.inputs.Input

class ConfigModifier extends mdoc.StringModifier {
  override val name: String = "zio-http-config"

  override def process(info: String, code: Input, reporter: Reporter): String = {
    GenConfigDocs.allConfigs
  }

}
