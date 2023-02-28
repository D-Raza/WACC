package wacc.backend

import scala.collection.mutable

object Labels {

  case class Data(s: String, labelIndex: String, actualSize: Int) {
    val label: String = s"msg_$labelIndex"
    val instruction: mutable.ListBuffer[Instruction] =
      mutable.ListBuffer(
        Label(label),
        Directive(s"word $actualSize"),
        Directive(s"ascii " + "\"" + s + "\""))
  }

  val dataMap: mutable.LinkedHashMap[String, Data] = mutable.LinkedHashMap().empty
  private var dataCnt = 0

  def addDataMsg(s: String): String = {
    val result = addDataMsgWithLabel(s, dataCnt.toString)
    dataCnt += 1
    result
  }

  private def addDataMsgWithLabel(s: String, label: String): String = {
    // Get the real length of the string before additional escape chars are added
    val addedEscape  = s.map(escapeCharToString).mkString
    val msg = Data(addedEscape, label, s.length())
    dataMap.get(addedEscape) match {
      case None =>
        dataMap.put(addedEscape, msg)
        msg.label
      case Some(v) =>
        v.label
    }
  }

  private def escapeCharToString(c: Char): String = {
    c match {
        case '\u0000' => "\\0"
        case '\b' => "\\b"
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\f' => "\\f"
        case '\r' => "\\r"
        case '\"' => "\\\""
        case '\'' => "\\\'"
        case '\\' => "\\\\"
        case _ => c.toString
    }
  }

  // TODO:
  // Deal with if/while messages
}
