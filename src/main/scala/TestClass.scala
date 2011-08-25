package org.improving

import javax.xml.bind.annotation._

case class Entry(tag: Int, bytes: Array[Byte])

class TestClass1 {
  def met(param1: Long) = "method-1"
  def met(param1: Long = 445, param2: String) = "method-2"
}

@XmlType
class TestClass2 {
  @XmlAttribute
  val f1 = 150.75
}
