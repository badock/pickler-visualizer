package aa

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

class J1 {
  def f[K, V](x: Map[K, V]): Map[K, V] = error("")
}
class J2 {
  def f[K, K1 <: K, V](x: Map[K1, V]): Map[K, V] = error("")
}
class J3 {
  def f[K, K1 >: K, V](x: Map[K1, V]): Map[K, V] = error("")
}

class J4[TypeParam] {
  def f[MethodParam](x: TypeParam)(implicit y: MethodParam) = ((x, y))
}

trait Base[@specialized T] { }
class Sub[@specialized T](x: T) extends Base[T] { }

class B1 {
  lazy val foo = 123
  def bar = foo
}
class B2(val foo: Int) {
  class foo {
    val foo = "abc"
  }
}
