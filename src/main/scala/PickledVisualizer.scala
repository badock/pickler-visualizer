package org.improving

import java.io.{ File, PrintWriter }
import java.util.Arrays
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble
import scala.collection.{ mutable, immutable }
import scala.reflect.internal.pickling._
import scala.reflect.internal.Flags
import scala.tools.nsc.util.ShowPickled
import scala.sys.process._
import PickleFormat._

object PickleView {
  def loadFromClassPath(name: String) {
    println("Processing class: " + name)
    val clazz: Class[_] = getClass.getClassLoader.loadClass(name)
    new PickleProcessor(clazz).process()
  }

  def runDemo() {
    println("Running demo...")
    (new PickleProcessor(classOf[TestClass1])).process()
    (new PickleProcessor(classOf[TestClass2])).process()
  }

  def main(args: Array[String]) {
    if (args.isEmpty) runDemo
    else args foreach loadFromClassPath
    sys.exit(0)
  }
}

class PickleProcessor(clazz: Class[_]) {
  val clazzName = clazz.getSimpleName
  
  def this(name: String) = this (ClassLoader.getSystemClassLoader().loadClass(name))

  val defaultColor = "navajowhite"
  val symColor     = "firebrick"
  val refColor     = "darkgoldenrod1"
  val tpeColor     = "plum2"
  val annotColor   = "coral"
  val literalColor = "chartreuse"
  
  val typeFont = "/Users/paulp/Library/Fonts/Consolas Italic.ttf"
  val termFont = "/Users/paulp/Library/Fonts/Consolas.ttf"

  val bytes: Array[Byte] = {
    val scalaSigAnnot = clazz.getAnnotation(classOf[scala.reflect.ScalaSignature])
    val encodedBytes  = scalaSigAnnot.bytes.getBytes
    val len           = ByteCodecs.decode(encodedBytes)

    Arrays.copyOf(encodedBytes, len)
  }

  private var w: PrintWriter = _
  
  def withWriter[T](writer: PrintWriter)(body: => T): T = {
    w = writer
    try body
    finally { w.close() ; w = null }
  }

  val buf = new PickleBuffer(bytes, 0, bytes.length)

  val index: Array[Int] = {
    val version = buf.readNat() + "." + buf.readNat()
    val i = buf.createIndex
    buf.readIndex = 0
    i
  }

  val visited = new Array[Boolean](index.length)
  val indexToName = mutable.Map[Int, String]()

  def seekToPos(pos: Int) {
    buf.readIndex = pos;
  }
  
  private def q(s: String) = "\"" + s + "\""
  def mkLine(label: String, pairs: Tuple2[_,_]*): String = {
    pairs map { case (k, v) => k + "=" + v } mkString (label + " [", ", ", "];")
  }
  
  def graphLine(pairs: Tuple2[_,_]*): String = mkLine("graph", pairs: _*)
  def nodeLine(pairs: Tuple2[_,_]*): String  = mkLine("node", pairs: _*)
  def digraph(body: => Unit) {
    w println "digraph {"
    body
    w println "}"
  }

  def process(outputExtension: String = "pdf") {
    val dir = new File("target/dot")
    dir.mkdirs()
    val dotFile = new File(dir, clazzName + ".dot")
    val outFile = new File(dir, clazzName + "." + outputExtension)
    println("Generating " + outFile + "...")

    withWriter(new PrintWriter(dotFile)) {
      digraph {
        w println graphLine("label" -> q(clazzName), "concentrate" -> true)
        w println nodeLine("shape" -> "box", "style" -> "filled", "color" -> defaultColor)
        0 until index.size foreach processEntry
      }
    }

    (
          ("dot -Tpdf -o " + outFile + " " + dotFile)
      #&& ("test -f /usr/bin/open")
      #&& ("/usr/bin/open " + outFile)
    ).!!
  }

  def processEntry(i: Int) {
    if (!visited(i)) {
      visited(i) = true

      seekToPos(index(i))
      val tag = buf.readByte
      val len = buf.readNat

      processEntry(tag, len, i)
    }
  }
  
  def tag2string(tag: Int): String = tag match {
    case TERMname       => "TermName"
    case TYPEname       => "TypeName"
    case NONEsym        => "NoSymbol"
    case TYPEsym        => "type"
    case ALIASsym       => "alias"
    case CLASSsym       => "class"
    case MODULEsym      => "object"
    case VALsym         => "value"
    case EXTref         => "Ext"
    case EXTMODCLASSref => "ExtModule"
    case NOtpe          => "NoType"
    case NOPREFIXtpe    => "NoPrefix"
    case THIStpe        => "ThisType"
    case SINGLEtpe      => "SingleType"
    case CONSTANTtpe    => "ConstantType"
    case TYPEREFtpe     => "TypeRef"
    case TYPEBOUNDStpe  => "TypeBounds"
    case REFINEDtpe     => "RefinedType"
    case CLASSINFOtpe   => "ClassInfoType"
    case METHODtpe      => "MethodType"
    case POLYtpe        => "PolyType"
    case IMPLICITMETHODtpe => "MethodType" // IMPLICITMETHODtpe no longer used.
    case SUPERtpe       => "SuperType"
    case LITERALunit    => "()"
    case LITERALboolean => "Boolean"
    case LITERALbyte    => "Byte"
    case LITERALshort   => "Short"
    case LITERALchar    => "Char"
    case LITERALint     => "Int"
    case LITERALlong    => "Long"
    case LITERALfloat   => "Float"
    case LITERALdouble  => "Double"
    case LITERALstring  => "String"
    case LITERALnull    => "null"
    case LITERALclass   => "Class[_]"
    case LITERALenum    => "Enum"
    case SYMANNOT       => "Annotation"
    case CHILDREN       => "Children"
    case ANNOTATEDtpe   => "AnnotatedType"
    case ANNOTINFO      => "AnnotationInfo"
    case ANNOTARGARRAY  => "AnnotArgArray"
    case EXISTENTIALtpe => "ExistentialType"
    case TREE           => "Tree"
    case MODIFIERS      => "Modifiers"
        
    case _ => "***BAD TAG***(" + tag + ")"
  }
  

  def processEntry(tag: Int, len: Int, i: Int) {
    val end = buf.readIndex + len
    val tn  = tag2string(tag)

    tag match {
      case (TERMname | TYPEname) =>
        indexToName(i) = readNameInfo(end)
        
      case (TYPEsym | ALIASsym | MODULEsym) =>
        processSymbolInfo(i, tn, end)
      case CLASSsym =>
        processSymbolInfo(i, tn, end)
        if (buf.readIndex < end) {processRef(i, "thisType")}
      case VALsym =>
        processSymbolInfo(i, tn, end)
        if (buf.readIndex < end) {processRef(i, "alias")}
      case NONEsym =>
        printNodeInfo(i, tn, symColor)
      case EXTref =>
        val refName = readNameRef()
        printNodeInfo(i, "class " + refName, refColor)
        if (buf.readIndex < end)
          processOwnerRef(i)
          
      case EXTMODCLASSref =>
        val refName = readNameRef()
        printNodeInfo(i, "module " + refName, refColor)
        if (buf.readIndex < end)
          processOwnerRef(i)

      case THIStpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "sym")
      case SINGLEtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe")
        processRef(i, "sym")
      case CONSTANTtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "const")
      case (TYPEBOUNDStpe | SUPERtpe) =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "lo")
        processRef(i, "hi")
      case TYPEREFtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "pre")
        processRef(i, "sym")
        processListRef(i, end, "args")
      case (REFINEDtpe | CLASSINFOtpe) =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "classSym")
        processListRef(i, end, "tpe")
      case (METHODtpe | POLYtpe | IMPLICITMETHODtpe) =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe")
        processListRef(i, end, "sym")
      case ANNOTATEDtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe")
        processListRef(i, end, "annotInfo")
      case EXISTENTIALtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe")
        processListRef(i, end, "sym")
      case LITERALboolean =>
        val v = if (buf.readLong(len) == 0L) "false" else "true"
        printLiteralNodeInfo(i, v)
      case LITERALbyte =>
        printLiteralNodeInfo(i, buf.readLong(len).toByte + ": " + tn)
      case LITERALshort =>
        printNodeInfo(i, buf.readLong(len).toShort + ": " + tn)
      case LITERALchar =>
        printLiteralNodeInfo(i, buf.readLong(len).toChar + ": " + tn)
      case LITERALint =>
        printLiteralNodeInfo(i, "" + buf.readLong(len).toInt)
      case LITERALlong =>
        printLiteralNodeInfo(i, buf.readLong(len) + "L")
      case LITERALfloat =>
        printLiteralNodeInfo(i, intBitsToFloat(buf.readLong(len).toInt) + "f")
      case LITERALdouble =>
        printLiteralNodeInfo(i, longBitsToDouble(buf.readLong(len)) + "d")
      case LITERALstring =>
        printNodeInfo(i, readNameRef(), literalColor)
      case LITERALnull =>
        printNodeInfo(i, tn, literalColor)
      case LITERALclass =>
        printNodeInfo(i, tn, literalColor)
        processRef(i, "tpe")
      case LITERALenum =>
        printNodeInfo(i, tn, literalColor)
        processRef(i, "sym")
      case SYMANNOT =>
        processRef(i, "sym")
        processAnnotInfoBody(i, tn, end)
      case CHILDREN =>
        printNodeInfo(i, tn, annotColor)
      case ANNOTINFO =>
        processAnnotInfoBody(i, tn, end)
      case ANNOTARGARRAY =>
        printNodeInfo(i, tn, annotColor)
        processListRef(i, end, "constAnnotArg")
      case _ =>
        printNodeInfo(i, tn)
    }
  }

  def printNodeInfo(i: Int, label: String) {
    w.println(i + " [label=\"" + label + "\"];")
  }

  def printNodeInfo(i: Int, label: String, color: String) {
    w.println(i + " [label=\"" + label + "\", color=\"" + color + "\"];")
  }

  def printLiteralNodeInfo(i: Int, value: String) {
    printNodeInfo(i, value, literalColor)
  }

  def readNameInfo(end: Int) = new String(buf.bytes.slice(buf.readIndex, end))
  
  private def compose(xs: Any*): String = xs map ("" + _) filterNot (_ == "") mkString " "

  def processSymbolInfo(i: Int, tag: String, end: Int) {
    val pos = buf.readIndex
    val nameRef = readNameRef()
    processOwnerRef(i)

    val flagLongNat = Flags.rawFlagsToPickled(buf.readLongNat)
    val hasFlag0    = 0 to 62 map (1L << _) filter (f => (flagLongNat & f) != 0L)
    val isParam     = hasFlag0 contains Flags.PARAM
    val isTParam    = isParam && (tag == "type")
    val isImplicit  = hasFlag0 contains Flags.IMPLICIT
    val filtFlags   = (hasFlag0 filterNot Set(Flags.PARAM, Flags.IMPLICIT)).foldLeft(0L)(_ & _)
    val flagString  = Flags.flagsToString(filtFlags)

    // Handle optional [privateWithin_Ref]
    val nextIdx = buf.readNat
    val (privateWithinIdx, infoIdx) = (
      if (buf.readIndex == end) (-1, nextIdx)
      else (nextIdx, buf.readNat)
    )
    val description = compose(
      flagString,
      if (isImplicit) "implicit" else "",
      if (isTParam) "tparam" else if (isParam) "param" else "",
      if (tag == "class" || tag == "type" || tag == "alias") tag else "",
      nameRef
    )
    printNodeInfo(i, description, symColor)

    if (privateWithinIdx != -1)
      processRef(i, privateWithinIdx, "privateWithin")

    processRef(i, infoIdx, "info")
  }

  def processAnnotInfoBody(i: Int, tag: String, end: Int) {
    printNodeInfo(i, tag, annotColor)
    processRef(i, "info")
    processListRef(i, end)
  }
  
  def readNameRef() = {
    val refIdx = buf.readNat
    val pos    = buf.readIndex
    processEntry(refIdx)
    buf.readIndex = pos
    indexToName(refIdx)
  }
  
  def processOwnerRef(i: Int) {
    w.println(i + " -> " + buf.readNat + " [label=\"owner\"];")
  }

  def processRef(i: Int, name: String) {
    processRef(i, buf.readNat, name)
  }

  def processRef(i: Int, refIdx: Int, name: String) {
    val pos = buf.readIndex

    w.println(i + " -> " + refIdx + " [label=\"" + name + "\"];")
    processEntry(refIdx)
    buf.readIndex = pos
  }

  def processListRef(i: Int, end: Int, name: String = "") {buf.until(end, () => processRef(i, name))}

  def treeTag2string(tag: Int): String = tag match {
    case ALTERNATIVEtree     => "ALTERNATIVEtree"
    case ANNOTATEDtree       => "ANNOTATEDtree"
    case APPLIEDTYPEtree     => "APPLIEDTYPEtree"
    case APPLYDYNAMICtree    => "APPLYDYNAMICtree"
    case APPLYtree           => "APPLYtree"
    case ARRAYVALUEtree      => "ARRAYVALUEtree"
    case ASSIGNtree          => "ASSIGNtree"
    case BINDtree            => "BINDtree"
    case BLOCKtree           => "BLOCKtree"
    case CASEtree            => "CASEtree"
    case CLASStree           => "CLASStree"
    case COMPOUNDTYPEtree    => "COMPOUNDTYPEtree"
    case DEFDEFtree          => "DEFDEFtree"
    case DOCDEFtree          => "DOCDEFtree"
    case EMPTYtree           => "EMPTYtree"
    case EXISTENTIALTYPEtree => "EXISTENTIALTYPEtree"
    case FUNCTIONtree        => "FUNCTIONtree"
    case IDENTtree           => "IDENTtree"
    case IFtree              => "IFtree"
    case IMPORTtree          => "IMPORTtree"
    case LABELtree           => "LABELtree"
    case LITERALtree         => "LITERALtree"
    case MATCHtree           => "MATCHtree"
    case MODULEtree          => "MODULEtree"
    case NEWtree             => "NEWtree"
    case PACKAGEtree         => "PACKAGEtree"
    case RETURNtree          => "RETURNtree"
    case SELECTFROMTYPEtree  => "SELECTFROMTYPEtree"
    case SELECTtree          => "SELECTtree"
    case SINGLETONTYPEtree   => "SINGLETONTYPEtree"
    case STARtree            => "STARtree"
    case SUPERtree           => "SUPERtree"
    case TEMPLATEtree        => "TEMPLATEtree"
    case THIStree            => "THIStree"
    case THROWtree           => "THROWtree"
    case TREtree             => "TREtree"
    case TYPEAPPLYtree       => "TYPEAPPLYtree"
    case TYPEBOUNDStree      => "TYPEBOUNDStree"
    case TYPEDEFtree         => "TYPEDEFtree"
    case TYPEDtree           => "TYPEDtree"
    case TYPEtree            => "TYPEtree"
    case UNAPPLYtree         => "UNAPPLYtree"
    case VALDEFtree          => "VALDEFtree"

    case _ => throw new RuntimeException("Unknown tree tag: " + tag)
  }
}
