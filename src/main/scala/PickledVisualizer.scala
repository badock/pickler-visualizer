package org.improving

import java.io.{ File, PrintWriter }
import java.util.Arrays
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble
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
  def this(name: String) = this (ClassLoader.getSystemClassLoader().loadClass(name))
  
  val defaultColor = "navajowhite"
  val nameColor    = "mediumseagreen"
  val symColor     = "skyblue"
  val refColor     = "darkgoldenrod1"
  val tpeColor     = "plum2"
  val annotColor   = "coral"
  val literalColor = "brown"

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

  def seekToPos(pos: Int) {
    buf.readIndex = pos;
  }

  def process(outputExtension: String = "pdf") {
    val dir = new File("target/dot") 
    dir.mkdirs()
    val dotFile = new File(dir, clazz.getSimpleName + ".dot")
    val outFile = new File(dir, clazz.getSimpleName + "." + outputExtension)
    println("Generating " + outFile + "...")
    
    withWriter(new PrintWriter(dotFile)) {
      w.println("digraph {")
      w.println("graph [label=\"" + clazz.getSimpleName + "\", concentrate=true];")
      w.println("node [shape=box, style=filled, color=" + defaultColor + "];")

      0 until index.size foreach processEntry
      w.println("}")
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

  def processEntry(tag: Int, len: Int, i: Int) {
    val end = buf.readIndex + len

    val tn = ShowPickled.tag2string(tag)

    tag match {
      case (TERMname | TYPEname) =>
        processNameInfo(i, tn, end)
      case (TYPEsym | ALIASsym | MODULEsym) =>
        processSymbolInfo(i, tn, end)
      case CLASSsym =>
        processSymbolInfo(i, tn, end)
        if (buf.readIndex < end) {processRef(i, "thistype_Ref")}
      case VALsym =>
        processSymbolInfo(i, tn, end)
        if (buf.readIndex < end) {processRef(i, "alias_Ref")}
      case NONEsym =>
        printNodeInfo(i, tn, symColor)
      case (EXTref | EXTMODCLASSref) =>
        printNodeInfo(i, tn, refColor)
        processRef(i, "name_Ref")
        if (buf.readIndex < end) {processRef(i, "owner_Ref")}
      case THIStpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "sym_Ref")
      case SINGLEtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe_Ref")
        processRef(i, "sym_Ref")
      case CONSTANTtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "constant_Ref")
      case (TYPEBOUNDStpe | SUPERtpe) =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe_Ref")
        processRef(i, "tpe_Ref")
      case TYPEREFtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe_Ref")
        processRef(i, "sym_Ref")
        processListRef(i, end, "targ_Ref")
      case (REFINEDtpe | CLASSINFOtpe) =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "classsym_Ref")
        processListRef(i, end, "tpe_Ref")
      case (METHODtpe | POLYtpe | IMPLICITMETHODtpe) =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe_Ref")
        processListRef(i, end, "sym_Ref")
      case ANNOTATEDtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe_Ref")
        processListRef(i, end, "annotinfo_Ref")
      case EXISTENTIALtpe =>
        printNodeInfo(i, tn, tpeColor)
        processRef(i, "tpe_Ref")
        processListRef(i, end, "sym_Ref")
      case LITERALboolean =>
        val v = if (buf.readLong(len) == 0L) "false" else "true"
        printLiteralNodeInfo(i, tn + "" + v)
      case LITERALbyte =>
        printLiteralNodeInfo(i, tn + "" + buf.readLong(len).toByte)
      case LITERALshort =>
        printNodeInfo(i, tn + "" + buf.readLong(len).toShort)
      case LITERALchar =>
        printLiteralNodeInfo(i, tn + "" + buf.readLong(len).toChar)
      case LITERALint =>
        printLiteralNodeInfo(i, tn + "" + buf.readLong(len).toInt)
      case LITERALlong =>
        printLiteralNodeInfo(i, tn + "" + buf.readLong(len))
      case LITERALfloat =>
        printLiteralNodeInfo(i, tn + "" + intBitsToFloat(buf.readLong(len).toInt))
      case LITERALdouble =>
        printLiteralNodeInfo(i, "" + longBitsToDouble(buf.readLong(len)))
      case LITERALstring =>
        printNodeInfo(i, tn, literalColor)
        processRef(i, "name_Ref")
      case LITERALnull =>
        printNodeInfo(i, tn, literalColor)
      case LITERALclass =>
        printNodeInfo(i, tn, literalColor)
        processRef(i, "tpe_Ref")
      case LITERALenum =>
        printNodeInfo(i, tn, literalColor)
        processRef(i, "sym_Ref")
      case SYMANNOT =>
        processRef(i, "sym_Ref")
        processAnnotInfoBody(i, tn, end)
      case CHILDREN =>
        printNodeInfo(i, tn, annotColor)
      case ANNOTINFO =>
        processAnnotInfoBody(i, tn, end)
      case ANNOTARGARRAY =>
        printNodeInfo(i, tn, annotColor)
        processListRef(i, end, "constAnnotArg_Ref")
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
    printNodeInfo(i, "(" + value + ")", literalColor)
  }

  def processNameInfo(i: Int, tag: String, end: Int) {
    val data = buf.bytes.slice(buf.readIndex, end)
    val name = new String(data)
    printNodeInfo(i, tag + "(" + name + ")", nameColor)
  }

  def processSymbolInfo(i: Int, tag: String, end: Int) {
    val pos = buf.readIndex

    processRef(i, "name_Ref")
    processRef(i, "owner_Ref")
    val flagLongNat = buf.readLongNat

    // Handle optional [privateWithin_Ref]
    val nextIdx = buf.readNat
    val (privateWithinIdx, infoIdx) = if (buf.readIndex == end) {
      (-1, nextIdx)
    } else {
      (nextIdx, buf.readNat)
    }

    printNodeInfo(i, tag + "[" + Flags.flagsToString(flagLongNat) + "]", symColor)

    if (privateWithinIdx != -1) {
      processRef(i, privateWithinIdx, "privateWithin_Ref")
    }
    processRef(i, infoIdx, "info_Ref")
  }

  def processAnnotInfoBody(i: Int, tag: String, end: Int) {
    printNodeInfo(i, tag, annotColor)
    processRef(i, "info_Ref")
    processListRef(i, end)
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
    case EMPTYtree => "EMPTYtree"
    case PACKAGEtree => "PACKAGEtree"
    case CLASStree => "CLASStree"
    case MODULEtree => "MODULEtree"
    case VALDEFtree => "VALDEFtree"
    case DEFDEFtree => "DEFDEFtree"
    case TYPEDEFtree => "TYPEDEFtree"
    case LABELtree => "LABELtree"
    case IMPORTtree => "IMPORTtree"
    case DOCDEFtree => "DOCDEFtree"
    case TEMPLATEtree => "TEMPLATEtree"
    case BLOCKtree => "BLOCKtree"
    case CASEtree => "CASEtree"
    case ALTERNATIVEtree => "ALTERNATIVEtree"
    case STARtree => "STARtree"
    case BINDtree => "BINDtree"
    case UNAPPLYtree => "UNAPPLYtree"
    case ARRAYVALUEtree => "ARRAYVALUEtree"
    case FUNCTIONtree => "FUNCTIONtree"
    case ASSIGNtree => "ASSIGNtree"
    case IFtree => "IFtree"
    case MATCHtree => "MATCHtree"
    case RETURNtree => "RETURNtree"
    case TREtree => "TREtree"
    case THROWtree => "THROWtree"
    case NEWtree => "NEWtree"
    case TYPEDtree => "TYPEDtree"
    case TYPEAPPLYtree => "TYPEAPPLYtree"
    case APPLYtree => "APPLYtree"
    case APPLYDYNAMICtree => "APPLYDYNAMICtree"
    case SUPERtree => "SUPERtree"
    case THIStree => "THIStree"
    case SELECTtree => "SELECTtree"
    case IDENTtree => "IDENTtree"
    case LITERALtree => "LITERALtree"
    case TYPEtree => "TYPEtree"
    case ANNOTATEDtree => "ANNOTATEDtree"
    case SINGLETONTYPEtree => "SINGLETONTYPEtree"
    case SELECTFROMTYPEtree => "SELECTFROMTYPEtree"
    case COMPOUNDTYPEtree => "COMPOUNDTYPEtree"
    case APPLIEDTYPEtree => "APPLIEDTYPEtree"
    case TYPEBOUNDStree => "TYPEBOUNDStree"
    case EXISTENTIALTYPEtree => "EXISTENTIALTYPEtree"

    case _ => throw new RuntimeException("Unknown tree tag: " + tag)
  }
}
