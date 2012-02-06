package aa

import java.io.{ File, PrintWriter }
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble
import scala.collection.{ mutable, immutable }
import scala.reflect.internal.pickling._
import scala.reflect.internal.Flags
import scala.tools.nsc.util.ShowPickled
import scala.sys.process._
import PickleFormat._
import util.Random
import fr.emn.criojo.core.Variable
import java.util.{HashMap, Date, Arrays}
import java.lang.reflect.Method
import java.lang.reflect.Field
import fr.emn.criojo.lang.{Cham2, Cham, Molecule}

object PickleView {

  var mapDependencies:HashMap[String, List[String]] = new HashMap[String, List[String]]()
  var mapPackages:mutable.Map[String, Package] = mutable.Map[String, Package]()

  var clazzName:String = _

  def loadFromClassPath(name: String) {
    println("Processing class: " + name)
    var t:Cham = new Cham();
    //var t:Variable = new Variable("x")
    //var t:Cham2 = new Cham2();
    //var t:String = ""

    val clazz: Class[_] = t.getClass()
    var methods:Array[Method] = clazz.getMethods
    var fields:Array[Field] = clazz.getDeclaredFields()
    //var annotation:_ = clazz.annotation

    if(!mapDependencies.containsKey(clazz)) {

      mapDependencies.put(fullNameToClassName(clazz.getName), List())
    }

    if(!mapPackages.isDefinedAt(clazzName)) {

      mapPackages.put(fullNameToClassName(clazz.getName), clazz.getPackage)
    }

    clazzName = fullNameToClassName(clazz.getName())
    analyzeMethods(methods, clazz)
    analyzeFields(fields, clazz)

    export();
    println("c'est fini!")
  }



  def fullNameToClassName(name:String):String = {

    val returnTypeNameArray = name.split("""\.""").toList
    returnTypeNameArray(returnTypeNameArray.size -1).replace("$", ".")
  }

  def analyseArguments(arguments:Array[Class[_]], clazz:Class[_]) = {

    for(arg <- arguments) {

      val argumentTypeName = fullNameToClassName(arg.getName)
      val className = fullNameToClassName(clazz.getName)

      if(!mapDependencies.get(className).contains(argumentTypeName))
        mapDependencies.put(className, argumentTypeName :: mapDependencies.get(className))

      if(!mapPackages.isDefinedAt(argumentTypeName))
        mapPackages.put(argumentTypeName, arg.getPackage)
    }
  }

  def analyzeMethods(methods:Array[Method], clazz:Class[_]) = {

    for(m:Method <- methods) {

      val name:String = m.getName
      if(m.getDeclaringClass.equals(clazz)) {
        //println(m+" ==> ["+name+"]")

        analyseArguments(m.getParameterTypes, clazz)

        val returnTypeName = fullNameToClassName(m.getReturnType.getName)
        val className = fullNameToClassName(clazz.getName)
        if(!mapDependencies.get(className).contains(returnTypeName))
          mapDependencies.put(className, returnTypeName :: mapDependencies.get(className))

        if(!mapPackages.isDefinedAt(returnTypeName))
          mapPackages.put(returnTypeName, m.getReturnType.getPackage)
      }
    }
  }

  def analyzeFields(fields:Array[Field], clazz:Class[_]) = {

    for(f:Field <- fields) {

      val name:String = f.getName
      if(f.getDeclaringClass.equals(clazz)) {

        //println(f+" ==> ["+name+"]")


        val fieldTypeName = fullNameToClassName(f.getType.getName)
        val className = fullNameToClassName(clazz.getName)

        if(!mapDependencies.get(className).contains(fieldTypeName))
          mapDependencies.put(className, fieldTypeName :: mapDependencies.get(className))

        if(!mapPackages.isDefinedAt(fieldTypeName))
          mapPackages.put(fieldTypeName, f.getType.getPackage)
      }
    }
  }

  def main(args: Array[String]) {

    //if (args.isEmpty) runDemo
    //else args foreach loadFromClassPath
    //args foreach loadFromClassPath2
    loadFromClassPath("hello")
    sys.exit(0)
  }

  private var w: PrintWriter = _

  def export() {
     process()
  }

  val defaultColor = "navajowhite"
  val symColor     = "firebrick"
  val refColor     = "darkgoldenrod1"
  val tpeColor     = "plum2"
  val annotColor   = "coral"
  val literalColor = "chartreuse"

  val typeFont = "/Users/paulp/Library/Fonts/Consolas Italic.ttf"
  val termFont = "/Users/paulp/Library/Fonts/Consolas.ttf"

  def withWriter[T](writer: PrintWriter)(body: => T): T = {
    w = writer
    try body
    finally { w.close() ; w = null }
  }


  def process(outputExtension: String = "pdf") {

    var tempMapPackage:HashMap[Package, Int] = new HashMap[Package, Int]()
    var tempGeneratedMapPackage:HashMap[String, Int] = new HashMap[String, Int]()

    val dir = new File("target/dot")
    dir.mkdirs()
    val dotFile = new File(dir, clazzName + ".dot")
    val outFile = new File(dir, clazzName + "." + outputExtension)
    println("Generating " + outFile + "...")

    withWriter(new PrintWriter(dotFile)) {
      w println   "digraph {"
      w println   "  node [shape=box, style=filled, color=navajowhite];"

      w println   "0 [label=\""+clazzName+"\", color=\"orange\"];"

      var cpt:Int = 1


      mapPackages.foreach(t => {


        var k:String = t._1
        var v:Package = t._2

        if(v!=null && !tempMapPackage.containsKey(v) && !tempGeneratedMapPackage.containsKey(v.getName)) {

          val names:Array[String] = v.getName.split("""\.""")

          if(!tempGeneratedMapPackage.containsKey(v.getName)) {

            w println cpt+" [label=\""+names.last+"\", color=\"gold\"];"
            tempMapPackage.put(v, cpt)

            cpt = cpt+1
          }

          var index:Int = names.size-2
          while(index>=0){

            if(tempGeneratedMapPackage.containsKey(names(index))) {
              w println (cpt-1)+" -> "+tempGeneratedMapPackage.get(names(index))+" [label=\"\" arrowsize=1, color=\"gold\"];"
              index = -1
            }
            else {
              w println cpt+" [label=\""+names(index)+"\", color=\"gold\"];"
              w println (cpt-1)+" -> "+cpt+" [label=\"\" arrowsize=1, color=\"gold\"];"
              tempGeneratedMapPackage.put(names(index), cpt)
              cpt = cpt+1
              index = index-1

            }

          }
        }
      })

      for (value <- mapDependencies.get(clazzName)) {
        w println cpt+" [label=\""+value+"\", color=\"plum2\"];"
        w println cpt+" -> 0 [label=\"\" arrowsize=1, color=\"plum2\"];"
        
        if(mapPackages.isDefinedAt(value)) {

          var p:Package = mapPackages.get(value).get

          if(p!=null && !p.getName.equals("scala")) {

            var boole:Boolean = !p.getName.equals("scala")
            val i:Int = tempMapPackage.get(p)
            w println cpt+" -> "+i+" [label=\"\" arrowsize=1, color=gold];"
          }
          else {

            val i:Int = tempGeneratedMapPackage.get("scala")
            w println cpt+" -> "+i+" [label=\"\" arrowsize=1, color=gold];"
          }

        }

        cpt = cpt+1
      }

      w println   "}"
    }



    (
      ("/usr/local/bin/dot -Tpdf -o " + outFile + " " + dotFile)
        #&& ("test -f /usr/bin/open")
        #&& ("/usr/bin/open " + outFile)
      ).!!
  }
}