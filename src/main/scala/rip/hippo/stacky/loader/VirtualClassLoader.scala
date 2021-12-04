package rip.hippo.stacky.loader

import rip.hippo.hippocafe.{ClassFile, ClassReader}
import rip.hippo.stacky.proxy.{FieldProxy, MethodProxy}
import rip.hippo.stacky.values.types.VirtualClass
import rip.hippo.stacky.{ExecutionContext, VirtualMachine, VirtualMemory}

import java.io.{File, FileInputStream}
import java.util.jar.JarFile
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Using

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
final class VirtualClassLoader(virtualMachine: VirtualMachine) {

  private val classFileMap: mutable.Map[String, ClassFile] = mutable.Map()
  private val loadedClasses: mutable.Map[String, VirtualClass] = mutable.Map()

  def loadFromFile(file: File): Unit =
    if (file.getName.endsWith(".jar")) {
      Using(new JarFile(file)) {
        jar =>
          val entries = jar.entries()
          while (entries.hasMoreElements) {
            val entry = entries.nextElement()
            if (entry.getName.matches("^.+\\.class\\/?$")) {
              val stream = jar.getInputStream(entry)
              Using(new ClassReader(stream)) {
                classReader =>
                  classFileMap += (classReader.classFile.name -> classReader.classFile)
              }
            }
          }
      }
    } else {
      Using(new ClassReader(new FileInputStream(file))) {
        classReader =>
          classFileMap += (classReader.classFile.name -> classReader.classFile)
      }
    }

  def loadClassFile(classFile: ClassFile): Unit =
    classFileMap += (classFile.name -> classFile)

  def loadClass(className: String): VirtualClass =
    loadedClasses.get(className) match {
      case Some(value) => value
      case None =>
        if (className.charAt(0) == '[')
          loadArrayClass(className) 
        else classFileMap.get(className) match {
          case Some(value) =>
            val superClass = if (value.superName != null) Option(loadClass(value.superName)) else Option.empty
            val interfaces = ListBuffer[VirtualClass]()
            value.interfaces.foreach(interface => interfaces += loadClass(interface))
            val virtualClass = VirtualClass(className, superClass, interfaces, Option(this))
            virtualClass.fields ++= value.fields.map(info => FieldProxy(virtualClass, info, VirtualMachine.getDefaultValue(info.descriptor)))
            virtualClass.methods ++= value.methods.map(info => MethodProxy(virtualClass, info))

            virtualClass.lookupMethod("<clinit>", "()V") match {
              case Some(value) =>
                val executionContext = new ExecutionContext(new VirtualMemory, value.methodInfo)
                virtualMachine.execute(executionContext)
              case None =>
            }

            virtualClass
          case None => throw new ClassNotFoundException(className)
        }
    }

  private def loadArrayClass(descriptor: String): VirtualClass =
    loadedClasses.get(descriptor) match {
      case Some(value) => value
      case None =>
        val arrayClass = VirtualClass(descriptor, Option(loadClass("java/lang/Object")), ListBuffer(), Option.empty)
        loadedClasses += (descriptor -> arrayClass)
        arrayClass
    }
}
