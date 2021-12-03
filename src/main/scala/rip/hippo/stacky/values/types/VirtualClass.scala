package rip.hippo.stacky.values.types

import rip.hippo.stacky.proxy.{FieldProxy, MethodProxy}
import rip.hippo.hippocafe.util.Type
import rip.hippo.stacky.values.VirtualValue
import rip.hippo.stacky.loader.VirtualClassLoader

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
final case class VirtualClass(name: String, superClass: Option[VirtualClass], interfaces: ListBuffer[VirtualClass], virtualClassLoader: Option[VirtualClassLoader]) extends VirtualValue {
  val fields: ListBuffer[FieldProxy] = ListBuffer()
  val methods: ListBuffer[MethodProxy] = ListBuffer()

  superClass match {
    case Some(value) =>
      fields ++= value.fields
      methods ++= value.methods.filterNot(_.isInit)
    case None =>
  }
  
  interfaces.foreach(interface => {
    fields ++= interface.fields
    methods ++= interface.methods.filterNot(_.isInit) // Shouldn't exist in interfaces, but when has an extra filter ever hurt anyone
  })

  val isPrimitive: Boolean = name.length == 1 && !Type.getType(name).isObject

  def isAssignableFrom(virtualClass: VirtualClass): Boolean = {
    if (virtualClass.name.equals(name)) return true
    val supers = mutable.Set[String]()
    var current = virtualClass.superClass
    while (current.nonEmpty) {
      supers += current.get.name
      current = current.get.superClass
    }

    if (supers.contains(name)) return true

    def getAllInterfaces(virtualClass: VirtualClass): Set[String] = {
      val collectedInterfaces = mutable.Set[String]()

      virtualClass.interfaces.foreach(vc => {
        collectedInterfaces += vc.name
        collectedInterfaces ++= getAllInterfaces(vc)
      })

      collectedInterfaces.toSet
    }

    getAllInterfaces(virtualClass).contains(name)
  }

  def lookupField(name: String, descriptor: String): Option[FieldProxy] =
    fields.find(proxy => proxy.fieldInfo.name.equals(name) && proxy.fieldInfo.descriptor.equals(descriptor))

  def lookupMethod(name: String, descriptor: String): Option[MethodProxy] =
    methods.find(proxy => proxy.methodInfo.name.equals(name) && proxy.methodInfo.descriptor.equals(descriptor))
}

object VirtualClass {
  val VOID_CLASS: VirtualClass = VirtualClass("V", Option.empty, ListBuffer(), Option.empty)
  val BOOLEAN_CLASS: VirtualClass = VirtualClass("Z", Option.empty, ListBuffer(), Option.empty)
  val BYTE_CLASS: VirtualClass = VirtualClass("B", Option.empty, ListBuffer(), Option.empty)
  val SHORT_CLASS: VirtualClass = VirtualClass("S", Option.empty, ListBuffer(), Option.empty)
  val CHAR_CLASS: VirtualClass = VirtualClass("C", Option.empty, ListBuffer(), Option.empty)
  val INT_CLASS: VirtualClass = VirtualClass("I", Option.empty, ListBuffer(), Option.empty)
  val FLOAT_CLASS: VirtualClass = VirtualClass("F", Option.empty, ListBuffer(), Option.empty)
  val LONG_CLASS: VirtualClass = VirtualClass("J", Option.empty, ListBuffer(), Option.empty)
  val DOUBLE_CLASS: VirtualClass = VirtualClass("D", Option.empty, ListBuffer(), Option.empty)
}
