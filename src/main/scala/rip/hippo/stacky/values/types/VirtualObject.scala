package rip.hippo.stacky.values.types

import rip.hippo.stacky.proxy.{FieldProxy, MethodProxy}
import rip.hippo.stacky.values.VirtualValue
import rip.hippo.stacky.values.types.VirtualObject.NULL_OBJECT
import rip.hippo.stacky.values.types.primitive.VirtualInteger

import java.util.concurrent.locks.{Condition, Lock, ReentrantLock}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
class VirtualObject(val thisClass: VirtualClass) extends VirtualValue with VirtualReference {
  
  override val fields: ListBuffer[FieldProxy] = Option(thisClass) match {
    case Some(value) => value.fields.filter(_.isVirtual).map(_.virtualCopy)
    case None => ListBuffer()
  }
  override val methods: ListBuffer[MethodProxy] = Option(thisClass) match {
    case Some(value) => value.methods.filter(_.isVirtual)
    case None => ListBuffer()
  }

  def isInstanceOf(virtualClass: VirtualClass): VirtualInteger = {
    if (this == NULL_OBJECT) {
      VirtualInteger(0)
    } else {
      VirtualInteger(if (virtualClass.isAssignableFrom(thisClass)) 1 else 0)
    }
  }

  def checkCast(virtualClass: VirtualClass): VirtualObject = {
    if (this == NULL_OBJECT) return this
    //TODO: make this throw exception: if (!isInstanceOf(virtualClass))
    val casted = VirtualObject(virtualClass)
    fields.foreach(field => {
      casted.lookupField(field.fieldInfo.name, field.fieldInfo.descriptor) match {
        case Some(value) =>
          value.fieldValue = field.fieldValue
        case None =>
      }
    })
    casted
  }

  override def lookupField(name: String, descriptor: String): Option[FieldProxy] =
    fields.find(proxy => proxy.fieldInfo.name.equals(name) && proxy.fieldInfo.descriptor.equals(descriptor))

  override def lookupMethod(name: String, descriptor: String): Option[MethodProxy] =
    methods.find(proxy => proxy.methodInfo.name.equals(name) && proxy.methodInfo.descriptor.equals(descriptor))

  def convertToString: String =
    lookupField("value", "[C") match {
      case Some(value) =>
        String(value.fieldValue.asInstanceOf[VirtualArray].value.map(_.asInstanceOf[VirtualInteger].char))
      case None => "" // TODO: no field exception
    }

  def virtualClone: VirtualObject = { // TODO: check if cloneable
    val copy = VirtualObject(thisClass)
    copy.fields.clear()
    copy.fields ++= fields
    copy
  }
}

object VirtualObject {
  val NULL_OBJECT: VirtualObject = VirtualObject(null)
}