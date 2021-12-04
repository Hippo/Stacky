package rip.hippo.stacky.values.types

import rip.hippo.stacky.proxy.{FieldProxy, MethodProxy}
import rip.hippo.stacky.values.VirtualValue
import rip.hippo.stacky.values.types.VirtualObject.NULL_OBJECT

import java.util.concurrent.locks.{Lock, ReentrantLock}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
final case class VirtualObject(thisClass: VirtualClass) extends VirtualValue {
  
  val lock: Lock = new ReentrantLock()
  
  val fields: ListBuffer[FieldProxy] = thisClass.fields.filter(_.isVirtual).map(_.virtualCopy)
  val methods: ListBuffer[MethodProxy] = thisClass.methods.filter(_.isVirtual)


  def lookupField(name: String, descriptor: String): Option[FieldProxy] =
    fields.find(proxy => proxy.fieldInfo.name.equals(name) && proxy.fieldInfo.descriptor.equals(descriptor))

  def lookupMethod(name: String, descriptor: String): Option[MethodProxy] =
    methods.find(proxy => proxy.methodInfo.name.equals(name) && proxy.methodInfo.descriptor.equals(descriptor))
}

object VirtualObject {
  val NULL_OBJECT: VirtualObject = VirtualObject(null)
}