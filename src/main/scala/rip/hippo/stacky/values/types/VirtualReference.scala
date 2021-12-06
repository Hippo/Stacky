package rip.hippo.stacky.values.types

import rip.hippo.stacky.proxy.{FieldProxy, MethodProxy}
import rip.hippo.stacky.values.VirtualValue

import java.util.concurrent.locks.{Condition, Lock, ReentrantLock}
import scala.collection.mutable.ListBuffer

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
trait VirtualReference extends VirtualValue:
  val fields: ListBuffer[FieldProxy]
  val methods: ListBuffer[MethodProxy]

  def lookupField(name: String, descriptor: String): Option[FieldProxy]
  def lookupMethod(name: String, descriptor: String): Option[MethodProxy]
