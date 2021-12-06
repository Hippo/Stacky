package rip.hippo.stacky.hook

import scala.collection.mutable.ListBuffer

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
class ClassHook(val className: String) {
  val hooks: ListBuffer[RegisteredClassHook] = ListBuffer()
  
  def addHook(name: String, descriptor: String, hook: Hook): Unit =
    hooks += RegisteredClassHook(name, descriptor, hook)
}

final case class RegisteredClassHook(name: String, descriptor: String, hook: Hook)
