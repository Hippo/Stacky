package rip.hippo.stacky.proxy

import rip.hippo.hippocafe.MethodInfo
import rip.hippo.hippocafe.access.AccessFlag
import rip.hippo.stacky.values.types.VirtualClass

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
final case class MethodProxy(virtualClass: VirtualClass, methodInfo: MethodInfo) {
  val isStatic: Boolean = methodInfo.accessFlags.contains(AccessFlag.ACC_STATIC)
  val isVirtual: Boolean = !isStatic
  val isAbstract: Boolean = methodInfo.accessFlags.contains(AccessFlag.ACC_ABSTRACT)
  val isInit: Boolean = methodInfo.name.matches("^<(cl)?init>$")
}
