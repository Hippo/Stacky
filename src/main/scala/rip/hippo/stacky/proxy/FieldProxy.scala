package rip.hippo.stacky.proxy

import rip.hippo.hippocafe.FieldInfo
import rip.hippo.hippocafe.access.AccessFlag
import rip.hippo.stacky.values.types.VirtualClass

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
final case class FieldProxy(virtualClass: VirtualClass, fieldInfo: FieldInfo) {
  val isStatic: Boolean = fieldInfo.accessFlags.contains(AccessFlag.ACC_STATIC)
  val isVirtual: Boolean = !isStatic
}
