package rip.hippo.stacky.hook.hooks.java.lang

import rip.hippo.stacky.hook.ClassHook
import rip.hippo.stacky.values.types.VirtualObject
import rip.hippo.stacky.values.types.primitive.{VirtualInteger, VirtualVoid}

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
final class StringHook extends ClassHook("java/lang/String") {

  addHook("intern", "()Ljava/lang/String;", (virtualMachine, memory) => virtualMachine.intern(memory.loadAs[VirtualObject](0)))

 addHook("substring", "(II)Ljava/lang/String;", (virtualMachine, memory) => virtualMachine.getInternedString(memory.loadAs[VirtualObject](0).convertToString.substring(memory.loadAs[VirtualInteger](1).value, memory.loadAs[VirtualInteger](2).value)))

 addHook("charAt", "(I)C", (virtualMachine, memory) => VirtualInteger(memory.loadAs[VirtualObject](0).convertToString.charAt(memory.loadAs[VirtualInteger](1).value)))
}
