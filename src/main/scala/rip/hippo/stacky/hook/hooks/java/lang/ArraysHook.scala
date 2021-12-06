package rip.hippo.stacky.hook.hooks.java.lang

import rip.hippo.stacky.hook.ClassHook
import rip.hippo.stacky.values.types.VirtualArray
import rip.hippo.stacky.values.types.primitive.VirtualInteger

import java.util
import java.util.Arrays

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
final class ArraysHook extends ClassHook("java/util/Arrays") {

  // TODO: add the rest of these
  addHook("copyOf", "([CI)[C", (virtualMachine, memory) => {
    val array = memory.loadAs[VirtualArray](0)
    val length = memory.loadAs[VirtualInteger](1).value
    
    val newArray = util.Arrays.copyOf(array.value, length)
    VirtualArray(array.elementType, newArray)
  })
}
