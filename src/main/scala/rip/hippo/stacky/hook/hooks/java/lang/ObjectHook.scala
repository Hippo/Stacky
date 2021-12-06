package rip.hippo.stacky.hook.hooks.java.lang

import rip.hippo.stacky.hook.ClassHook
import rip.hippo.stacky.values.types.VirtualObject
import rip.hippo.stacky.values.types.primitive.{VirtualInteger, VirtualLong, VirtualVoid}

import java.util.concurrent.TimeUnit

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
final class ObjectHook extends ClassHook("java/lang/Object") {

  addHook("getClass", "()Ljava/lang/Class;", (_, memory) => memory.loadAs[VirtualObject](0).thisClass)
  addHook("hashCode", "()I", (_, memory) => VirtualInteger(memory.load(0).hashCode()))

  // todo: make these useful
  addHook("wait", "(J)V", (_, memory) => {
    VirtualVoid()
  })
  addHook("notify", "()V", (_, memory) => {
    VirtualVoid()
  })
  addHook("notifyAll", "()V", (_, memory) => {
    VirtualVoid()
  })

  addHook("clone", "()Ljava/lang/Object;", (_, memory) => memory.loadAs[VirtualObject](0).virtualClone)
}
