package rip.hippo.stacky.hook.hooks.java.lang

import rip.hippo.stacky.hook.ClassHook
import rip.hippo.stacky.values.types.primitive.VirtualInteger
import rip.hippo.stacky.values.types.{VirtualClass, VirtualObject}

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
final class JavaClassHook extends ClassHook("java/lang/Class") {

  addHook("getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;", (virtualMachine, memory) => {
    memory.loadAs[VirtualObject](0).convertToString match {
      case "void" => VirtualClass.VOID_CLASS
      case "boolean" => VirtualClass.BOOLEAN_CLASS
      case "byte" => VirtualClass.BYTE_CLASS
      case "char" => VirtualClass.CHAR_CLASS
      case "short" => VirtualClass.SHORT_CLASS
      case "int" => VirtualClass.INT_CLASS
      case "float" => VirtualClass.FLOAT_CLASS
      case "long" => VirtualClass.LONG_CLASS
      case "double" => VirtualClass.DOUBLE_CLASS
    }
  })

  addHook("desiredAssertionStatus0", "(Ljava/lang/Class;)Z", (virtualMachine, memory) => VirtualInteger(0))
}
