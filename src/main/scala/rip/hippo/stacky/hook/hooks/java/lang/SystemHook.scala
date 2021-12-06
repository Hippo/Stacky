package rip.hippo.stacky.hook.hooks.java.lang

import rip.hippo.stacky.{ExecutionContext, VirtualMemory}
import rip.hippo.stacky.hook.ClassHook
import rip.hippo.stacky.values.types.{VirtualArray, VirtualObject}
import rip.hippo.stacky.values.types.primitive.{VirtualInteger, VirtualLong, VirtualVoid}

/**
 * @author Hippo
 * @version 1.0.0, 12/4/21
 * @since 1.0.0
 */
final class SystemHook extends ClassHook("java/lang/System") {

  addHook("initProperties", "(Ljava/util/Properties;)Ljava/util/Properties;", (virtualMachine, memory) => {
    val virtualProperties = memory.loadAs[VirtualObject](0)

    System.getProperties.entrySet().forEach(pair => {
      val key = pair.getKey.toString
      val value = pair.getValue.toString
      virtualProperties.lookupMethod("setProperty", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;") match {
        case Some(proxy) =>
          val newMemory = VirtualMemory.create()
            .withLocals(Map(
              0 -> virtualProperties,
              1 -> virtualMachine.getInternedString(key),
              2 -> virtualMachine.getInternedString(value)
            )).build()
          virtualMachine.execute(new ExecutionContext(newMemory, proxy.methodInfo))
        case None => // TODO: no method exception
      }
    })

    virtualProperties
  })

  addHook("currentTimeMillis", "()J", (_, _) => VirtualLong(System.currentTimeMillis()))
  addHook("nanoTime", "()J", (_, _) => VirtualLong(System.nanoTime()))
  addHook("arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", (virtualMachine, memory) => {
    val src = memory.loadAs[VirtualArray](0).value
    val srcPos = memory.loadAs[VirtualInteger](1).value
    val dest = memory.loadAs[VirtualArray](2).value
    val destPos = memory.loadAs[VirtualInteger](3).value
    val length = memory.loadAs[VirtualInteger](4).value

    System.arraycopy(src, srcPos, dest, destPos, length)

    VirtualVoid()
  })

  addHook("identityHashCode", "(Ljava/lang/Object;)I", (_, memory) => VirtualInteger(System.identityHashCode(memory.load(0))))

  addHook("setIn0", "(Ljava/io/InputStream;)V", (virtualMachine, memory) => {
    virtualMachine.classLoader.loadClass("java/lang/System").lookupField("in", "Ljava/io/InputStream;") match {
      case Some(value) =>
        value.fieldValue = memory.load(0)
      case None => // TODO: no field exception
    }
    VirtualVoid()
  })

  addHook("setOut0", "(Ljava/io/OutputStream;)V", (virtualMachine, memory) => {
    virtualMachine.classLoader.loadClass("java/lang/System").lookupField("out", "Ljava/io/OutputStream;") match {
      case Some(value) =>
        value.fieldValue = memory.load(0)
      case None => // TODO: no field exception
    }
    VirtualVoid()
  })

  addHook("setErr0", "(Ljava/io/OutputStream;)V", (virtualMachine, memory) => {
    virtualMachine.classLoader.loadClass("java/lang/System").lookupField("err", "Ljava/io/OutputStream;") match {
      case Some(value) =>
        value.fieldValue = memory.load(0)
      case None => // TODO: no field exception
    }
    VirtualVoid()
  })

  addHook("mapLibraryName", "(Ljava/lang/String;)Ljava/lang/String;", (virtualMachine, memory) => virtualMachine.getInternedString(memory.loadAs[VirtualObject](0).convertToString))
}
