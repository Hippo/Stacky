package rip.hippo.stacky.test

import org.scalatest.funsuite.AnyFunSuite
import rip.hippo.stacky.VirtualMemory
import rip.hippo.stacky.values.types.primitive.VirtualInteger
import rip.hippo.stacky.values.types.primitive.VirtualFloat

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
final class VirtualMemoryTest extends AnyFunSuite {


  test("VirtualMemory.clone") {
    val memory = VirtualMemory()


    memory.push(VirtualInteger(1))
    memory.pop()
    memory.push(VirtualInteger(5))
    memory.store(1, VirtualInteger(12))

    val copy = memory.copy()

    memory.store(0, VirtualInteger(69))

    println(memory)
    println(copy)


    val memoryWithBuilder = VirtualMemory.create()
      .push(VirtualInteger(64))
      .withLocals(Map(
        1 -> VirtualInteger(64),
        2 -> VirtualFloat(45.5)
      )).build()

    val obj = new Object

    
    println(memoryWithBuilder)
  }

}
