package rip.hippo.stacky.test

import org.scalatest.funsuite.AnyFunSuite
import rip.hippo.stacky.{ExecutionContext, VirtualMachine, VirtualMemory}
import rip.hippo.hippocafe.disassembler.instruction.BytecodeOpcode
import rip.hippo.hippocafe.disassembler.instruction.constant.impl.StringConstant
import rip.hippo.hippocafe.disassembler.instruction.impl.{ConstantInstruction, PushInstruction, ReferenceInstruction, SimpleInstruction}
import rip.hippo.stacky.test.util.RuntimeUtil
import rip.hippo.stacky.values.types.primitive.VirtualInteger

import scala.collection.mutable.ListBuffer

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
final class StackSimulationTest extends AnyFunSuite {

  val vm = new VirtualMachine().withDefaultHooks

  val testWithJRE = true

  if (testWithJRE)
    RuntimeUtil.loadRuntimeJar(vm)

  test("stack.simulate") {
    val example = List(
      PushInstruction(11341),
      PushInstruction(-16470),
      SimpleInstruction(BytecodeOpcode.IXOR),
      PushInstruction(23246),
      SimpleInstruction(BytecodeOpcode.IXOR),
      PushInstruction(-14032),
      SimpleInstruction(BytecodeOpcode.IXOR)
    )
    val result = vm.execute(ExecutionContext(new VirtualMemory, example, ListBuffer()))
    assert(result.virtualMemory.popAs[VirtualInteger].value == 25)

    if (testWithJRE) {
      val stringLengthExample = List (
        ConstantInstruction(StringConstant("11111")),
        ReferenceInstruction(BytecodeOpcode.INVOKEVIRTUAL, "java/lang/String", "length", "()I"),
        SimpleInstruction(BytecodeOpcode.IRETURN)
      )

      val stringLengthResult = vm.execute(ExecutionContext(new VirtualMemory, stringLengthExample, ListBuffer()))
      println(stringLengthResult)
    }
  }

}
