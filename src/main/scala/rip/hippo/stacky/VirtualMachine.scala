package rip.hippo.stacky

import rip.hippo.hippocafe.disassembler.instruction.Instruction
import rip.hippo.hippocafe.disassembler.instruction.impl.{ANewArrayInstruction, AppendFrameInstruction, BranchInstruction, ChopFrameInstruction, ConstantInstruction, FullFrameInstruction, IncrementInstruction, InvokeDynamicInstruction, LabelInstruction, LineNumberInstruction, LookupSwitchInstruction, MultiANewArrayInstruction, NewArrayInstruction, PushInstruction, ReferenceInstruction, SameFrameInstruction, SameLocalsFrameInstruction, SimpleInstruction, TableSwitchInstruction, TypeInstruction, VariableInstruction}
import rip.hippo.hippocafe.disassembler.tcb.TryCatchBlock
import rip.hippo.stacky.hook.{ClassHook, Hook}
import rip.hippo.stacky.loader.VirtualClassLoader
import rip.hippo.stacky.values.types.VirtualObject
import rip.hippo.stacky.values.types.primitive.VirtualVoid

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
final class VirtualMachine {
  val classLoader: VirtualClassLoader = new VirtualClassLoader(this)
  private val hooks: mutable.Map[BasicIdentifier, Hook] = mutable.Map()
  private var thrown: Option[VirtualObject] = Option.empty

  def addHook(owner: String, name: String, descriptor: String, hook: Hook): Unit =
    hooks += (BasicIdentifier(owner, name, descriptor) -> hook)

  def addHook(classHook: ClassHook): Unit =
    classHook.hooks.foreach(hook => hooks += (BasicIdentifier(classHook.className, hook.name, hook.descriptor) -> hook.hook))

  def getHook(owner: String, name: String, descriptor: String): Option[Hook] =
    hooks.get(BasicIdentifier(owner, name, descriptor))


  def execute(executionContext: ExecutionContext): ExecutionResult = {
    val instructions = executionContext.instructions
    val memory = executionContext.virtualMemory

    var index = 0
    var continue = true


    def getScopedTcbs: List[TryCatchBlock] = {
      val buffer = ListBuffer[TryCatchBlock]()

      executionContext.tryCatchBlocks
        .filter(tcb => instructions.indexOf(tcb.start) <= index && instructions.indexOf(tcb.end) >= index)
        .foreach(buffer.+=)



      buffer.sortBy(tcb => instructions.indexOf(tcb.start)).reverse.toList
    }

    def throwException(): Unit = {
      memory.clearStack()
      val exception = thrown.get
      val tcbs = getScopedTcbs
      tcbs.find(tcb => classLoader.loadClass(tcb.safeCatchType).isAssignableFrom(exception.thisClass)) match {
        case Some(value) =>
          memory.push(exception)
          index = instructions.indexOf(value.handler)
          thrown = Option.empty
        case None =>
      }
    }

    while (continue) {
      thrown match {
        case Some(value) => throwException()
        case None =>
      }

      val instruction = executionContext.process(instructions(index))
      continue = !executionContext.shouldStop(instruction)
      if (continue) {
        instruction match {
          case ANewArrayInstruction(descriptor) =>
          case BranchInstruction(bytecodeOpcode, label) =>
          case ConstantInstruction(constant) =>
          case IncrementInstruction(localIndex, value) =>
          case InvokeDynamicInstruction(invokeDynamicConstant) =>
          case LineNumberInstruction(number) =>
          case LookupSwitchInstruction(default) =>
          case MultiANewArrayInstruction(descriptor, dimensions) =>
          case NewArrayInstruction(arrayType) =>
          case PushInstruction(value) =>
          case ReferenceInstruction(bytecodeOpcode, owner, name, descriptor) =>
          case SimpleInstruction(bytecodeOpcode) =>
          case TableSwitchInstruction(default, low, high) =>
          case TypeInstruction(bytecodeOpcode, typeName) =>
          case VariableInstruction(bytecodeOpcode, index) =>
          case _ =>
        }
        index += 1
      }
    }

    ExecutionResult(memory, VirtualVoid(), executionContext)
  }


}

private final case class BasicIdentifier(owner: String, name: String, descriptor: String)
