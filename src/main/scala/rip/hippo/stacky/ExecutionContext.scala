package rip.hippo.stacky

import rip.hippo.hippocafe.MethodInfo
import rip.hippo.hippocafe.disassembler.instruction.Instruction
import rip.hippo.hippocafe.disassembler.tcb.TryCatchBlock

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
final case class ExecutionContext(virtualMemory: VirtualMemory, instructions: List[Instruction], tryCatchBlocks: ListBuffer[TryCatchBlock]) {

  def this(virtualMemory: VirtualMemory, methodInfo: MethodInfo) =
    this(virtualMemory, methodInfo.instructions.toList, methodInfo.tryCatchBlocks)

  val memoryWatches: mutable.Map[Instruction, ListBuffer[VirtualMemory]] = mutable.Map()
  private val watchInstructions = mutable.Set[Instruction]()
  private val stops = mutable.Set[Instruction]()

  def process(instruction: Instruction): Instruction = {
    if (watchInstructions.contains(instruction)) {
      memoryWatches(instruction) += virtualMemory.copy()
    }
    instruction
  }

  def watch(instruction: Instruction): ExecutionContext = {
    watchInstructions += instruction
    memoryWatches += (instruction -> ListBuffer())
    this
  }

  def watchAll: ExecutionContext = {
    watchInstructions ++= instructions
    instructions.foreach(instruction => memoryWatches += (instruction -> ListBuffer()))
    this
  }

  def stopOn(instruction: Instruction): ExecutionContext = {
    stops += instruction
    this
  }

  def shouldStop(instruction: Instruction): Boolean =
    stops.contains(instruction)
}
