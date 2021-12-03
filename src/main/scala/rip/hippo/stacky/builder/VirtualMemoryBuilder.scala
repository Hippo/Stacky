package rip.hippo.stacky.builder

import rip.hippo.stacky.VirtualMemory
import rip.hippo.stacky.values.VirtualValue

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
final class VirtualMemoryBuilder {

  private val memory = new VirtualMemory

  def push(value: VirtualValue): VirtualMemoryBuilder = {
    memory.push(value)
    this
  }

  def withStack(stack: Seq[VirtualValue]): VirtualMemoryBuilder = {
    stack.foreach(memory.push)
    this
  }

  def store(index: Int, value: VirtualValue): VirtualMemoryBuilder = {
    memory.store(index, value)
    this
  }

  def withLocals(locals: Map[Int, VirtualValue]): VirtualMemoryBuilder = {
    locals.foreach(memory.store)
    this
  }

  def build(): VirtualMemory =
    memory
}
