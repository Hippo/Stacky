package rip.hippo.stacky

import rip.hippo.stacky.builder.VirtualMemoryBuilder
import rip.hippo.stacky.values.VirtualValue
import rip.hippo.stacky.values.types.VirtualTop

import scala.collection.mutable

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
final class VirtualMemory {

  private val stack: mutable.Stack[VirtualValue] = mutable.Stack()
  private val locals: mutable.Map[Int, VirtualValue] = mutable.Map()

  def push(virtualValue: VirtualValue): Unit =
    stack.push(virtualValue)

  def pop(slots: Int): Unit =
    (0 until slots).foreach(_ => stack.pop())

  def pop(): VirtualValue =
    stack.pop()

  def popAs[T <: VirtualValue]: T =
    stack.pop().asInstanceOf[T]

  def popWide(): VirtualValue = {
    val pop1 = pop()
    val pop2 = pop()
    pop1 match {
      case VirtualTop() => pop2
      case _ => pop1
    }
  }

  def clearStack(): Unit =
    stack.clear()

  def store(index: Int, virtualValue: VirtualValue): Unit =
    locals += (index -> virtualValue)

  def load(index: Int): VirtualValue =
    locals(index)


  def loadAs[T <: VirtualValue](index: Int): T =
    locals(index).asInstanceOf[T]

  def copy(): VirtualMemory = {
    val memory = new VirtualMemory

    memory.stack ++= this.stack
    memory.locals ++= this.locals

    memory
  }


  override def toString = s"VirtualMemory($stack, $locals)"
}

object VirtualMemory {
  def create(): VirtualMemoryBuilder =
    new VirtualMemoryBuilder
}
