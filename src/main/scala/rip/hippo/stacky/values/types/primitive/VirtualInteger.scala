package rip.hippo.stacky.values.types.primitive

import rip.hippo.stacky.values.VirtualValue

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
final case class VirtualInteger(var value: Int) extends VirtualValue with VirtualPrimitive[Int] {

  def boolean: Boolean = value == 1
  def byte: Byte = value.toByte
  def short: Short = value.toShort
  def char: Char = value.toChar
}