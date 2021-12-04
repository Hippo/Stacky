package rip.hippo.stacky.values.types.primitive

import rip.hippo.stacky.values.VirtualValue

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
final case class VirtualDouble(var value: Double) extends VirtualValue with VirtualPrimitive[Double]