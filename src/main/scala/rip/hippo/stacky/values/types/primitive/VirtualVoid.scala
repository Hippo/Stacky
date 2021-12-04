package rip.hippo.stacky.values.types.primitive

import rip.hippo.stacky.values.VirtualValue
import rip.hippo.stacky.values.types.primitive.VirtualVoid.INSTANCE

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
final case class VirtualVoid() extends VirtualValue

object VirtualVoid {
  private val INSTANCE: VirtualVoid = new VirtualVoid()

  def apply(): VirtualVoid = INSTANCE
}
