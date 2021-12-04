package rip.hippo.stacky.values.types

import rip.hippo.stacky.values.VirtualValue

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
final case class VirtualArray(elementType: VirtualClass, value: Array[VirtualValue]) extends VirtualValue