package rip.hippo.stacky.values

import rip.hippo.stacky.values.types.{VirtualArray, VirtualClass, VirtualObject, VirtualTop}
import rip.hippo.stacky.values.types.primitive.{VirtualDouble, VirtualFloat, VirtualInteger, VirtualLong, VirtualVoid}

/**
 * @author Hippo
 * @version 1.0.0, 12/1/21
 * @since 1.0.0
 */
trait VirtualValue:
  def isWide: Boolean =
    this match {
      case VirtualDouble(value) => true
      case VirtualLong(value) => true
      case _ => false
    }