package rip.hippo.stacky.hook

import rip.hippo.stacky.VirtualMemory
import rip.hippo.stacky.values.VirtualValue

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
trait Hook:
  def hook(memory: VirtualMemory): VirtualValue
