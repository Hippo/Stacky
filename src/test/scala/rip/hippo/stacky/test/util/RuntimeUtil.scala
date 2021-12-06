package rip.hippo.stacky.test.util

import rip.hippo.stacky.{ExecutionContext, VirtualMachine, VirtualMemory}

import java.io.File

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
object RuntimeUtil {

  def loadRuntimeJar(virtualMachine: VirtualMachine): Unit =
    try {
      Option(System.getenv("JAVA_HOME")) match {
        case Some(value) =>
          println("Loading runtime jar, this may take a while...")
          virtualMachine.classLoader.loadFromFile(new File("/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home", "/jre/lib/rt.jar"))
        case None => throw new RuntimeException("JAVA_HOME not set, unable to load runtime jar")
      }
    } finally {
      println("Runtime jar loaded!")
    }

  def initializeSystemClass(virtualMachine: VirtualMachine): Unit = {
    println("Initializing system class...")
    virtualMachine.classLoader.loadClass("java/lang/System").lookupMethod("initializeSystemClass", "()V") match {
      case Some(value) =>
        virtualMachine.execute(new ExecutionContext(new VirtualMemory, value.methodInfo))
        println("System class initialized.")
      case None =>
        println("Unable to find initializeSystemClass.")

    }
  }
}
