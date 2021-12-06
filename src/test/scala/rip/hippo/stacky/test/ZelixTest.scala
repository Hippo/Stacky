package rip.hippo.stacky.test

import org.scalatest.funsuite.AnyFunSuite
import rip.hippo.hippocafe.ClassReader
import rip.hippo.stacky.test.util.RuntimeUtil
import rip.hippo.stacky.values.types.VirtualObject
import rip.hippo.stacky.values.types.primitive.{VirtualInteger, VirtualVoid}
import rip.hippo.stacky.{ExecutionContext, VirtualMachine, VirtualMemory}

import scala.util.{Failure, Success, Using}

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
final class ZelixTest extends AnyFunSuite {


  private val fileName = "ZelixStringTest"

  private val vm = new VirtualMachine().withDefaultHooks
  RuntimeUtil.loadRuntimeJar(vm)


  test("zelix.test") {
    Option(Thread.currentThread().getContextClassLoader.getResourceAsStream(s"$fileName.class")) match {
      case Some(value) =>
        Using(new ClassReader(value)) {
          classReader =>
            classReader.classFile
        } match {
          case Success(value) =>
            vm.classLoader.loadClassFile(value)

            val virtualCLass = vm.classLoader.loadClass(fileName)
            virtualCLass.lookupMethod("a", "(II)Ljava/lang/String;") match {
              case Some(value) =>
                val executionContext = new ExecutionContext(VirtualMemory.create().withLocals(Map(
                  0 -> VirtualInteger(-31220),
                  1 -> VirtualInteger(-21169)
                )).build(), value.methodInfo)
                vm.execute(executionContext)
              case None =>
                println("NO MAIN METHOD FOUND!")
            }
          case Failure(exception) => exception.printStackTrace()
        }
      case None => println(s"Could not load resource $fileName.class")
    }
  }

}
