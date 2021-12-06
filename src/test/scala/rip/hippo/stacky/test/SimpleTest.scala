package rip.hippo.stacky.test

import org.scalatest.funsuite.AnyFunSuite
import rip.hippo.hippocafe.ClassReader
import rip.hippo.stacky.{ExecutionContext, VirtualMachine, VirtualMemory}
import rip.hippo.stacky.test.util.RuntimeUtil
import rip.hippo.stacky.values.types.VirtualObject
import rip.hippo.stacky.values.types.primitive.{VirtualInteger, VirtualVoid}

import scala.util.{Failure, Success, Using}

/**
 * @author Hippo
 * @version 1.0.0, 12/5/21
 * @since 1.0.0
 */
final class SimpleTest extends AnyFunSuite {


  private val fileName = "HelloWorldArr"

  private val vm = new VirtualMachine().withDefaultHooks
  RuntimeUtil.loadRuntimeJar(vm)

  // TODO: possibly make it do this on its own (lot more than initially thought)
  vm.addHook("java/io/PrintStream", "println", "(Ljava/lang/String;)V", (vm, memory) => {
    println(memory.loadAs[VirtualObject](1).convertToString)
    VirtualVoid()
  })
  vm.addHook("java/io/PrintStream", "println", "(I)V", (vm, memory) => {
    println(memory.loadAs[VirtualInteger](1).value)
    VirtualVoid()
  })

  vm.addHook("java/lang/System", "<clinit>", "()V", (vm, memory) => {
    vm.classLoader.loadClass("java/lang/System").lookupField("out", "Ljava/io/PrintStream;") match {
      case Some(value) =>
        value.fieldValue = VirtualObject(vm.classLoader.loadClass("java/io/PrintStream"))
      case None => println("Unable to find standard out!")
    }
    VirtualVoid()
  })

  test("simple.main") {
    Option(Thread.currentThread().getContextClassLoader.getResourceAsStream(s"$fileName.class")) match {
      case Some(value) =>
        Using(new ClassReader(value)) {
          classReader =>
            classReader.classFile
        } match {
          case Success(value) =>
            vm.classLoader.loadClassFile(value)

            val virtualCLass = vm.classLoader.loadClass(fileName)
            virtualCLass.lookupMethod("main", "([Ljava/lang/String;)V") match {
              case Some(value) =>
                val executionContext = new ExecutionContext(new VirtualMemory, value.methodInfo)
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
