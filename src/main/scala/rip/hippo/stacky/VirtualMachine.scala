package rip.hippo.stacky

import rip.hippo.hippocafe.disassembler.instruction.Instruction
import rip.hippo.hippocafe.disassembler.instruction.BytecodeOpcode.*
import rip.hippo.hippocafe.disassembler.instruction.constant.impl.{ClassConstant, DoubleConstant, DynamicConstant, FloatConstant, IntegerConstant, InvokeDynamicConstant, LongConstant, MethodHandleConstant, MethodTypeConstant, ModuleConstant, PackageConstant, StringConstant, UTF8Constant}
import rip.hippo.hippocafe.disassembler.instruction.impl.{ANewArrayInstruction, AppendFrameInstruction, BranchInstruction, ChopFrameInstruction, ConstantInstruction, FullFrameInstruction, IncrementInstruction, InvokeDynamicInstruction, LabelInstruction, LineNumberInstruction, LookupSwitchInstruction, MultiANewArrayInstruction, NewArrayInstruction, PushInstruction, ReferenceInstruction, SameFrameInstruction, SameLocalsFrameInstruction, SimpleInstruction, TableSwitchInstruction, TypeInstruction, VariableInstruction}
import rip.hippo.hippocafe.disassembler.tcb.TryCatchBlock
import rip.hippo.hippocafe.disassembler.instruction.array.ArrayType
import rip.hippo.hippocafe.util.Type
import rip.hippo.stacky.hook.{ClassHook, Hook}
import rip.hippo.stacky.loader.VirtualClassLoader
import rip.hippo.stacky.values.VirtualValue
import rip.hippo.stacky.values.types.{VirtualArray, VirtualClass, VirtualObject, VirtualTop}
import rip.hippo.stacky.values.types.primitive.{VirtualDouble, VirtualFloat, VirtualInteger, VirtualLong, VirtualVoid}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * @author Hippo
 * @version 1.0.0, 12/2/21
 * @since 1.0.0
 */
final class VirtualMachine {
  val classLoader: VirtualClassLoader = new VirtualClassLoader(this)
  private val hooks: mutable.Map[BasicIdentifier, Hook] = mutable.Map()
  private var thrown: Option[VirtualObject] = Option.empty
  private val internedStrings: mutable.Map[String, VirtualObject] = mutable.Map()

  def addHook(owner: String, name: String, descriptor: String, hook: Hook): Unit =
    hooks += (BasicIdentifier(owner, name, descriptor) -> hook)

  def addHook(classHook: ClassHook): Unit =
    classHook.hooks.foreach(hook => hooks += (BasicIdentifier(classHook.className, hook.name, hook.descriptor) -> hook.hook))

  def getHook(owner: String, name: String, descriptor: String): Option[Hook] =
    hooks.get(BasicIdentifier(owner, name, descriptor))


  def execute(executionContext: ExecutionContext): ExecutionResult = {
    val instructions = executionContext.instructions
    val memory = executionContext.virtualMemory

    var index = 0
    var continue = true


    def getScopedTcbs: List[TryCatchBlock] = {
      val buffer = ListBuffer[TryCatchBlock]()

      executionContext.tryCatchBlocks
        .filter(tcb => instructions.indexOf(tcb.start) <= index && instructions.indexOf(tcb.end) >= index)
        .foreach(buffer.+=)



      buffer.sortBy(tcb => instructions.indexOf(tcb.start)).reverse.toList
    }

    def throwException(): Unit = {
      memory.clearStack()
      val exception = thrown.get
      val tcbs = getScopedTcbs
      tcbs.find(tcb => classLoader.loadClass(tcb.safeCatchType).isAssignableFrom(exception.thisClass)) match {
        case Some(value) =>
          memory.push(exception)
          index = instructions.indexOf(value.handler)
          thrown = Option.empty
        case None =>
      }
    }

    while (continue) {
      thrown match {
        case Some(value) => throwException()
        case None =>
      }

      val instruction = executionContext.process(instructions(index))
      continue = !executionContext.shouldStop(instruction)
      if (continue) {
        instruction match {
          case ANewArrayInstruction(descriptor) =>
            val index = memory.popAs[VirtualInteger]
            val backend = new Array[VirtualValue](index.value)
            memory.push(VirtualArray(classLoader.loadClass(descriptor), backend))

          case BranchInstruction(bytecodeOpcode, label) =>
            val branchIndex = instructions.indexOf(label) - 1

            bytecodeOpcode match {

              case IF_ACMPEQ | IF_ACMPNE =>
                val value2 = memory.pop()
                val value1 = memory.pop()
                bytecodeOpcode match {
                  case IF_ACMPEQ if value1 eq value1 => index = branchIndex
                  case IF_ACMPNE if value1 ne value2 => index = branchIndex
                  case _ =>
                }

              case IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value

                bytecodeOpcode match {
                  case IF_ICMPEQ if value1 == value2 => index = branchIndex
                  case IF_ICMPNE if value1 != value2 => index = branchIndex
                  case IF_ICMPLT if value1 < value2 => index = branchIndex
                  case IF_ICMPLE if value1 <= value2 => index = branchIndex
                  case IF_ICMPGT if value1 > value2 => index = branchIndex
                  case IF_ICMPGE if value1 >= value2 => index = branchIndex
                  case _ =>
                }

              case IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE =>
                val value = memory.popAs[VirtualInteger].value

                bytecodeOpcode match {
                  case IFEQ if value == 0 => index = branchIndex
                  case IFNE if value != 0 => index = branchIndex
                  case IFLT if value < 0 => index = branchIndex
                  case IFLE if value <= 0 => index = branchIndex
                  case IFGT if value > 0 => index = branchIndex
                  case IFGE if value >= 0 => index = branchIndex
                  case _ =>
                }

              case IFNONNULL | IFNULL =>
                memory.popAs[VirtualObject] match {
                  case VirtualObject.NULL_OBJECT if bytecodeOpcode == IFNULL => index = branchIndex
                  case _ if bytecodeOpcode == IFNONNULL => index = branchIndex
                  case _ =>
                }

              case _ =>
            }

          case ConstantInstruction(constant) =>
            constant match {
              case ClassConstant(value) =>
                memory.push(classLoader.loadClass(value))
              case DoubleConstant(value) =>
                memory.push(VirtualDouble(value))
                memory.push(VirtualTop())
              case DynamicConstant(name, descriptor, bootstrapMethod, bootstrapArguments) =>
                // TODO: Dynamic
              case FloatConstant(value) =>
                memory.push(VirtualFloat(value))
              case IntegerConstant(value) =>
                memory.push(VirtualInteger(value))
              case InvokeDynamicConstant(name, descriptor, bootstrapMethod, bootstrapArguments) =>
                // TODO: Dynamic
              case LongConstant(value) =>
                memory.push(VirtualLong(value))
                memory.push(VirtualTop())
              case MethodHandleConstant(referenceKind, owner, name, descriptor) =>
                // TODO: Method handles
              case MethodTypeConstant(value) =>
                // TODO: Method type
              case StringConstant(value) =>
                memory.push(getInternedString(value))
              case UTF8Constant(value) =>
                memory.push(getInternedString(value))
              case _ =>
            }
          case IncrementInstruction(localIndex, value) =>
            memory.loadAs[VirtualInteger](localIndex).value += value

          case InvokeDynamicInstruction(invokeDynamicConstant) =>
            // TODO: Invoke dynamic
          case lookupSwitchInstruction: LookupSwitchInstruction =>
            val key = memory.popAs[VirtualInteger].value
            lookupSwitchInstruction.pairs.get(key) match {
              case Some(value) => index = instructions.indexOf(value) - 1
              case None => index = instructions.indexOf(lookupSwitchInstruction.default) - 1
            }

          case MultiANewArrayInstruction(descriptor, dimensions) =>
            val sizes = new Array[Int](dimensions)
            (0 until dimensions).foreach(i => sizes(i) = memory.popAs[VirtualInteger].value)

            def createMultiArray(descriptor: String, dimensions: Array[Int], index: Int): VirtualArray = {
              val backend = new Array[VirtualValue](dimensions(index))
              val virtualArray = VirtualArray(classLoader.loadClass(descriptor.substring(1)), backend)
              if (index == dimensions.length - 1) return virtualArray
              (0 until dimensions(index)).foreach(i => virtualArray.value(i) = createMultiArray(descriptor.substring(1), dimensions, index))
              virtualArray
            }

            memory.push(createMultiArray(descriptor, sizes, 0))

          case NewArrayInstruction(arrayType) =>
            val size = memory.popAs[VirtualInteger].value
            val backend = new Array[VirtualValue](size)
            val elementType = arrayType match {
              case ArrayType.BOOLEAN => VirtualClass.BOOLEAN_CLASS
              case ArrayType.CHAR => VirtualClass.CHAR_CLASS
              case ArrayType.FLOAT => VirtualClass.FLOAT_CLASS
              case ArrayType.DOUBLE => VirtualClass.DOUBLE_CLASS
              case ArrayType.BYTE => VirtualClass.BYTE_CLASS
              case ArrayType.SHORT => VirtualClass.SHORT_CLASS
              case ArrayType.INT => VirtualClass.INT_CLASS
              case ArrayType.LONG => VirtualClass.LONG_CLASS
            }
            memory.push(VirtualArray(elementType, backend))

          case PushInstruction(value) =>
            memory.push(VirtualInteger(value))

          case ReferenceInstruction(bytecodeOpcode, owner, name, descriptor) =>
            bytecodeOpcode match {
              case GETSTATIC =>
                classLoader.loadClass(owner).lookupField(name, descriptor) match {
                  case Some(value) =>
                    memory.push(value.fieldValue)
                    if (value.fieldValue.isWide) {
                      memory.push(VirtualTop())
                    }
                  case None => // TODO: no field exception
                }

              case PUTSTATIC =>
                classLoader.loadClass(owner).lookupField(name, descriptor) match {
                  case Some(value) =>
                    val newValue = if (value.fieldValue.isWide) memory.popWide() else memory.pop()
                    value.fieldValue = newValue
                  case None => //TODO: no field exception
                }

              case GETFIELD =>
                val reference = memory.popAs[VirtualObject]
                reference.lookupField(name, descriptor) match {
                  case Some(value) =>
                    memory.push(value.fieldValue)
                    if (value.fieldValue.isWide) {
                      memory.push(VirtualTop())
                    }
                  case None => //TODO: no field exception
                }

              case PUTFIELD =>
                classLoader.loadClass(owner).lookupField(name, descriptor) match {
                  case Some(staticLookup) =>
                    val newValue = if (staticLookup.fieldValue.isWide) memory.popWide() else memory.pop()
                    val reference = memory.popAs[VirtualObject]
                    reference.lookupField(name, descriptor).get.fieldValue = newValue
                  case None => //TODO: no field exception
                }
            }

          case SimpleInstruction(bytecodeOpcode) =>
          case TableSwitchInstruction(default, low, high) =>
          case TypeInstruction(bytecodeOpcode, typeName) =>
          case VariableInstruction(bytecodeOpcode, index) =>
          case _ =>
        }
        index += 1
        if (index >= instructions.size) continue = false
      }
    }

    ExecutionResult(memory, VirtualVoid(), executionContext)
  }


  def getInternedString(string: String): VirtualObject =
    internedStrings.get(string) match {
      case Some(value) => value
      case None =>
        val virtualString = getString(string)
        internedStrings += (string -> virtualString)
        virtualString
    }

  private def getString(string: String): VirtualObject = {
    val stringObject = VirtualObject(classLoader.loadClass("java/lang/String"))

    val backend = new Array[VirtualValue](string.length)
    (0 until string.length).foreach(i => backend(i) = VirtualInteger(string.charAt(i)))
    val virtualCharArray = VirtualArray(VirtualClass.CHAR_CLASS, backend)

    stringObject.lookupField("value", "[C").get.fieldValue = virtualCharArray
    stringObject
  }
}

object VirtualMachine {
  def getDefaultValue(descriptor: String): VirtualValue = {
    Type.getType(descriptor) match {
      case Type.VOID => VirtualVoid()
      case Type.BOOLEAN | Type.BYTE | Type.CHAR | Type.SHORT | Type.INT => VirtualInteger(0)
      case Type.FLOAT => VirtualFloat(0F)
      case Type.LONG => VirtualLong(0L)
      case Type.DOUBLE => VirtualDouble(0D)
      case _ => VirtualObject.NULL_OBJECT
    }
  }
}

private final case class BasicIdentifier(owner: String, name: String, descriptor: String)
