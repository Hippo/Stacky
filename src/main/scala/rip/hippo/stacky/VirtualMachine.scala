package rip.hippo.stacky

import rip.hippo.hippocafe.disassembler.instruction.Instruction
import rip.hippo.hippocafe.disassembler.instruction.BytecodeOpcode.*
import rip.hippo.hippocafe.disassembler.instruction.constant.impl.{ClassConstant, DoubleConstant, DynamicConstant, FloatConstant, IntegerConstant, InvokeDynamicConstant, LongConstant, MethodHandleConstant, MethodTypeConstant, ModuleConstant, PackageConstant, StringConstant, UTF8Constant}
import rip.hippo.hippocafe.disassembler.instruction.impl.{ANewArrayInstruction, AppendFrameInstruction, BranchInstruction, ChopFrameInstruction, ConstantInstruction, FullFrameInstruction, IncrementInstruction, InvokeDynamicInstruction, LabelInstruction, LineNumberInstruction, LookupSwitchInstruction, MultiANewArrayInstruction, NewArrayInstruction, PushInstruction, ReferenceInstruction, SameFrameInstruction, SameLocalsFrameInstruction, SimpleInstruction, TableSwitchInstruction, TypeInstruction, VariableInstruction}
import rip.hippo.hippocafe.disassembler.tcb.TryCatchBlock
import rip.hippo.hippocafe.disassembler.instruction.array.ArrayType
import rip.hippo.hippocafe.util.Type
import rip.hippo.stacky.hook.hooks.java.lang.{ArraysHook, JavaClassHook, ObjectHook, StringHook, SystemHook}
import rip.hippo.stacky.hook.{ClassHook, Hook}
import rip.hippo.stacky.loader.VirtualClassLoader
import rip.hippo.stacky.values.VirtualValue
import rip.hippo.stacky.values.types.{VirtualArray, VirtualClass, VirtualObject, VirtualReference, VirtualTop}
import rip.hippo.stacky.values.types.primitive.{VirtualDouble, VirtualFloat, VirtualInteger, VirtualLong, VirtualPrimitive, VirtualVoid}

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

  def addClassHook(classHook: ClassHook): Unit =
    classHook.hooks.foreach(hook => hooks += (BasicIdentifier(classHook.className, hook.name, hook.descriptor) -> hook.hook))

  def getHook(owner: String, name: String, descriptor: String): Option[Hook] =
    hooks.get(BasicIdentifier(owner, name, descriptor))


  def withDefaultHooks: VirtualMachine = {
    addClassHook(new SystemHook)
    addClassHook(new ObjectHook)
    addClassHook(new StringHook)
    addClassHook(new JavaClassHook)
    addClassHook(new ArraysHook)
    this
  }

  def execute(executionContext: ExecutionContext): ExecutionResult = {
    val instructions = executionContext.instructions
    val memory = executionContext.virtualMemory

    var index = 0
    var continue = instructions.nonEmpty // TODO: maybe do something if they try to execute an abstract method?


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
                memory.popAs[VirtualReference] match {
                  case VirtualObject.NULL_OBJECT if bytecodeOpcode == IFNULL => index = branchIndex
                  case _ if bytecodeOpcode == IFNONNULL => index = branchIndex
                  case _ =>
                }

              case GOTO | GOTO_W =>
                index = branchIndex

              case JSR | JSR_W => // TODO: implement JSR

              case _ =>
            }

          case ConstantInstruction(constant) =>
            constant match {
              case ClassConstant(value) =>
                //memory.push(classLoader.loadClass(value))
                memory.push(VirtualObject(classLoader.loadClass("java/lang/Class")))
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
                val reference = memory.popAs[VirtualReference]
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
                    val reference = memory.popAs[VirtualReference]
                    reference.lookupField(name, descriptor).get.fieldValue = newValue
                  case None => //TODO: no field exception
                }

              case INVOKEVIRTUAL | INVOKESPECIAL | INVOKEINTERFACE | INVOKESTATIC =>
                val isStatic = bytecodeOpcode == INVOKESTATIC
                val offset = if (isStatic) 0 else 1
                val parameterCount = Type.getMethodParameterTypes(descriptor).size
                val fullStack = ListBuffer[VirtualValue]()
                (0 until parameterCount).foreach(_ => fullStack += memory.pop())
                val parameters = mutable.Map[Int, VirtualValue]()
                fullStack.reverse.zipWithIndex.foreach((value, idx) => parameters += ((idx + offset) -> value))
                if (!isStatic) {
                  parameters += (0 -> memory.pop())
                }
                /*val parameters = (0 until parameterCount + offset).map(i => if (i == parameterCount) {
                  0 -> memory.pop()
                } else {
                  ((parameterCount + offset) - (i + offset)) -> memory.pop()
                }).toMap*/
                val newMemory = VirtualMemory.create().withLocals(parameters.toMap).build()
                getHook(owner, name, descriptor) match {
                  case Some(value) =>
                    val returnValue = value.hook(this, newMemory)
                    returnValue match {
                      case VirtualDouble(value) =>
                        memory.push(returnValue)
                        memory.push(VirtualTop())
                      case VirtualLong(value) =>
                        memory.push(returnValue)
                        memory.push(VirtualTop())
                      case VirtualVoid() =>
                      case _ => memory.push(returnValue)
                    }
                  case None =>
                    val methodOption = classLoader.loadClass(owner).lookupMethod(name, descriptor) /*if (isStatic) {
                      classLoader.loadClass(owner).lookupMethod(name, descriptor)
                    } else {
                      newMemory.loadAs[VirtualReference](0).lookupMethod(name, descriptor)
                    }*/

                    methodOption match {
                      case Some(value) =>
                        val newContext = new ExecutionContext(newMemory, value.methodInfo)
                        val returnValue = execute(newContext).returnValue
                        returnValue match {
                          case VirtualDouble(value) =>
                            memory.push(returnValue)
                            memory.push(VirtualTop())
                          case VirtualLong(value) =>
                            memory.push(returnValue)
                            memory.push(VirtualTop())
                          case VirtualVoid() =>
                          case _ => memory.push(returnValue)
                        }
                      case None =>
                        println("no method: " + owner + " " + name + " " + descriptor)
                      // TODO: no method exception
                    }
                }
            }

          case SimpleInstruction(bytecodeOpcode) =>

            bytecodeOpcode match {
              case NOP =>
              case ACONST_NULL => memory.push(VirtualObject.NULL_OBJECT)
              case ATHROW =>
                thrown = Option(memory.popAs[VirtualObject])
              case ALOAD_0 | ALOAD_1 | ALOAD_2 | ALOAD_3 =>
                memory.push(memory.load(bytecodeOpcode.opcode - ALOAD_0.opcode))
              case ILOAD_0 | ILOAD_1 | ILOAD_2 | ILOAD_3 =>
                memory.push(memory.load(bytecodeOpcode.opcode - ILOAD_0.opcode))
              case FLOAD_0 | FLOAD_1 | FLOAD_2 | FLOAD_3 =>
                memory.push(memory.load(bytecodeOpcode.opcode - FLOAD_0.opcode))
              case DLOAD_0 | DLOAD_1 | DLOAD_2 | DLOAD_3 =>
                memory.push(memory.load(bytecodeOpcode.opcode - DLOAD_0.opcode))
                memory.push(VirtualTop())
              case LLOAD_0 | LLOAD_1 | LLOAD_2 | LLOAD_3 =>
                memory.push(memory.load(bytecodeOpcode.opcode - LLOAD_0.opcode))
                memory.push(VirtualTop())
              case ASTORE_0 | ASTORE_1 | ASTORE_2 | ASTORE_3 =>
                memory.store(bytecodeOpcode.opcode - ASTORE_0.opcode, memory.pop())
              case ISTORE_0 | ISTORE_1 | ISTORE_2 | ISTORE_3 =>
                memory.store(bytecodeOpcode.opcode - ISTORE_0.opcode, memory.pop())
              case FSTORE_0 | FSTORE_1 | FSTORE_2 | FSTORE_3 =>
                memory.store(bytecodeOpcode.opcode - FSTORE_0.opcode, memory.pop())
              case DSTORE_0 | DSTORE_1 | DSTORE_2 | DSTORE_3 =>
                memory.store(bytecodeOpcode.opcode - DSTORE_0.opcode, memory.popWide())
                memory.store(bytecodeOpcode.opcode - DSTORE_1.opcode, VirtualTop())
              case LSTORE_0 | LSTORE_1 | LSTORE_2 | LSTORE_3 =>
                memory.store(bytecodeOpcode.opcode - LSTORE_0.opcode, memory.popWide())
                memory.store(bytecodeOpcode.opcode - LSTORE_1.opcode, VirtualTop())

              case L2I => memory.push(VirtualInteger(memory.popWide().asInstanceOf[VirtualLong].value.intValue))
              case D2I => memory.push(VirtualInteger(memory.popWide().asInstanceOf[VirtualDouble].value.intValue))
              case F2I => memory.push(VirtualInteger(memory.popAs[VirtualFloat].value.intValue))
              case D2F => memory.push(VirtualFloat(memory.popWide().asInstanceOf[VirtualDouble].value.floatValue))
              case L2F => memory.push(VirtualFloat(memory.popWide().asInstanceOf[VirtualLong].value.floatValue))
              case I2F => memory.push(VirtualFloat(memory.popAs[VirtualInteger].value.floatValue))
              case L2D =>
                memory.push(VirtualDouble(memory.popWide().asInstanceOf[VirtualLong].value.doubleValue))
                memory.push(VirtualTop())
              case I2D =>
                memory.push(VirtualDouble(memory.popAs[VirtualInteger].value.doubleValue))
                memory.push(VirtualTop())
              case F2D =>
                memory.push(VirtualDouble(memory.popAs[VirtualFloat].value.doubleValue))
                memory.push(VirtualTop())
              case D2L =>
                memory.push(VirtualLong(memory.popWide().asInstanceOf[VirtualDouble].value.longValue))
                memory.push(VirtualTop())
              case I2L =>
                memory.push(VirtualLong(memory.popAs[VirtualInteger].value.longValue))
                memory.push(VirtualTop())
              case F2L =>
                memory.push(VirtualLong(memory.popAs[VirtualFloat].value.longValue))
                memory.push(VirtualTop())
              case I2B | I2C =>

              case AALOAD | BALOAD | SALOAD | CALOAD | IALOAD | FALOAD | LALOAD | DALOAD =>
                val arrayIndex = memory.popAs[VirtualInteger].value
                val reference = memory.popAs[VirtualArray]
                val value = reference.value(arrayIndex)
                memory.push(value)
                if (value.isWide) {
                  memory.push(VirtualTop())
                }
              case AASTORE | BASTORE | SASTORE | CASTORE | IASTORE | FASTORE | LASTORE | DASTORE =>
                val poppedValue = if (bytecodeOpcode == LASTORE || bytecodeOpcode == DASTORE) {
                  memory.popWide()
                } else {
                  memory.pop()
                }
                val arrayIndex = memory.popAs[VirtualInteger].value
                val reference = memory.popAs[VirtualArray]

                reference.value(arrayIndex) = poppedValue

              case IADD =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 + value2))
              case ISUB =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 - value2))
              case IMUL =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 * value2))
              case IDIV =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 / value2))
              case ISHL =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 << value2))
              case ISHR =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 >> value2))
              case IUSHR =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 >>> value2))
              case IOR =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 | value2))
              case IAND =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 & value2))
              case IXOR =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 ^ value2))
              case IREM =>
                val value2 = memory.popAs[VirtualInteger].value
                val value1 = memory.popAs[VirtualInteger].value
                memory.push(VirtualInteger(value1 % value2))
              case INEG =>
                memory.push(VirtualInteger(-memory.popAs[VirtualInteger].value))

              case LADD =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 + value2))
                memory.push(VirtualTop())
              case LSUB =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 - value2))
                memory.push(VirtualTop())
              case LMUL =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 * value2))
                memory.push(VirtualTop())
              case LDIV =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 / value2))
                memory.push(VirtualTop())
              case LSHL =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 << value2))
                memory.push(VirtualTop())
              case LSHR =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 >> value2))
                memory.push(VirtualTop())
              case LUSHR =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 >>> value2))
                memory.push(VirtualTop())
              case LOR =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 | value2))
                memory.push(VirtualTop())
              case LXOR =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 ^ value2))
                memory.push(VirtualTop())
              case LAND =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 & value2))
                memory.push(VirtualTop())
              case LREM =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value
                memory.push(VirtualLong(value1 % value2))
                memory.push(VirtualTop())
              case LNEG =>
                memory.push(VirtualLong(-memory.popWide().asInstanceOf[VirtualLong].value))
                memory.push(VirtualTop())


              case FADD =>
                val value2 = memory.popAs[VirtualFloat].value
                val value1 = memory.popAs[VirtualFloat].value
                memory.push(VirtualFloat(value1 + value2))
              case FSUB =>
                val value2 = memory.popAs[VirtualFloat].value
                val value1 = memory.popAs[VirtualFloat].value
                memory.push(VirtualFloat(value1 - value2))
              case FMUL =>
                val value2 = memory.popAs[VirtualFloat].value
                val value1 = memory.popAs[VirtualFloat].value
                memory.push(VirtualFloat(value1 * value2))
              case FDIV =>
                val value2 = memory.popAs[VirtualFloat].value
                val value1 = memory.popAs[VirtualFloat].value
                memory.push(VirtualFloat(value1 / value2))
              case FREM =>
                val value2 = memory.popAs[VirtualFloat].value
                val value1 = memory.popAs[VirtualFloat].value
                memory.push(VirtualFloat(value1 % value2))
              case FNEG =>
                memory.push(VirtualFloat(-memory.popAs[VirtualFloat].value))

              case DADD =>
                val value2 = memory.popWide().asInstanceOf[VirtualDouble].value
                val value1 = memory.popWide().asInstanceOf[VirtualDouble].value
                memory.push(VirtualDouble(value1 + value2))
                memory.push(VirtualTop())
              case DSUB =>
                val value2 = memory.popWide().asInstanceOf[VirtualDouble].value
                val value1 = memory.popWide().asInstanceOf[VirtualDouble].value
                memory.push(VirtualDouble(value1 - value2))
                memory.push(VirtualTop())
              case DMUL =>
                val value2 = memory.popWide().asInstanceOf[VirtualDouble].value
                val value1 = memory.popWide().asInstanceOf[VirtualDouble].value
                memory.push(VirtualDouble(value1 * value2))
                memory.push(VirtualTop())
              case DDIV =>
                val value2 = memory.popWide().asInstanceOf[VirtualDouble].value
                val value1 = memory.popWide().asInstanceOf[VirtualDouble].value
                memory.push(VirtualDouble(value1 / value2))
                memory.push(VirtualTop())
              case DREM =>
                val value2 = memory.popWide().asInstanceOf[VirtualDouble].value
                val value1 = memory.popWide().asInstanceOf[VirtualDouble].value
                memory.push(VirtualDouble(value1 % value2))
                memory.push(VirtualTop())
              case DNEG =>
                memory.push(VirtualDouble(-memory.popWide().asInstanceOf[VirtualDouble].value))
                memory.push(VirtualTop())

              case ARRAYLENGTH =>
                memory.push(VirtualInteger(memory.popAs[VirtualArray].value.length))

              case ICONST_M1 | ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 =>
                memory.push(VirtualInteger(bytecodeOpcode.opcode - ICONST_0.opcode))
              case FCONST_0 | FCONST_1 | FCONST_2 =>
                memory.push(VirtualFloat((bytecodeOpcode.opcode - FCONST_0.opcode).floatValue))
              case LCONST_0 | LCONST_1 =>
                memory.push(VirtualLong((bytecodeOpcode.opcode - LCONST_0.opcode).longValue))
                memory.push(VirtualTop())
              case DCONST_0 | DCONST_1 =>
                memory.push(VirtualDouble((bytecodeOpcode.opcode - DCONST_0.opcode).doubleValue))
                memory.push(VirtualTop())

              case SWAP =>
                val pop1 = memory.pop()
                val pop2 = memory.pop()
                memory.push(pop1)
                memory.push(pop2)

              case DUP =>
                val pop1 = memory.pop()
                memory.push(pop1)
                memory.push(pop1)
              case DUP_X1 =>
                val pop1 = memory.pop()
                val pop2 = memory.pop()
                memory.push(pop1)
                memory.push(pop2)
                memory.push(pop1)
              case DUP_X2 =>
                val pop1 = memory.pop()
                val pop2 = memory.pop()
                val pop3 = memory.pop()
                memory.push(pop1)
                memory.push(pop3)
                memory.push(pop2)
                memory.push(pop1)
              case DUP2 =>
                val pop1 = memory.pop()
                val pop2 = memory.pop()
                memory.push(pop2)
                memory.push(pop1)
                memory.push(pop2)
                memory.push(pop1)
              case DUP2_X1 =>
                val pop1 = memory.pop()
                val pop2 = memory.pop()
                val pop3 = memory.pop()
                memory.push(pop2)
                memory.push(pop1)
                memory.push(pop3)
                memory.push(pop2)
                memory.push(pop1)
              case DUP2_X2 =>
                val pop1 = memory.pop()
                val pop2 = memory.pop()
                val pop3 = memory.pop()
                val pop4 = memory.pop()
                memory.push(pop2)
                memory.push(pop1)
                memory.push(pop4)
                memory.push(pop3)
                memory.push(pop2)
                memory.push(pop1)

              case POP =>
                memory.pop()
              case POP2 =>
                memory.popWide()

              case LCMP =>
                val value2 = memory.popWide().asInstanceOf[VirtualLong].value
                val value1 = memory.popWide().asInstanceOf[VirtualLong].value

                memory.push(VirtualInteger(value1.compareTo(value2)))

              case DCMPG | DCMPL =>
                val value2 = memory.popWide().asInstanceOf[VirtualDouble].value
                val value1 = memory.popWide().asInstanceOf[VirtualDouble].value
                if (value1.isNaN || value2.isNaN) {
                  memory.push(VirtualInteger(if (bytecodeOpcode == DCMPG) -1 else 1))
                } else {
                  memory.push(VirtualInteger(value1.compareTo(value2)))
                }

              case FCMPG | FCMPL =>
                val value2 = memory.popAs[VirtualFloat].value
                val value1 = memory.popAs[VirtualFloat].value
                if (value1.isNaN || value2.isNaN) {
                  memory.push(VirtualInteger(if (bytecodeOpcode == FCMPG) -1 else 1))
                } else {
                  memory.push(VirtualInteger(value1.compareTo(value2)))
                }

              case RETURN | ARETURN | IRETURN | FRETURN | LRETURN | DRETURN =>
                val returnValue = bytecodeOpcode match {
                  case RETURN => VirtualVoid()
                  case LRETURN | DRETURN => memory.popWide()
                  case _ => memory.pop()
                }
                return ExecutionResult(memory, returnValue, executionContext)

                // TODO: monitor system (dont count on it)
              case MONITORENTER =>
              case MONITOREXIT =>
            }

          case tableSwitchInstruction: TableSwitchInstruction =>
            val key = memory.popAs[VirtualInteger].value
            val switchIndex = key - tableSwitchInstruction.low
            if (switchIndex < tableSwitchInstruction.table.size && switchIndex >= 0) {
              index = instructions.indexOf(tableSwitchInstruction.table(switchIndex)) - 1
            } else {
              index = instructions.indexOf(tableSwitchInstruction.default) - 1
            }

          case TypeInstruction(bytecodeOpcode, typeName) =>
            bytecodeOpcode match {
              case INSTANCEOF =>
                val reference = memory.popAs[VirtualObject]
                memory.push(reference.isInstanceOf(classLoader.loadClass(typeName)))
              case NEW =>
                memory.push(VirtualObject(classLoader.loadClass(typeName)))
              case CHECKCAST =>
                memory.push(memory.popAs[VirtualObject].checkCast(classLoader.loadClass(typeName)))
            }
          case variableInstruction: VariableInstruction =>
            if (variableInstruction.isLoad) {
              memory.push(memory.load(variableInstruction.index))
            } else variableInstruction.bytecodeOpcode match {
              case DSTORE | LSTORE =>
                memory.store(variableInstruction.index, memory.popWide())
                memory.store(variableInstruction.index + 1, VirtualTop())
              case RET => // TODO: implement JSR
              case _ =>
              //  println(instructions(index - 1))
                memory.store(variableInstruction.index, memory.pop())
            }
          case _ =>
        }
        index += 1
        if (index >= instructions.size) continue = false
      }
    }

    ExecutionResult(memory, VirtualVoid(), executionContext)
  }


  def intern(string: VirtualObject): VirtualObject =
    getInternedString(string.convertToString)

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
