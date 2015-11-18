module Instructions where

import qualified Data.ByteString.Lazy as B
import           Data.Word


data Instruction = Instruction
    { operation :: String
    , input     :: [Word8]
    } deriving (Show)

parseCode :: [Word8] -> [Instruction]
parseCode []          = []
parseCode (0xab:code) = let (i,c) = parseLookupSwitch code in i : parseCode c
parseCode (0xaa:code) = let (i,c) = parseTableSwitch code in i : parseCode c
parseCode (0xc4:code) = let (i,c) = parseWide code in i : parseCode c
parseCode (op:code)   = let (i,c) = splitAt bytes code in Instruction ins i : parseCode c
    where (ins,bytes) = byteToOp op

parseLookupSwitch :: [Word8] -> (Instruction,[Word8])
parseLookupSwitch = undefined

parseTableSwitch :: [Word8] -> (Instruction,[Word8])
parseTableSwitch = undefined

parseWide :: [Word8] -> (Instruction,[Word8])
parseWide = undefined

byteToOp :: Word8 -> (String,Int)
byteToOp 0x32 = ("aaload",0)
byteToOp 0x53 = ("aastore",0)
byteToOp 0x1  = ("aconst_null",0)
byteToOp 0x19 = ("aload",1)
byteToOp 0x2a = ("aload_0",0)
byteToOp 0x2b = ("aload_1",0)
byteToOp 0x2c = ("aload_2",0)
byteToOp 0x2d = ("aload_3",0)
byteToOp 0xbd = ("anewarray",2)
byteToOp 0xb0 = ("areturn",0)
byteToOp 0xbe = ("arraylength",0)
byteToOp 0x3a = ("astore",1)
byteToOp 0x4b = ("astore_0",0)
byteToOp 0x4c = ("astore_1",0)
byteToOp 0x4d = ("astore_2",0)
byteToOp 0x4e = ("astore_3",0)
byteToOp 0xbf = ("athrow",0)
byteToOp 0x33 = ("baload",0)
byteToOp 0x54 = ("bastore",0)
byteToOp 0x10 = ("bipush",1)
byteToOp 0x34 = ("caload",0)
byteToOp 0x55 = ("castore",0)
byteToOp 0xc0 = ("checkcast",2)
byteToOp 0x90 = ("d2f",0)
byteToOp 0x8e = ("d2i",0)
byteToOp 0x8f = ("d2l",0)
byteToOp 0x63 = ("dadd",0)
byteToOp 0x31 = ("daload",0)
byteToOp 0x52 = ("dastore",0)
byteToOp 0x98 = ("dcmpg",0)
byteToOp 0x97 = ("dcmpl",0)
byteToOp 0xe  = ("dconst_0",0)
byteToOp 0xf  = ("dconst_1",0)
byteToOp 0x6f = ("ddiv",0)
byteToOp 0x18 = ("dload",1)
byteToOp 0x26 = ("dload_0",0)
byteToOp 0x27 = ("dload_1",0)
byteToOp 0x28 = ("dload_2",0)
byteToOp 0x29 = ("dload_3",0)
byteToOp 0x6b = ("dmul",0)
byteToOp 0x77 = ("dneg",0)
byteToOp 0x73 = ("drem",0)
byteToOp 0xaf = ("dreturn",0)
byteToOp 0x39 = ("dstore",1)
byteToOp 0x47 = ("dstore_0",0)
byteToOp 0x48 = ("dstore_1",0)
byteToOp 0x49 = ("dstore_2",0)
byteToOp 0x4a = ("dstore_3",0)
byteToOp 0x67 = ("dsub",0)
byteToOp 0x59 = ("dup",0)
byteToOp 0x5a = ("dup_x1",0)
byteToOp 0x5b = ("dup_x2",0)
byteToOp 0x5c = ("dup2",0)
byteToOp 0x5d = ("dup2_x1",0)
byteToOp 0x5e = ("dup2_x2",0)
byteToOp 0x8d = ("f2d",0)
byteToOp 0x8b = ("f2i",0)
byteToOp 0x8c = ("f2l",0)
byteToOp 0x62 = ("fadd",0)
byteToOp 0x30 = ("faload",0)
byteToOp 0x51 = ("fastore",0)
byteToOp 0x96 = ("fcmpg",0)
byteToOp 0x95 = ("fcmpl",0)
byteToOp 0xb  = ("fconst_0",0)
byteToOp 0xc  = ("fconst_1",0)
byteToOp 0xd  = ("fconst_2",0)
byteToOp 0x6e = ("fdiv",0)
byteToOp 0x17 = ("fload",1)
byteToOp 0x22 = ("fload_0",0)
byteToOp 0x23 = ("fload_1",0)
byteToOp 0x24 = ("fload_2",0)
byteToOp 0x25 = ("fload_3",0)
byteToOp 0x6a = ("fmul",0)
byteToOp 0x76 = ("fneg",0)
byteToOp 0x72 = ("frem",0)
byteToOp 0xae = ("freturn",0)
byteToOp 0x38 = ("fstore",1)
byteToOp 0x43 = ("fstore_0",0)
byteToOp 0x44 = ("fstore_1",0)
byteToOp 0x45 = ("fstore_2",0)
byteToOp 0x46 = ("fstore_3",0)
byteToOp 0x66 = ("fsub",0)
byteToOp 0xb4 = ("getfield",2)
byteToOp 0xb2 = ("getstatic",2)
byteToOp 0xa7 = ("goto",2)
byteToOp 0xc8 = ("goto_w",4)
byteToOp 0x91 = ("i2b",0)
byteToOp 0x92 = ("i2c",0)
byteToOp 0x87 = ("i2d",0)
byteToOp 0x86 = ("i2f",0)
byteToOp 0x85 = ("i2l",0)
byteToOp 0x93 = ("i2s",0)
byteToOp 0x60 = ("iadd",0)
byteToOp 0x2e = ("iaload",0)
byteToOp 0x7e = ("iand",0)
byteToOp 0x4f = ("iastore",0)
byteToOp 0x2  = ("iconst_m1",0)
byteToOp 0x3  = ("iconst_0",0)
byteToOp 0x4  = ("iconst_1",0)
byteToOp 0x5  = ("iconst_2",0)
byteToOp 0x6  = ("iconst_3",0)
byteToOp 0x7  = ("iconst_4",0)
byteToOp 0x8  = ("iconst_5",0)
byteToOp 0x6c = ("idiv",0)
byteToOp 0xa5 = ("if_acmpeq",2)
byteToOp 0xa6 = ("if_acmpne",2)
byteToOp 0x9f = ("if_icmpeq",2)
byteToOp 0xa0 = ("if_icmpne",2)
byteToOp 0xa1 = ("if_icmplt",2)
byteToOp 0xa2 = ("if_icmpge",2)
byteToOp 0xa3 = ("if_icmpgt",2)
byteToOp 0xa4 = ("if_icmple",2)
byteToOp 0x99 = ("ifeq",2)
byteToOp 0x9a = ("ifne",2)
byteToOp 0x9b = ("iflt",2)
byteToOp 0x9c = ("ifge",2)
byteToOp 0x9d = ("ifgt",2)
byteToOp 0x9e = ("ifle",2)
byteToOp 0xc7 = ("ifnonnull",2)
byteToOp 0xc6 = ("ifnull",2)
byteToOp 0x84 = ("iinc",2)
byteToOp 0x15 = ("iload",1)
byteToOp 0x1a = ("iload_0",0)
byteToOp 0x1b = ("iload_1",0)
byteToOp 0x1c = ("iload_2",0)
byteToOp 0x1d = ("iload_3",0)
byteToOp 0x68 = ("imul",0)
byteToOp 0x74 = ("ineg",0)
byteToOp 0xc1 = ("instanceof",2)
byteToOp 0xba = ("invokedynamic",4)
byteToOp 0xb9 = ("invokeinterface",4)
byteToOp 0xb7 = ("invokespecial",2)
byteToOp 0xb8 = ("invokestatic",2)
byteToOp 0xb6 = ("invokevirtual",2)
byteToOp 0x80 = ("ior",0)
byteToOp 0x70 = ("irem",0)
byteToOp 0xac = ("ireturn",0)
byteToOp 0x78 = ("ishl",0)
byteToOp 0x7a = ("ishr",0)
byteToOp 0x36 = ("istore",1)
byteToOp 0x3b = ("istore_0",0)
byteToOp 0x3c = ("istore_1",0)
byteToOp 0x3d = ("istore_2",0)
byteToOp 0x3e = ("istore_3",0)
byteToOp 0x64 = ("isub",0)
byteToOp 0x7c = ("iushr",0)
byteToOp 0x82 = ("ixor",0)
byteToOp 0xa8 = ("jsr",2)
byteToOp 0xc9 = ("jsr_w",4)
byteToOp 0x8a = ("l2d",0)
byteToOp 0x89 = ("l2f",0)
byteToOp 0x88 = ("l2i",0)
byteToOp 0x61 = ("ladd",0)
byteToOp 0x2f = ("laload",0)
byteToOp 0x7f = ("land",0)
byteToOp 0x50 = ("lastore",0)
byteToOp 0x94 = ("lcmp",0)
byteToOp 0x9  = ("lconst_0",0)
byteToOp 0xa  = ("lconst_1",0)
byteToOp 0x12 = ("ldc",1)
byteToOp 0x13 = ("ldc_w",2)
byteToOp 0x14 = ("ldc2_w",2)
byteToOp 0x6d = ("ldiv",0)
byteToOp 0x16 = ("lload",1)
byteToOp 0x1e = ("lload_0",0)
byteToOp 0x1f = ("lload_1",0)
byteToOp 0x20 = ("lload_2",0)
byteToOp 0x21 = ("lload_3",0)
byteToOp 0x69 = ("lmul",0)
byteToOp 0x75 = ("lneg",0)
byteToOp 0xab = ("lookupswitch",-1)
byteToOp 0x81 = ("lor",0)
byteToOp 0x71 = ("lrem",0)
byteToOp 0xad = ("lreturn",0)
byteToOp 0x79 = ("lshl",0)
byteToOp 0x7b = ("lshr",0)
byteToOp 0x37 = ("lstore",1)
byteToOp 0x3f = ("lstore_0",0)
byteToOp 0x40 = ("lstore_1",0)
byteToOp 0x41 = ("lstore_2",0)
byteToOp 0x42 = ("lstore_3",0)
byteToOp 0x65 = ("lsub",0)
byteToOp 0x7d = ("lushr",0)
byteToOp 0x83 = ("lxor",0)
byteToOp 0xc2 = ("monitorenter",0)
byteToOp 0xc3 = ("monitorexit",0)
byteToOp 0xc5 = ("multianewarray",3)
byteToOp 0xbb = ("new",2)
byteToOp 0xbc = ("newarray",1)
byteToOp 0x0  = ("nop",0)
byteToOp 0x57 = ("pop",0)
byteToOp 0x58 = ("pop2",0)
byteToOp 0xb5 = ("putfield",2)
byteToOp 0xb3 = ("putstatic",2)
byteToOp 0xa9 = ("ret",1)
byteToOp 0xb1 = ("return",0)
byteToOp 0x35 = ("saload",0)
byteToOp 0x56 = ("sastore",0)
byteToOp 0x11 = ("sipush",2)
byteToOp 0x5f = ("swap",0)
byteToOp 0xaa = ("tableswitch",-1)
byteToOp 0xc4 = ("wide",-1)
byteToOp b    = error $ "Unrecognized opcode: " ++ (show b)
