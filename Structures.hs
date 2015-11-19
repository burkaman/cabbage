{-# LANGUAGE DuplicateRecordFields #-}

module Structures where

import qualified Data.ByteString.Lazy as B
import Data.Word

data ClassFile = ClassFile
    { minor_version :: Word16
    , major_version :: Word16
    , constant_pool :: Constant_Pool
    , cf_access_flags  :: Word16
    , this_class    :: Word16
    , super_class   :: Word16
    , interfaces    :: [Word16]
    , fields        :: [Field_Info]
    , methods       :: [Method_Info]
    , cf_attributes    :: [Attribute_Info]
    }

type Constant_Pool = [Constant]

data Constant = C_Utf8 { string :: B.ByteString }

              | C_Integer { bytes :: Word32 }

              | C_Float { bytes :: Word32 }

              | C_Long { high_bytes :: Word32
                       , low_bytes  :: Word32 }

              | C_Double { high_bytes :: Word32
                         , low_bytes  :: Word32 }

              | C_Class { name_index :: Word16 }

              | C_String { string_index :: Word16 }

              | C_Fieldref { class_index         :: Word16
                           , name_and_type_index :: Word16 }

              | C_Methodref { class_index         :: Word16
                            , name_and_type_index :: Word16 }

              | C_InterfaceMethodref { class_index         :: Word16
                                     , name_and_type_index :: Word16 }

              | C_NameAndType { name_index       :: Word16
                              , descriptor_index :: Word16 }

              | C_MethodHandle { reference_kind  :: Word8
                               , reference_index :: Word16 }

              | C_MethodType { descriptor_index :: Word16 }

              | C_InvokeDynamic { bootstrap_method_attr_index :: Word16
                                , name_and_type_index         :: Word16 }
              deriving (Show)

data Field_Info = Field_Info
    { access_flags     :: Word16
    , name_index       :: Word16
    , descriptor_index :: Word16
    , attributes       :: [Attribute_Info]
    } deriving (Show)

data Method_Info = Method_Info
    { mi_access_flags     :: Word16
    , name_index       :: Word16
    , descriptor_index :: Word16
    , attributes       :: [Attribute_Info]
    } deriving (Show)

data Attribute_Info = Attribute_Info
    { attribute_name_index :: Word16
    , info                 :: Attribute
    } deriving (Show)

data Attribute = A_ConstantValue { index :: Word16 }

               | A_Code { max_stack       :: Word16
                        , max_locals      :: Word16
                        , code            :: B.ByteString
                        , exception_table :: [ExceptionTableEntry]
                        , attributes :: [Attribute_Info] }

               | A_StackMapTable { entries :: [StackMapFrame] }

               | A_Exceptions { exception_index_table :: [Word16] }

               | A_InnerClasses { classes :: [InnerClass] }

               | A_EnclosingMethod { class_index  :: Word16
                                   , method_index :: Word16 }

               | A_Synthetic

               | A_Signature { signature_index :: Word16 }

               | A_SourceFile { sourcefile_index :: Word16 }

               | A_SourceDebugExtension { debug_extension :: B.ByteString }

               | A_LineNumberTable { line_number_table :: [LineNumberEntry] }

               | A_LocalVariableTable { local_variable_table :: [LocalVariableEntry] }

               | A_LocalVariableTypeTable { local_variable_type_table :: [LocalVariableTypeEntry] }

               | A_Deprecated

               | A_RuntimeAnnotations { visible     :: Bool
                                      , annotations :: [Annotation] }

               | A_RuntimeParameterAnnotations { parameter_annotations :: [ParameterAnnotation] }

               | A_RuntimeTypeAnnotations { type_annotations :: [TypeAnnotation] }

               | A_AnnotationDefault { default_value :: Element_Value }

               | A_BootstrapMethods { bootstrap_methods :: [BootstrapMethodEntry] }

               | A_MethodParameters { parameters :: [Parameter] }

               | A_Other { attribute_name :: B.ByteString
                         , attribute_info :: [Word8] }
               deriving (Show)



data ExceptionTableEntry = ExceptionTableEntry
    { start_pc   :: Word16
    , end_pc     :: Word16
    , handler_pc :: Word16
    , catch_type :: Word16
    } deriving (Show)

data StackMapFrame = StackMapFrame
    { tag      :: Stack_Map_Frame_Tag
    , offset_delta :: Word16
    , locals       :: [Verification_Type_Info]
    , stack        :: [Verification_Type_Info]
    } deriving (Show)

data Stack_Map_Frame_Tag = Same
                         | Same_Locals_1_Stack_Item
                         | Same_Locals_1_Stack_Item_Extended
                         | Chop
                         | Same_Extended
                         | Append
                         | Full
                         deriving (Show)

data Verification_Type_Info = Verification_Type_Info
    { tag  :: Verification_Type_Info_tag
    , info :: Word16 -- cpool_index for Object, or offset for Uninitialized
    } deriving (Show)

data Verification_Type_Info_tag = Top
                                | Integer
                                | Float
                                | Double
                                | Long
                                | Null
                                | UninitializedThis
                                | Object
                                | Uninitialized
                                deriving (Show, Enum)

data InnerClass = InnerClass
    { inner_class_info_index   :: Word16
    , outer_class_info_index   :: Word16
    , inner_name_index         :: Word16
    , inner_class_access_flags :: Word16
    } deriving (Show)

data LineNumberEntry = LineNumberEntry
    { start_pc    :: Word16
    , line_number :: Word16
    } deriving (Show)

data LocalVariableEntry = LocalVariableEntry
    { start_pc         :: Word16
    , length           :: Word16
    , name_index       :: Word16
    , descriptor_index :: Word16
    , index            :: Word16
    } deriving (Show)

data LocalVariableTypeEntry = LocalVariableTypeEntry
    { start_pc        :: Word16
    , length          :: Word16
    , name_index      :: Word16
    , signature_index :: Word16
    , index           :: Word16
    } deriving (Show)

data Annotation = Annotation
    { type_index          :: Word16
    , element_value_pairs :: [ElementValuePair]
    } deriving (Show)

data ElementValuePair = ElementValuePair
    { element_name_index :: Word16
    , value              :: Element_Value
    } deriving (Show)

data Element_Value = Element_Value
    { tag   :: ElementValueTag
    , value :: ElementValueItem
    } deriving (Show)

data ElementValueTag = B | C | D | F | I | J | S | Z | Ss | Ee | Cc | Ann | Arr deriving (Show)

data ElementValueItem = Const_Value { const_value_index :: Word16 }

                      | Enum_Const_Value { type_name_index  :: Word16
                                         , const_name_index :: Word16 }

                      | Class_Value { class_info_index :: Word16 }

                      | Annotation_Value { annotation_value :: Annotation }

                      | Array_Value { values :: [Element_Value] }
                      deriving (Show)

data TypeAnnotation = TypeAnnotation
    { target_type         :: Word8
    , target_info         :: TargetInfo
    , target_path         :: TypePath
    , type_index          :: Word16
    , element_value_pairs :: [ElementValuePair]
    } deriving (Show)

data TargetInfo = Type_Parameter { type_parameter_index :: Word8 }

                | Supertype { supertype_index :: Word16 }

                | Type_Parameter_Bound { type_parameter_bound_index :: Word8
                                       , bound_index                :: Word8
                                       }

                | Empty

                | Method_Formal_Parameter { formal_parameter_index :: Word8 }

                | Throws { throws_type_index :: Word16 }

                | LocalVar { localvar_table :: [LocalVarEntry] }

                | Catch { exception_table_index :: Word16 }

                | Offset { offset :: Word16 }

                | Type_Argument { ta_offset              :: Word16
                                , ta_type_argument_index :: Word8 }
                deriving (Show)

data TypePath = TypePath
    { path :: [PathEntry]
    } deriving (Show)

data PathEntry = PathEntry
    { type_path_kind      :: Word8
    , type_argument_index :: Word8
    } deriving (Show)

data ParameterAnnotation = ParameterAnnotation
    { annotations :: [Annotation]
    } deriving (Show)

data LocalVarEntry = LocalVarEntry
    { start_pc :: Word16
    , length   :: Word16
    , index    :: Word16
    } deriving (Show)

data BootstrapMethodEntry = BootstrapMethodEntry
    { bootstrap_method_ref :: Word16
    , bootstrap_arguments  :: [Word16]
    } deriving (Show)

data Parameter = Parameter
    { name_index   :: Word16
    , access_flags :: Word16
    } deriving (Show)
