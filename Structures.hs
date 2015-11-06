module Structures where

import qualified Data.ByteString.Lazy as B
import Data.Word

data ClassFile = ClassFile
    { magic               :: Word32
    , minor_version       :: Word16
    , major_version       :: Word16
    , constant_pool_count :: Word16
    , constant_pool       :: [CP_Info]
    , access_flags        :: Word16
    , this_class          :: Word16
    , super_class         :: Word16
    , interfaces_count    :: Word16
    , interfaces          :: [Word16]
    , fields_count        :: Word16
    , fields              :: [Field_Info]
    , methods_count       :: Word16
    , methods             :: [Method_Info]
    , attributes_count    :: Word16
    , cf_attributes       :: [Attribute_Info]
    } deriving (Show)
--    } deriving (Show)

data CP_Info = CP_Info
    { ci_tag     :: Word8
    , ci_info    :: Constant
    } deriving (Show)

data Constant = C_Utf8 { utf8_length :: Word16
                       , utf8_bytes :: B.ByteString }

              | C_Integer { integer_bytes :: Word32 }

              | C_Float { float_bytes :: Word32 }

              | C_Long { long_high_bytes :: Word32
                       , long_low_bytes  :: Word32 }

              | C_Double { double_high_bytes :: Word32
                         , double_low_bytes  :: Word32 }

              | C_Class { class_name_index :: Word16 }

              | C_String { string_string_index :: Word16 }

              | C_Fieldref { fieldref_class_index         :: Word16
                           , fieldref_name_and_type_index :: Word16 }

              | C_Methodref { methodref_class_index         :: Word16
                            , methodref_name_and_type_index :: Word16 }

              | C_InterfaceMethodref { interfacemethodref_class_index         :: Word16
                                     , interfacemethodref_name_and_type_index :: Word16 }

              | C_NameAndType { nameandtype_name_index       :: Word16
                              , nameandtype_descriptor_index :: Word16 }

              | C_MethodHandle { methodhandle_reference_kind  :: Word8
                               , methodhandle_reference_index :: Word16 }

              | C_MethodType { methodtype_descriptor_index :: Word16 }

              | C_InvokeDynamic { invokedynamic_bootstrap_method_attr_index :: Word16
                                , invokedynamic_name_and_type_index         :: Word16 }
              deriving (Show)

data Field_Info = Field_Info
    { fi_access_flags     :: Word16
    , fi_name_index       :: Word16
    , fi_descriptor_index :: Word16
    , fi_attributes_count :: Word16
    , fi_attributes       :: [Attribute_Info]
    } deriving (Show)

data Method_Info = Method_Info
    { mi_access_flags     :: Word16
    , mi_name_index       :: Word16
    , mi_descriptor_index :: Word16
    , mi_attributes_count :: Word16
    , mi_attributes       :: [Attribute_Info]
    } deriving (Show)

data Attribute_Info = Attribute_Info
    { ai_attribute_name_index :: Word16
    , ai_attribute_length     :: Word32
    , ai_info                 :: Attribute
    } deriving (Show)

data Attribute = A_ConstantValue { constantvalue_index :: Word16 }

               | A_Code { max_stack              :: Word16
                        , max_locals             :: Word16
                        , code_length            :: Word32
                        , code                   :: [Word8]
                        , exception_table_length :: Word16
                        , exception_table        :: [Exception_Table]
                        , code_attributes_count  :: Word16
                        , code_attributes        :: [Attribute_Info] }

               | A_StackMapTable { number_of_entries :: Word16
                                 , entries           :: [StackMapFrame] }

               | A_Exceptions { number_of_exceptions  :: Word16
                              , exception_index_table :: [Word16]
                              }

               | A_InnerClasses { number_of_classes :: Word16
                                , classes           :: [InnerClass]
                                }

               | A_EnclosingMethod { class_index  :: Word16
                                   , method_index :: Word16
                                   }

               | A_Synthetic

               | A_Signature { signature_index :: Word16 }

               | A_SourceFile { sourcefile_index :: Word16 }

               | A_SourceDebugExtension { debug_extension :: B.ByteString }

               | A_LineNumberTable { line_number_table_length :: Word16
                                   , line_number_table        :: [LineNumberEntry]
                                   }

               | A_LocalVariableTable { local_variable_table_length :: Word16
                                      , local_variable_table        :: [LocalVariableEntry]
                                      }

               | A_LocalVariableTypeTable { local_variable_type_table_length :: Word16
                                          , local_variable_type_table        :: [LocalVariableTypeEntry]
                                          }

               | 



data Exception_Table = Exception_Table
    { start_pc   :: Word16
    , end_pc     :: Word16
    , handler_pc :: Word16
    , catch_type :: Word16
    } deriving (Show)

data Stack_Map_Frame = Stack_Map_Frame
    { smf_tag               :: Stack_Map_Frame_Tag
    , offset_delta          :: Word16
    , number_of_locals      :: Word16
    , locals                :: [Verification_Type_Info]
    , number_of_stack_items :: Word16
    , stack                 :: [Verification_Type_Info]
    } deriving (Show)

data Stack_Map_Frame_Tag = Same
                         | Same_Locals_1_Stack_Item
                         | Same_Locals_1_Stack_Item_Extended
                         | Chop
                         | Same_Extended
                         | Append
                         | Full

data Verification_Type_Info = Verification_Type_Info
    { vti_tag      :: Verification_Type_Info_tag
    , vti_info     :: Word16 -- cpool_index for Object, or offset for Uninitialized

data Verification_Type_Info_tag = Top
                                | Integer
                                | Float
                                | Double
                                | Long
                                | Null
                                | UninitializedThis
                                | Object
                                | Uninitialized
                                deriving (Enum)

data InnerClass = InnerClass
    { inner_class_info_index   :: Word16
    , outer_class_info_index   :: Word16
    , inner_name_index         :: Word16
    , inner_class_access_flags :: Word16
    } deriving (Show)

data LineNumberEntry = LineNumberEntry
    { lne_start_pc    :: Word16
    , lne_line_number :: Word16
    } deriving (Show)

data LocalVariableEntry = LocalVariableEntry
    { lve_start_pc         :: Word16
    , lve_length           :: Word16
    , lve_name_index       :: Word16
    , lve_descriptor_index :: Word16
    , lve_index            :: Word16
    } deriving (Show)

data LocalVariableTypeEntry = LocalVariableTypeEntry
