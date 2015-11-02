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
    { tag     :: Word8
    , ci_info :: Constant
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
    , ai_info              :: [Word8] -- TODO: Fix this
    } deriving (Show)

data Attribute = A_ConstantValue { constantvalue_index :: Word16 }

               | A_Code { max_stack :: Word16
                        , max_locals :: Word16
                        , code_length     :: Word32
                        , code            :: [Word8]
                        , exception_table_length :: Word16
                        , exception_table        :: [Exception_Table]
                        , code_attributes_count  :: Word16
                        , code_attributes        :: [Attribute_Info] }

               | A_StackMapTable { number_of_entries :: Word16
                                 , entries           :: [StackMapFrame] }

data Exception_Table = Exception_Table
    { start_pc   :: Word16
    , end_pc     :: Word16
    , handler_pc :: Word16
    , catch_type :: Word16
    } deriving (Show)

data Verification_Type_Info =
