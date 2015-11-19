module ShowStructures where

import Data.Bits
import Data.Maybe
import Data.Word

import Text.Printf

import Structures

instance Show ClassFile where
    show x = printf ("Version: %d.%d\n"
                  ++ "Class: %s\n"
                  ++ "Super Class: %s\n"
                  ++ "Constant Pool:%s\n"
                  ++ "Access Flags: %s\n"
                  ++ "Interfaces:%s\n"
                  ++ "Fields:%s\n"
                  ++ "Methods:%s\n"
                  ++ "Attributes:%s\n")
                  (major_version x) (minor_version x)
                  (show $ this_class x)
                  (show $ super_class x)
                  (show $ constant_pool x)
                  (printFlags $ cf_access_flags x)
                  (show $ interfaces x)
                  (show $ fields x)
                  (show $ methods x)
                  (show $ cf_attributes x)

instance Show Method_Info where
    show m = 

printFlags :: Word16 -> String
printFlags f = show . map fromJust . filter isJust . map (bytesToFlag . (f .&.)) $ [0xf000, 0x0f00, 0x00f0, 0x000f]

data Access_Flag = ACC_PUBLIC
                 | ACC_FINAL
                 | ACC_SUPER
                 | ACC_INTERFACE
                 | ACC_ABSTRACT
                 | ACC_SYNTHETIC
                 | ACC_ANNOTATION
                 | ACC_ENUM
                 deriving (Show)

bytesToFlag :: Word16 -> Maybe Access_Flag
bytesToFlag 0x0001 = Just ACC_PUBLIC
bytesToFlag 0x0010 = Just ACC_FINAL
bytesToFlag 0x0020 = Just ACC_SUPER
bytesToFlag 0x0200 = Just ACC_INTERFACE
bytesToFlag 0x0400 = Just ACC_ABSTRACT
bytesToFlag 0x1000 = Just ACC_SYNTHETIC
bytesToFlag 0x2000 = Just ACC_ANNOTATION
bytesToFlag 0x4000 = Just ACC_ENUM
bytesToFlag _      = Nothing
