module ShowStructures where

import Text.Printf

import Structures

instance Show ClassFile where
    show x = printf "Version: %d.%d\n"
                 ++ "Class: %s\n"
                 ++ "Super Class: %s\n"
                 ++ "Constant Pool:\n%s"
                 ++ "Access Flags: %s\n"
                 ++ "Interfaces:\n%s"
                 ++ "Fields:\n%s"
                 ++ "Methods:\n%s"
                 ++ "Attributes:\n%s"
                 (major_version x) (minor_version x)
                 this_class x
                 super_class x
                 (printFlags $ access_flags x)
                 (show $ interfaces x)
                 (show $ fields x)
                 (show $ methods x)
                 (show $ cf_attributes x)
