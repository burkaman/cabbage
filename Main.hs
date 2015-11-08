{-# LANGUAGE OverloadedStrings #-}

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Word

import GHC.Int

import Numeric

import Structures

main :: IO ClassFile
main = do
    file <- B.readFile "test/HelloWorld.class"
    return $ runGet parseFile file

parseFile :: Get ClassFile
parseFile = do
    magic  <- getWord32be
    minor  <- getWord16be
    major  <- getWord16be
    cpc    <- getWord16be
    cp     <- parseConstantPool (cpc - 1)
    afs    <- getWord16be
    tc     <- getWord16be
    sc     <- getWord16be
    intc   <- getWord16be
    ints   <- getNWord16be intc
    fc     <- getWord16be
    fields <- parseFields fc
    mc     <- getWord16be
    methds <- parseMethods mc
    ac     <- getWord16be
    atts   <- parseAttributes ac
    return $ ClassFile magic minor major cpc cp afs tc sc intc ints fc fields mc methds ac atts

getNWord16be :: Integral a => a -> Get [Word16]
getNWord16be 0 = return []
getNWord16be n = do
    word <- getWord16be
    rest <- getNWord16be (n - 1)
    return $ word:rest

parseConstantPool :: Integral a => a -> Get [CP_Info]
parseConstantPool 0 = return []
parseConstantPool n = do
    tag  <- getWord8
    info <- parseConstant tag
    rest <- parseConstantPool (n - 1)
    return $ CP_Info tag info : rest

parseConstant :: Integral a => a -> Get Constant
parseConstant 1  = do
    len <- getWord16be
    str <- getLazyByteString (fromIntegral len :: Int64)
    return $ C_Utf8 len str
parseConstant 3  = C_Integer <$> getWord32be
parseConstant 4  = C_Float <$> getWord32be
parseConstant 5  = C_Long <$> getWord32be <*> getWord32be
parseConstant 6  = C_Double <$> getWord32be <*> getWord32be
parseConstant 7  = C_Class <$> getWord16be
parseConstant 8  = C_String <$> getWord16be
parseConstant 9  = C_Fieldref <$> getWord16be <*> getWord16be
parseConstant 10 = C_Methodref <$> getWord16be <*> getWord16be
parseConstant 11 = C_InterfaceMethodref <$> getWord16be <*> getWord16be
parseConstant 12 = C_NameAndType <$> getWord16be <*> getWord16be
parseConstant 15 = C_MethodHandle <$> getWord8 <*> getWord16be
parseConstant 16 = C_MethodType <$> getWord16be
parseConstant 18 = C_InvokeDynamic <$> getWord16be <*> getWord16be
parseConstant x  = undefined

parseFields :: Integral a => a -> Get [Field_Info]
parseFields 0 = return []
parseFields n = do
    af   <- getWord16be
    ni   <- getWord16be
    di   <- getWord16be
    ac   <- getWord16be
    atts <- parseAttributes ac
    rest <- parseFields (n - 1)
    return $ Field_Info af ni di ac atts : rest

parseMethods :: Integral a => a -> Get [Method_Info]
parseMethods 0 = return []
parseMethods n = do
    af   <- getWord16be
    ni   <- getWord16be
    di   <- getWord16be
    ac   <- getWord16be
    atts <- parseAttributes ac
    rest <- parseMethods (n - 1)
    return $ Method_Info af ni di ac atts : rest

parseAttributes :: Integral a => a -> Get [Attribute_Info]
parseAttributes 0 = return []
parseAttributes n = do
    ni   <- getWord16be
    len  <- getWord32be
    --info <- getLazyByteString (fromIntegral len :: Int64)
    info <- parseAttribute
    rest <- parseAttributes (n - 1)
    return $ Attribute_Info ni len info : rest

parseAttribute :: Get Attribute
parseAttribute = undefined

showBytes :: B.ByteString -> String
showBytes s = concatMap (`showHex` "") (B.unpack s)

bytesToInt :: B.ByteString -> Int
bytesToInt s = read $ '0' : 'x' : showBytes s
