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
    fields <- parseFields cp fc
    mc     <- getWord16be
    methds <- parseMethods cp mc
    ac     <- getWord16be
    atts   <- parseAttributes cp ac
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

lookupConstant :: Integral a => [CP_Info] -> a -> Constant
lookupConstant cp index = if intdex > length cp then error $ (show intdex) ++ " is not a valid constant pool index"
                                               else ci_info $ cp !! intdex
    where intdex = fromIntegral index :: Int

parseFields :: Integral a => [CP_Info] -> a -> Get [Field_Info]
parseFields _  0 = return []
parseFields cp n = do
    af   <- getWord16be
    ni   <- getWord16be
    di   <- getWord16be
    ac   <- getWord16be
    atts <- parseAttributes cp ac
    rest <- parseFields cp (n - 1)
    return $ Field_Info af ni di ac atts : rest

parseMethods :: Integral a => [CP_Info] -> a -> Get [Method_Info]
parseMethods _  0 = return []
parseMethods cp n = do
    af   <- getWord16be
    ni   <- getWord16be
    di   <- getWord16be
    ac   <- getWord16be
    atts <- parseAttributes cp ac
    rest <- parseMethods cp (n - 1)
    return $ Method_Info af ni di ac atts : rest

parseAttributes :: Integral a => [CP_Info] -> a -> Get [Attribute_Info]
parseAttributes _  0 = return []
parseAttributes cp n = do
    ni   <- getWord16be
    len  <- getWord32be
    info <- parseAttribute cp (lookupConstant cp ni)
    rest <- parseAttributes cp (n - 1)
    return $ Attribute_Info ni len info : rest

parseAttribute :: [CP_Info] -> Constant -> Get Attribute
parseAttribute cp (C_Utf8 _ str) = case str of
                                     "ConstantValue" -> A_ConstantValue <$> getWord16be
                                     "Code" -> do
                                         ms   <- getWord16be
                                         ml   <- getWord16be
                                         cl   <- getWord32be
                                         code <- getLazyByteString (fromIntegral cl :: Int64)
                                         etl  <- getWord16be
                                         et   <- parseExceptionTable etl
                                         cac  <- getWord16be
                                         ca   <- parseAttributes cp cac
                                         return $ A_Code ms ml cl code etl et cac ca
                                     "StackMapTable" -> error $ B.unpack str
                                     "Exceptions" -> error $ B.unpack str
                                     "InnerClasses" -> error $ B.unpack str
                                     "EnclosingMethod" -> error $ B.unpack str
                                     "Synthetic" -> error $ B.unpack str
                                     "Signature" -> error $ B.unpack str
                                     "SourceFile" -> error $ B.unpack str
                                     "SourceDebugExtension" -> error $ B.unpack str
                                     "LineNumberTable" -> error $ B.unpack str
                                     "LocalVariableTable" -> error $ B.unpack str
                                     "LocalVariableTypeTable" -> error $ B.unpack str
                                     "Deprecated" -> error $ B.unpack str
                                     "RuntimeVisibleAnnotations" -> error $ B.unpack str
                                     "RuntimeInvisibleAnnotations" -> error $ B.unpack str
                                     "RuntimeVisibleParameterAnnotations" -> error $ B.unpack str
                                     "RuntimeInvisibleParameterAnnotations" -> error $ B.unpack str
                                     "RuntimeVisibleTypeAnnotations" -> error $ B.unpack str
                                     "RuntimeInvisibleTypeAnnotations" -> error $ B.unpack str
                                     "AnnotationDefault" -> error $ B.unpack str
                                     "BootstrapMethods" -> error $ B.unpack str
                                     "MethodParameters" -> error $ B.unpack str
                                     _ -> error $ B.unpack str
parseAttribute _  _              = error "Attribute name index did not point to Utf8 constant"

parseExceptionTable :: Integral a => a -> Get [Exception_Table]
parseExceptionTable 0 = return []
parseExceptionTable n = do
    sp   <- getWord16be
    ep   <- getWord16be
    hp   <- getWord16be
    ct   <- getWord16be
    rest <- parseExceptionTable (n - 1)
    return $ Exception_Table sp ep hp ct : rest

showBytes :: B.ByteString -> String
showBytes s = concatMap (`showHex` "") (B.unpack s)

bytesToInt :: B.ByteString -> Int
bytesToInt s = read $ '0' : 'x' : showBytes s
