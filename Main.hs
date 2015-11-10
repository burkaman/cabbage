{-# LANGUAGE OverloadedStrings #-}

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Word

import Debug.Trace

import GHC.Int
import GHC.Stack

import Numeric

import Structures

main :: IO ()
main = do
    file <- B.readFile "test/HelloWorld.class"
    let cl = runGet parseFile file
    putStrLn $ show cl

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
lookupConstant cp index = if intdex > length cp then errorWithStackTrace $ (show intdex) ++ " is not a valid constant pool index"
                                               else ci_info $ cp !! (intdex - 1)
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
                                     "StackMapTable" -> errorWithStackTrace $ show str
                                     "Exceptions" -> errorWithStackTrace $ show str
                                     "InnerClasses" -> errorWithStackTrace $ show str
                                     "EnclosingMethod" -> errorWithStackTrace $ show str
                                     "Synthetic" -> errorWithStackTrace $ show str
                                     "Signature" -> errorWithStackTrace $ show str
                                     "SourceFile" -> A_SourceFile <$> getWord16be
                                     "SourceDebugExtension" -> errorWithStackTrace $ show str
                                     "LineNumberTable" -> do
                                         len <- getWord16be
                                         lnt <- parseLineNumberTable len
                                         return $ A_LineNumberTable len lnt
                                     "LocalVariableTable" -> errorWithStackTrace $ show str
                                     "LocalVariableTypeTable" -> errorWithStackTrace $ show str
                                     "Deprecated" -> errorWithStackTrace $ show str
                                     "RuntimeVisibleAnnotations" -> errorWithStackTrace $ show str
                                     "RuntimeInvisibleAnnotations" -> errorWithStackTrace $ show str
                                     "RuntimeVisibleParameterAnnotations" -> errorWithStackTrace $ show str
                                     "RuntimeInvisibleParameterAnnotations" -> errorWithStackTrace $ show str
                                     "RuntimeVisibleTypeAnnotations" -> errorWithStackTrace $ show str
                                     "RuntimeInvisibleTypeAnnotations" -> errorWithStackTrace $ show str
                                     "AnnotationDefault" -> errorWithStackTrace $ show str
                                     "BootstrapMethods" -> errorWithStackTrace $ show str
                                     "MethodParameters" -> errorWithStackTrace $ show str
                                     _ -> errorWithStackTrace $ show str
parseAttribute _  c              = errorWithStackTrace $ "Attribute name index did not point to Utf8 constant. Constant: " ++ (show c)

parseExceptionTable :: Integral a => a -> Get [ExceptionTableEntry]
parseExceptionTable 0 = return []
parseExceptionTable n = do
    sp   <- getWord16be
    ep   <- getWord16be
    hp   <- getWord16be
    ct   <- getWord16be
    rest <- parseExceptionTable (n - 1)
    return $ ExceptionTableEntry sp ep hp ct : rest

parseLineNumberTable :: Integral a => a -> Get [LineNumberEntry]
parseLineNumberTable 0 = return []
parseLineNumberTable n = do
    sp   <- getWord16be
    ln   <- getWord16be
    rest <- parseLineNumberTable (n - 1)
    return $ LineNumberEntry sp ln : rest

showBytes :: B.ByteString -> String
showBytes s = concatMap (`showHex` "") (B.unpack s)

bytesToInt :: B.ByteString -> Int
bytesToInt s = read $ '0' : 'x' : showBytes s
