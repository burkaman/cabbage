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
import ShowStructures

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
    return $ ClassFile minor major cp afs tc sc ints fields methds atts

getNWord16be :: Integral a => a -> Get [Word16]
getNWord16be 0 = return []
getNWord16be n = do
    word <- getWord16be
    rest <- getNWord16be (n - 1)
    return $ word:rest

parseConstantPool :: Integral a => a -> Get Constant_Pool
parseConstantPool 0 = return []
parseConstantPool n = do
    tag  <- getWord8
    info <- parseConstant tag
    rest <- parseConstantPool (n - 1)
    return $ info : rest

parseConstant :: Integral a => a -> Get Constant
parseConstant 1  = do
    len <- getWord16be
    str <- getLazyByteString (fromIntegral len :: Int64)
    return $ C_Utf8 str
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

lookupConstant :: Integral a => Constant_Pool -> a -> Constant
lookupConstant cp index = if intdex > Prelude.length cp then error $ (show intdex) ++ " is not a valid constant pool index"
                                               else cp !! (intdex - 1)
    where intdex = fromIntegral index :: Int

parseFields :: Integral a => Constant_Pool -> a -> Get [Field_Info]
parseFields _  0 = return []
parseFields cp n = do
    af   <- getWord16be
    ni   <- getWord16be
    di   <- getWord16be
    ac   <- getWord16be
    atts <- parseAttributes cp ac
    rest <- parseFields cp (n - 1)
    return $ Field_Info af ni di atts : rest

parseMethods :: Integral a => Constant_Pool -> a -> Get [Method_Info]
parseMethods _  0 = return []
parseMethods cp n = do
    af   <- getWord16be
    ni   <- getWord16be
    di   <- getWord16be
    ac   <- getWord16be
    atts <- parseAttributes cp ac
    rest <- parseMethods cp (n - 1)
    return $ Method_Info af ni di atts : rest

parseAttributes :: Integral a => Constant_Pool -> a -> Get [Attribute_Info]
parseAttributes _  0 = return []
parseAttributes cp n = do
    ni   <- getWord16be
    len  <- getWord32be
    info <- parseAttribute cp (lookupConstant cp ni)
    rest <- parseAttributes cp (n - 1)
    return $ Attribute_Info ni info : rest

parseAttribute :: Constant_Pool -> Constant -> Get Attribute
parseAttribute cp (C_Utf8 str) = case str of
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
                                         return $ A_Code ms ml code et ca
                                     "StackMapTable" -> error $ show str
                                     "Exceptions" -> error $ show str
                                     "InnerClasses" -> error $ show str
                                     "EnclosingMethod" -> error $ show str
                                     "Synthetic" -> error $ show str
                                     "Signature" -> error $ show str
                                     "SourceFile" -> A_SourceFile <$> getWord16be
                                     "SourceDebugExtension" -> error $ show str
                                     "LineNumberTable" -> do
                                         len <- getWord16be
                                         lnt <- parseLineNumberTable len
                                         return $ A_LineNumberTable lnt
                                     "LocalVariableTable" -> error $ show str
                                     "LocalVariableTypeTable" -> error $ show str
                                     "Deprecated" -> error $ show str
                                     "RuntimeVisibleAnnotations" -> error $ show str
                                     "RuntimeInvisibleAnnotations" -> error $ show str
                                     "RuntimeVisibleParameterAnnotations" -> error $ show str
                                     "RuntimeInvisibleParameterAnnotations" -> error $ show str
                                     "RuntimeVisibleTypeAnnotations" -> error $ show str
                                     "RuntimeInvisibleTypeAnnotations" -> error $ show str
                                     "AnnotationDefault" -> error $ show str
                                     "BootstrapMethods" -> error $ show str
                                     "MethodParameters" -> error $ show str
                                     _ -> error $ show str
parseAttribute _  c              = error $ "Attribute name index did not point to Utf8 constant. Constant: " ++ (show c)

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
