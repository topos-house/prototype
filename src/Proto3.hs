import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Char



-- Notation

(<:>)  = liftA2 (:)
(<++>) = liftA2 (++)

infixr 5 <:>
infixr 5 <++>

quoted q p = char q *> p `manyTill` char q

-- Identifiers

ident :: Parser String
ident = letter <:> many (letter <|> digit <|> char '_')

fullIdent :: Parser String
fullIdent = concat <$> ident <:> many (char '.' <:> ident)

messageName = ident
enumName    = ident
fieldName   = ident
oneOfName   = ident
mapName     = ident
serviceName = ident
rpcName     = ident

messageType, enumType :: Parser String
[messageType, enumType] = map _Type [messageName, enumName]
 where
  _Type _Name = concat <$> option "" (string ".")
                           <:> many (try $ ident <++> string ".")
                           <++> _Name )

-- Literals

intLit :: Parser Int
intLit = read <$> (decLit <|> try octLit <|> hexLit)
 where
  decLit = oneOf ['1'..'9'] <:> many digit
  octLit = ("0o" ++) <$> char '0' <:> many octDigit
  hexLit = char '0' <:> oneOf "xX" <:> many1 hexDigit

floatLit :: Parser Float
floatLit = read <$> (try case1 <|> case2 <|> case3)
 where
  case1 = decimals <++> string "." <++> option "" decimals <++> option "" exponent
  case2 = decimals <++> exponent
  case3 = char '.' <:> decimals <++> option "" exponent
  decimals = many1 digit
  exponent = oneOf "eE" <:> option "" ((:[]) <$> oneOf "+-") <++> decimals

boolLit :: Parser Bool
boolLit = read <$> (string "true" <|> string "false")

strLit :: Parser String
strLit = quoted '"' (many charValue) <|> quoted '\'' (many charValue)
 where
  charValue  = try hexEscape <|> try octEscape <|> charEscape <|> noneOf "\0\n\\"
  hexEscape  = char '\\' <:> oneOf "xX" <:> count 2 hexDigit
  octEscape  = char '\\' <:> count 3 octDigit
  charEscape = sequence [char '\\', oneOf "abfnrtv\\'\""]



syntax :: Parser ()
syntax = string "syntax" >> string "=" >> (string "`proto3`" <|> string "\"proto3\"")

data Import = Weak String
            | Public String

import_ :: Parser Import
import_ = do
  string "import"
  wp <- string "weak" <|> string "public"
  s <- strLit
  char ';'
  return $ case wp of
    "weak"   -> Weak s
    "public" -> Public s

data Package = Package String

package :: Parser Package
package = Package <$> string "package" *> fullIdent <* char ';'

data Option = Option String String

option :: Parser Option
option = do
  string "option"
  n <- optionName
  char '='
  c <- constant
  char ';'
  return $ Option n c
 where
  optionName = concat <$> (ident <|> between (char '(') (char ')') fullIdent) <:> many (char '.' <++> ident)

constant :: Parser String
constant = undefined

data Type = Double | Float | Int32 | Int64 | UInt32 | UInt64| SInt32 | SInt64
          | Fixed32 | Fixed64 | SFixed32 | SFixed64 | Bool | String | Bytes
          | Defined String

type_ :: Parser Type
type_ = toType <$> choice (map (try . string) primTypes) <|> try messageType <|> enumType
 where
  primTypes = [ "double", "float", "int32", "int64", "uint32", "uint64"
              , "sint32", "sint64", "fixed32", "fixed64", "sfixed32", "sfixed64"
              , "bool", "string", "bytes"
              ]
  toType typeStr = case typeStr of
    "double"   -> Double
    "float"    -> Float
    "int32"    -> Int32
    "int64"    -> Int64
    "uint32"   -> UInt32
    "uint64"   -> UInt64
    "sint32"   -> SInt32
    "sint64"   -> SInt64
    "fixed32"  -> Fixed32
    "fixed64"  -> Fixed64
    "sfixed32" -> SFixed32
    "sfixed64" -> SFixed64
    "bool"     -> Bool
    "string"   -> String
    "bytes"    -> Bytes
     other     -> Defined other

data Field = Single   Type String Int [Option]
           | Repeated Type String Int [Option]

field :: Parser Field
field = do
  r <- optionMaybe (string "repeated")
  t <- type_
  n <- fieldName
  char '='
  i <- fieldNumber
  o <- many option
  return $ case r of
    Nothing -> Single   t n i o
    Just _  -> Repeated t n i o

module Proto3 where
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Applicative
-- Notation

(<:>)  = liftA2 (:)
(<++>) = liftA2 (++)

infixr 5 <:>
infixr 5 <++>

quoted :: Char -> Parser a -> Parser a
quoted q p = char q *> p <* char q

-- Identifiers


ident :: Parser String
ident = letter <:> many (letter <|> digit <|> char '_')

fullIdent :: Parser String
fullIdent = concat <$> ident <:> many (char '.' <:> ident)

messageName = ident
enumName    = ident
fieldName   = ident
oneOfName   = ident
mapName     = ident
serviceName = ident
rpcName     = ident

fieldNumber = intLit

messageType, enumType :: Parser String
[messageType, enumType] = map _Type [messageName, enumName]
 where
  _Type _Name = concat <$> ( _Name
                             <:> option "" (string ".")
                             <:> many (try $ ident <++> string ".")
                           )

-- Literals

intLit :: Parser Int
intLit = read <$> (decLit <|> try octLit <|> hexLit)
 where
  decLit = oneOf ['1'..'9'] <:> many digit
  octLit = ("0o" ++) <$> char '0' <:> many octDigit
  hexLit = char '0' <:> oneOf "xX" <:> many1 hexDigit

floatLit :: Parser Float
floatLit = read <$> (try case1 <|> case2 <|> case3)
 where
  case1 = decimals <++> string "." <++> option "" decimals <++> option "" exponent
  case2 = decimals <++> exponent
  case3 = char '.' <:> decimals <++> option "" exponent
  decimals = many1 digit
  exponent = oneOf "eE" <:> option "" ((:[]) <$> oneOf "+-") <++> decimals

boolLit :: Parser Bool
boolLit = read <$> (string "true" <|> string "false")

strLit :: Parser String
strLit = quoted '"' charValue <|> quoted '\'' charValue
 where
  charValue  = try hexEscape <|> try octEscape <|> charEscape <|> many (noneOf "\0\n\\")
  hexEscape  = char '\\' <:> oneOf "xX" <:> count 2 hexDigit
  octEscape  = char '\\' <:> count 3 octDigit
  charEscape = sequence [char '\\', oneOf "abfnrtv\\'\""]

emptyStatement :: Parser String
emptyStatement = string ";"

syntax :: Parser String 
syntax = string "syntax" >> string "=" >> (string "`proto3`" <|> string "\"proto3\"")

data Import = Weak String
            | Public String

import_ :: Parser Import
import_ = do
  string "import"
  wp <- string "weak" <|> string "public"
  s <- strLit
  char ';'
  return $ case wp of
    "weak"   -> Weak s
    "public" -> Public s

data Package = Package String

package :: Parser Package
package = Package <$> (string "package" *> fullIdent <* char ';')

data Option = Option String String

option_ :: Parser Option
option_ = do
  string "option"
  n <- optionName
  char '='
  c <- constant
  char ';'
  return $ Option n c
 where
  optionName = concat <$> (ident <|> between (char '(') (char ')') fullIdent) <:> many (char '.' <:> ident)

constant :: Parser String
constant = undefined

data Type = Double | Float | Int32 | Int64 | UInt32 | UInt64| SInt32 | SInt64
          | Fixed32 | Fixed64 | SFixed32 | SFixed64 | Bool | String | Bytes
          | Defined String

type_ :: Parser Type
type_ = toType <$> (choice (map (try . string) primTypes) <|> try messageType <|> enumType)
 where
  primTypes = [ "double", "float", "int32", "int64", "uint32", "uint64"
              , "sint32", "sint64", "fixed32", "fixed64", "sfixed32", "sfixed64"
              , "bool", "string", "bytes"
              ]
  toType typeStr = case typeStr of
    "double"   -> Double
    "float"    -> Float
    "int32"    -> Int32
    "int64"    -> Int64
    "uint32"   -> UInt32
    "uint64"   -> UInt64
    "sint32"   -> SInt32
    "sint64"   -> SInt64
    "fixed32"  -> Fixed32
    "fixed64"  -> Fixed64
    "sfixed32" -> SFixed32
    "sfixed64" -> SFixed64
    "bool"     -> Bool
    "string"   -> String
    "bytes"    -> Bytes
    other      -> Defined other

data Field = Single   Type String Int [Option]
           | Repeated Type String Int [Option]

field :: Parser Field
field = do
  r <- optionMaybe (string "repeated")
  t <- type_
  n <- fieldName
  char '='
  i <- fieldNumber
  o <- many option_
  return $ case r of
    Nothing -> Single   t n i o
    Just _  -> Repeated t n i o

topLevelDef = messageName <|> enumName <|> serviceName

-- TODO: Top level parser to Syntax tree
-- proto :: Parser Proto3
-- proto = do
--   syn <- syntax
--   statements <- many (import_ <|> package <|> option_ <|> topLevelDef <|> emptyStatement)
--   return Proto3 syn statements

-- data Statement = Import | Package | Option | TopLevelDef | EmptyStatement

-- data Proto3 = Proto3 String [Statement]
