{-# LANGUAGE FlexibleContexts #-}
module MLogic.HighLevel.Token where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.List hiding (sort)

import Control.Monad

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Monad.Trans

data OpToken = AddT | MinusT | MulT | DivT | ModT | PowT | EqualT | NotEqualT | LessThanT | LessThanEqT | GreaterThanT | GreaterThanEqT | StrictEqualT | StrictNotEqualT | ShlT | ShrT | LogicalAndT | BitwiseAndT | LogicalOrT | BitwiseOrT | XorT | FlipT | NotT deriving (Show, Eq)

data Token = IdT String | AtConstantT String | DoubleLitT Double | StringLitT String
           | CommaT | NullT | TrueT | FalseT | ColonT | SemicolonT | UnderscoreT | OpenParenT | CloseParenT
           | ReservedT String | OpenBracketT | CloseBracketT | OpT OpToken | EqualSignT
           | ArrowT | TargetT String | SortT String | BuildingGroupT String | CommentT deriving (Show, Eq)

data TokenLoc = TokenLoc Token SourcePos

instance Show TokenLoc where
  show (TokenLoc t _) = show t
alpha :: ParsecT ByteString () IO Char
alpha = oneOf (['a'..'z'] <> ['A'..'Z'])

alphaUnderscoreNum :: ParsecT ByteString () IO Char
alphaUnderscoreNum = oneOf (['a'..'z'] <> ['A'..'Z'] <> "_" <> ['0'..'9'])

ident :: ParsecT ByteString () IO String
ident = do
  fc <- alpha
  rest <- many alphaUnderscoreNum
  return (fc:rest)

parseId :: ParsecT ByteString () IO Token
parseId = do
  i <- ident
  return $ IdT i

atConstant :: ParsecT ByteString () IO Token
atConstant = do
  at <- char '@'
  conName <- do
    f <- alpha
    rest <- many alphaUnderscoreNum
    return (f:rest)
  return $ AtConstantT (at:conName)


doubleExponent :: ParsecT ByteString () IO String
doubleExponent = do
  _ <- oneOf "Ee"
  rest <- many digit
  return ('e':rest)

doubleDecimal :: ParsecT ByteString () IO String
doubleDecimal = do
  dot <- char '.'
  rest <- many1 digit
  return (dot:rest)

doubleLit :: ParsecT ByteString () IO Token
doubleLit = do
  f <- many1 digit
  expOrDec <- option "" $ doubleExponent <|> doubleDecimal
  return $ DoubleLitT (read $ f <> expOrDec)
  
stringLit :: ParsecT ByteString () IO Token
stringLit = do
  _ <- char '"'
  str <- many $
         many1 (noneOf "\\\"")
     <|> try (string "\\\\" >> return "\\")
     <|> try (string "\\\"" >> return "\"")
  _ <- char '"'
  return $ StringLitT $ concat str

comma :: ParsecT ByteString () IO Token
comma = do
  _ <- char ','
  return CommaT

nullT :: ParsecT ByteString () IO Token
nullT = do
  _ <- string "null"
  notFollowedBy alphaUnderscoreNum
  return NullT

trueT :: ParsecT ByteString () IO Token
trueT = do
  _ <- string "true"
  notFollowedBy alphaUnderscoreNum
  return TrueT

falseT :: ParsecT ByteString () IO Token
falseT = do
  _ <- string "false"
  notFollowedBy alphaUnderscoreNum
  return FalseT

colon :: ParsecT ByteString () IO Token
colon = do
  _ <- char ':'
  return ColonT

semicolon :: ParsecT ByteString () IO Token
semicolon = do
  _ <- char ';'
  return SemicolonT

underscore :: ParsecT ByteString () IO Token
underscore = do
  _ <- char '_'
  return UnderscoreT

openParen :: ParsecT ByteString () IO Token
openParen = do
  _ <- char '('
  return OpenParenT

closeParen :: ParsecT ByteString () IO Token
closeParen = do
  _ <- char ')'
  return CloseParenT

reserved :: ParsecT ByteString () IO Token
reserved = do
  result <- foldl1 (<|>) $ map (try . string) ["Var", "let", "fn", "do", "while", "end", "if", "else", "break", "for", "return", "declareLink", "declareInputVar"]
  notFollowedBy alphaUnderscoreNum
  return $ ReservedT result

openBracket :: ParsecT ByteString () IO Token
openBracket = do
  _ <- char '{'
  return OpenBracketT

closeBracket :: ParsecT ByteString () IO Token
closeBracket = do
  _ <- char '}'
  return CloseBracketT

op :: ParsecT ByteString () IO Token
op = do
  let oper = ["+", "-", "/", "%", "**", "*", "===", "==", "!==", "!=", "<<", ">>", "<=", "<", ">=", ">", "&&", "&", "||", "|", "^", "~", "!"]
  r <- foldl (\a b -> case length b == 1 of
                         True -> a <|> string b
                         False -> a <|> try (string b)) (fail "") oper
  case r of
    "+" -> return $ OpT AddT
    "-" -> return $ OpT MinusT
    "*" -> return $ OpT MulT
    "/" -> return $ OpT DivT
    "%" -> return $ OpT ModT
    "**" -> return $ OpT PowT
    "==" -> return $ OpT EqualT
    "!=" -> return $ OpT NotEqualT
    "<" -> return $ OpT LessThanT
    "<=" -> return $ OpT LessThanEqT
    ">" -> return $ OpT GreaterThanT
    ">=" -> return $ OpT GreaterThanEqT
    "===" -> return $ OpT StrictEqualT
    "!==" -> return $ OpT StrictNotEqualT
    "<<" -> return $ OpT ShlT
    ">>" -> return $ OpT ShrT
    "&&" -> return $ OpT LogicalAndT
    "&" -> return $ OpT BitwiseAndT
    "||" -> return $ OpT LogicalOrT
    "|" -> return $ OpT BitwiseOrT
    "^" -> return $ OpT XorT
    "~" -> return $ OpT FlipT
    "!" -> return $ OpT NotT
    _ -> fail $ "not an operator: "

equalSign :: ParsecT ByteString () IO Token
equalSign = do
  _ <- char '='
  return EqualSignT

arrow :: ParsecT ByteString () IO Token
arrow = do
  _ <- string "->"
  return ArrowT

target :: ParsecT ByteString () IO Token
target = do
  _ <- string "Target."
  i <- ident
  notFollowedBy alphaUnderscoreNum
  return $ TargetT i

sort :: ParsecT ByteString () IO Token
sort = do
  _ <- string "Sort."
  i <- ident
  notFollowedBy alphaUnderscoreNum
  return $ SortT i

buildingGroup :: ParsecT ByteString () IO Token
buildingGroup = do
  _ <- string "BuildingGroup."
  i <- ident
  notFollowedBy alphaUnderscoreNum
  return $ BuildingGroupT i


singleLineComment :: ParsecT ByteString () IO Token
singleLineComment = do
  _ <- string "//"
  _ <- many (noneOf "\n")
  return CommentT

multiLineCommentBodyEnd :: ParsecT ByteString () IO ()
multiLineCommentBodyEnd = do
   _ <- many $ do
         void (many1 (noneOf "/*"))
     <|> (try (string "/*") >> multiLineCommentBodyEnd)
     <|> try (void (char '/'))
     <|> try (void (char '*' >> noneOf "/"))
   _ <- string "*/"
   return ()

multiLineComment :: ParsecT ByteString () IO Token
multiLineComment = do
  _ <- string "/*"
  multiLineCommentBodyEnd
  return CommentT
tokenize' :: ParsecT ByteString () IO Token
tokenize' = foldl1 (<|>) [try singleLineComment, try multiLineComment, doubleLit, stringLit, comma, try nullT, try trueT, try falseT, colon, semicolon, underscore, openParen, closeParen, openBracket, closeBracket, try arrow, try op, equalSign, try target, try sort, try buildingGroup, try reserved, parseId, atConstant]

tokenizeWithLoc :: ParsecT ByteString () IO TokenLoc
tokenizeWithLoc = do
  pos <- getPosition
  tok <- tokenize'
  return $ TokenLoc tok pos
tokenize :: ParsecT ByteString () IO [TokenLoc]
tokenize = do
  spaces
  r <- sepEndBy tokenizeWithLoc spaces
  eof
  return r