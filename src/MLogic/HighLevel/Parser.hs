module MLogic.HighLevel.Parser where

import qualified MLogic.HighLevel.Token as T
import MLogic.HighLevel.Token (Token(..), TokenLoc(..), OpToken(..), tokenize)
import MLogic.HighLevel.Types

import qualified Data.ByteString as B
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.List hiding (sort)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Expr

import Control.Monad.Reader

import Prelude hiding (log)
data ParserLogLevel = ParserLogNone | ParserLog

data ParserConfig = ParserConfig { parserLogLevel :: ParserLogLevel
                                 , parserLogger :: String -> IO ()
                                 }

log :: String -> ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
log s = do
  logLevel <- asks parserLogLevel
  logger <- asks parserLogger
  case logLevel of
    ParserLogNone -> return ()
    ParserLog -> liftIO $ logger s

parseToken :: (Token -> Maybe a) -> ParsecT [TokenLoc] () (ReaderT ParserConfig IO) a
parseToken f = tokenPrim (\ (TokenLoc t _) -> show t) nextpos (\ (TokenLoc t _) -> f t)
  where
    nextpos :: SourcePos -> TokenLoc -> [TokenLoc] -> SourcePos
    nextpos _ _ (TokenLoc _ next:_) = next
    nextpos pos _ _ = pos

getBinOp :: OpToken -> ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Expr SourcePos -> Expr SourcePos -> Expr SourcePos)
getBinOp AddT = do
  pos <- getPosition
  parseToken (\t -> case t of
                      OpT AddT -> Just (Add pos Nothing)
                      _ -> Nothing)
getBinOp MinusT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT MinusT -> Just (Sub pos Nothing)
                        _ -> Nothing)
getBinOp MulT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT MulT -> Just (Mul pos Nothing)
                        _ -> Nothing)
getBinOp DivT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT DivT -> Just (Div pos Nothing)
                        _ -> Nothing)
getBinOp ModT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT ModT -> Just (Mod pos Nothing)
                        _ -> Nothing)
getBinOp PowT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT PowT -> Just (Pow pos Nothing)
                        _ -> Nothing)
getBinOp EqualT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT EqualT -> Just (Equal pos Nothing)
                        _ -> Nothing)
getBinOp NotEqualT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT NotEqualT -> Just (NotEqual pos Nothing)
                        _ -> Nothing)
getBinOp LessThanT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT LessThanT -> Just (LessThan pos Nothing)
                        _ -> Nothing)
getBinOp LessThanEqT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT LessThanEqT -> Just (LessThanEq pos Nothing)
                        _ -> Nothing)
getBinOp GreaterThanT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT GreaterThanT -> Just (GreaterThan pos Nothing)
                        _ -> Nothing)
getBinOp GreaterThanEqT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT GreaterThanEqT -> Just (GreaterThanEq pos Nothing)
                        _ -> Nothing)
getBinOp StrictEqualT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT StrictEqualT -> Just (StrictEqual pos Nothing)
                        _ -> Nothing)
getBinOp StrictNotEqualT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT StrictNotEqualT -> Just (StrictNEq pos Nothing)
                        _ -> Nothing)
getBinOp ShlT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT ShlT -> Just (Shl pos Nothing)
                        _ -> Nothing)
getBinOp ShrT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT ShrT -> Just (Shr pos Nothing)
                        _ -> Nothing)
getBinOp LogicalAndT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT LogicalAndT -> Just (LAnd pos Nothing)
                        _ -> Nothing)
getBinOp BitwiseAndT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT BitwiseAndT -> Just (BAnd pos Nothing)
                        _ -> Nothing)
getBinOp LogicalOrT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT LogicalOrT -> Just (LOr pos Nothing)
                        _ -> Nothing)
getBinOp BitwiseOrT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT BitwiseOrT -> Just (BOr pos Nothing)
                        _ -> Nothing)
getBinOp XorT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT XorT -> Just (Xor pos Nothing)
                        _ -> Nothing)
getBinOp op = error $ "getBinOp: " <> show op <> " is not a binary operator"

getUnaryOp :: OpToken -> ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Expr SourcePos -> Expr SourcePos)
getUnaryOp MinusT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT MinusT -> Just (Negate pos Nothing)
                        _ -> Nothing)
getUnaryOp FlipT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT FlipT -> Just (Flip pos Nothing)
                        _ -> Nothing)
getUnaryOp NotT = do
  pos <- getPosition
  parseToken (\t -> case t of
                        OpT NotT -> Just (Not pos Nothing)
                        _ -> Nothing)
getUnaryOp op = error $ "getUnaryOp: " <> show op <> " is not a unary operator"

ident :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) String
ident = parseToken $ \t ->
          case t of
            IdT str -> Just str
            _ -> Nothing


comma :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
comma = parseToken $ \t ->
          case t of
            CommaT -> Just ()
            _ -> Nothing

colon :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
colon = parseToken $ \t ->
              case t of
                ColonT -> Just ()
                _ -> Nothing

semicolon :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
semicolon = parseToken $ \t ->
              case t of
                SemicolonT -> Just ()
                _ -> Nothing

underscore :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
underscore = parseToken $ \t ->
               case t of
                 UnderscoreT -> Just ()
                 _ -> Nothing

openParen :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
openParen = parseToken $ \t ->
              case t of
                OpenParenT -> Just ()
                _ -> Nothing

closeParen :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
closeParen = parseToken $ \t ->
              case t of
                CloseParenT -> Just ()
                _ -> Nothing

withParen :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) a -> ParsecT [TokenLoc] () (ReaderT ParserConfig IO) a
withParen p = do
  openParen
  r <- p
  closeParen
  return r

target :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) String
target = parseToken $ \t ->
           case t of
             TargetT s -> Just s
             _ -> Nothing

sort :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) String
sort = parseToken $ \t ->
           case t of
             SortT s -> Just s
             _ -> Nothing

buildingGroup :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) String
buildingGroup = parseToken $ \t ->
           case t of
             BuildingGroupT s -> Just s
             _ -> Nothing


openBracket :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
openBracket = parseToken $ \t ->
              case t of
                OpenBracketT -> Just ()
                _ -> Nothing

closeBracket :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
closeBracket = parseToken $ \t ->
              case t of
                CloseBracketT -> Just ()
                _ -> Nothing

withBracket :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) a -> ParsecT [TokenLoc] () (ReaderT ParserConfig IO) a
withBracket p = do
  openBracket
  r <- p
  closeBracket
  return r

arrow :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
arrow = parseToken $ \t ->
          case t of
            ArrowT -> Just ()
            _ -> Nothing

typeVar :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
typeVar = parseToken $ \t ->
            case t of
              ReservedT "Var" -> Just ()
              _ -> Nothing

equalSign :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
equalSign = parseToken $ \t ->
              case t of
                EqualSignT -> Just ()
                _ -> Nothing

reserved :: String -> ParsecT [TokenLoc] () (ReaderT ParserConfig IO) ()
reserved str = parseToken $ \t ->
                 case t == ReservedT str of
                   True -> Just ()
                   False -> Nothing

simpleLit :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) CL
simpleLit = parseToken $ \t ->
               case t of
                 AtConstantT str -> Just $ AtConstant str
                 DoubleLitT d -> Just $ DoubleLit d
                 StringLitT s -> Just $ StrLit s
                 NullT -> Just Null
                 TrueT -> Just TrueL
                 FalseT -> Just FalseL
                 _ -> Nothing

literal :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) CL
literal = simpleLit <|> do
            lits <- withParen $ sepBy1 literal comma
            case lits of
              (lit:[]) -> return lit
              _ -> return $ TupleLit lits


identWithType :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (String, SourcePos, Types)
identWithType = do
  pos <- getPosition
  i <- ident
  colon
  t <- type'
  return (i, pos, t)

paramlist :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) [(String, SourcePos, Types)]
paramlist = withParen $ sepBy identWithType comma

typelist :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) [Types]
typelist = withParen $ sepBy type' comma

type' :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) Types
type' = (typeVar >> return VarTy) <|> (do
                                        tys <- typelist
                                        case tys of
                                          [ty] -> return ty
                                          _ -> return $ TupleTy tys)

funcRet :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) Types
funcRet = arrow >> type'

function :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Function SourcePos)
function = do
  pos <- getPosition
  reserved "fn"
  fname <- ident
  params <- paramlist
  retType <- optionMaybe funcRet
  body <- block
  return Function { functionName = fname
                  , functionParams = params
                  , functionRetType = retType
                  , functionBody = body
                  , functionLoc = pos
                  }


block :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Block SourcePos)
block = do
  st <- withBracket (many statement)
  return $ Block st Nothing

statement :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
statement =
  let stmt1 = do
         stmt <- (declareStmt
                  <|> try assignStmt
                  <|> breakStmt
                  <|> returnStmt
                  <|> endStmt)
         semicolon
         return stmt
      stmt2 = blockStmt <|> try ifThenElseStmt <|> ifThenStmt <|> whileStmt <|> forStmt
      stmt3 = do
        stmt <- exprStmt
        semicolon
        return stmt
  in stmt1 <|> stmt2 <|> stmt3

blockStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
blockStmt = do
  pos <- getPosition
  b <- block
  return $ BlockStmt pos Nothing b

exprStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
exprStmt = do
  pos <- getPosition
  e <- expr
  return $ ExprStmt pos Nothing e

rhsStatement :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
rhsStatement = do
  log "trying rhsStatement"
  blockStmt <|> exprStmt

statementWithoutSemicolon :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
statementWithoutSemicolon = do
  log "trying statementWithoutSemicolon"
  declareStmt
   <|> try assignStmt
   <|> breakStmt
   <|> returnStmt
   <|> endStmt
   <|> try ifThenElseStmt
   <|> ifThenStmt
   <|> whileStmt
   <|> forStmt
   <|> rhsStatement

declareStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
declareStmt = do
  log "trying declareStmt"
  pos <- getPosition
  reserved "let"
  p <- pat
  ty <- optionMaybe $ do
    colon
    type'
  rhs <- optionMaybe $ do
           equalSign
           rhsStatement
  return $ DeclareStmt pos Nothing p ty rhs

assignStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
assignStmt = do
  pos <- getPosition
  p <- pat
  equalSign
  rhs <- rhsStatement
  return $ AssignStmt pos Nothing p rhs

breakStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
breakStmt = do
  pos <- getPosition
  reserved "break"
  return $ BreakStmt pos Nothing

returnStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
returnStmt = do
  pos <- getPosition
  reserved "return"
  rhs <- optionMaybe rhsStatement
  return $ ReturnStmt pos Nothing rhs

endStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
endStmt = do
  pos <- getPosition
  reserved "end"
  return $ EndStmt pos Nothing
ifThenStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
ifThenStmt = do
  pos <- getPosition
  reserved "if"
  cond <- withParen expr
  cons <- block
  return $ IfThenStmt pos Nothing cond cons


ifThenElseStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
ifThenElseStmt = do
  pos <- getPosition
  reserved "if"
  cond <- withParen expr
  cons <- block
  reserved "else"
  alt <- block
  return $ IfThenElseStmt pos Nothing cond cons alt

whileStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
whileStmt = do
  pos <- getPosition
  reserved "while"
  cond <- withParen expr
  body <- block
  return $ WhileStmt pos Nothing cond body

forStmt :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Statement SourcePos)
forStmt = do
  pos <- getPosition
  reserved "for"
  (initial, cond, step) <- withParen $ do
    initial <- statementWithoutSemicolon
    semicolon
    cond <- expr
    semicolon
    step <- statementWithoutSemicolon
    return (initial, cond, step)
  body <- block
  return $ ForStmt pos Nothing initial cond step body

idOrUnderscorePat :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Pat SourcePos)
idOrUnderscorePat =
  let idPat = do
        pos <- getPosition
        i <- ident
        return $ VarPat pos i
      underscorePat = do
        pos <- getPosition
        underscore
        return $ IgnorePat pos
  in idPat <|> underscorePat
patlist :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Pat SourcePos)
patlist = TuplePat <$> getPosition <*> withParen (sepBy1 pat comma)

pat :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Pat SourcePos)
pat = idOrUnderscorePat <|> patlist

linkDeclaration :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (SourcePos, String)
linkDeclaration = do
  pos <- getPosition
  reserved "declareLink"
  i <- withParen ident
  semicolon
  return (pos, i)

inputVarDeclaration :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (SourcePos, String, Maybe CL)
inputVarDeclaration = do
  pos <- getPosition
  reserved "declareInputVar"
  (i, rhs) <- withParen $ do
    i <- ident
    rhs <- optionMaybe $ do
      comma
      literal
    return (i, rhs)
  semicolon
  return (pos, i, rhs)
operatorTable :: OperatorTable [TokenLoc] () (ReaderT ParserConfig IO) (Expr SourcePos)
operatorTable = [ [Prefix $ getUnaryOp MinusT, Prefix $ getUnaryOp FlipT, Prefix $ getUnaryOp NotT]
                , [ Infix (getBinOp PowT) AssocLeft]
                , [ Infix (getBinOp MulT) AssocLeft
                  , Infix (getBinOp DivT) AssocLeft
                  , Infix (getBinOp ModT) AssocLeft
                  , Infix (getBinOp BitwiseAndT) AssocLeft ]
                , [ Infix (getBinOp AddT) AssocLeft
                  , Infix (getBinOp MinusT) AssocLeft
                  , Infix (getBinOp XorT) AssocLeft ]
                , [ Infix (getBinOp ShlT) AssocLeft
                  , Infix (getBinOp ShrT) AssocLeft
                  , Infix (getBinOp BitwiseOrT) AssocLeft ]
                , [ Infix (getBinOp EqualT) AssocLeft
                  , Infix (getBinOp NotEqualT) AssocLeft
                  , Infix (getBinOp LessThanT) AssocLeft
                  , Infix (getBinOp LessThanEqT) AssocLeft
                  , Infix (getBinOp GreaterThanT) AssocLeft
                  , Infix (getBinOp GreaterThanEqT) AssocLeft
                  , Infix (getBinOp StrictEqualT) AssocLeft
                  , Infix (getBinOp StrictNotEqualT) AssocLeft ]
                , [ Infix (getBinOp LogicalAndT) AssocLeft
                  , Infix (getBinOp LogicalOrT) AssocLeft ]
                ]

expr :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Expr SourcePos)
expr = buildExpressionParser operatorTable (try term)

funCallArg :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (FunCallArg SourcePos)
funCallArg = do
  pos <- getPosition
  (ExprArg pos <$> expr)
    <|> (TargetArg pos <$> target)
    <|> (SortArg pos <$> sort)
    <|> (BuildingGroupArg pos <$> buildingGroup)
funCallExpr :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Expr SourcePos)
funCallExpr = do
  pos <- getPosition
  name <- ident
  args <- withParen (sepBy funCallArg comma)
  return $ FunCallExpr pos Nothing name args
tupleExpr :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Expr SourcePos)
tupleExpr = do
  pos <- getPosition
  e <- withParen $ do
         sepBy1 expr comma
  case e of
    (x:[]) -> return x
    _ -> return $ Tuple pos Nothing e

term :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Expr SourcePos)
term = do
  pos <- getPosition
  tupleExpr
    <|> try funCallExpr
    <|> (Var pos Nothing <$> ident)
    <|> (Lit pos Nothing <$> literal)

program :: ParsecT [TokenLoc] () (ReaderT ParserConfig IO) (Program SourcePos)
program = do
  linkDecls <- many linkDeclaration
  inputVars <- many inputVarDeclaration
  funcs <- many function
  eof
  return Program { functions = funcs
                 , inputVars = inputVars
                 , declaredLinkConstants = linkDecls
                 }