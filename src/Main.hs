module Main where

import Data.Endo

import qualified MLogic.HighLevel.Token as T
import qualified MLogic.HighLevel.Parser as P
import qualified MLogic.HighLevel.TC as TC
import qualified MLogic.HighLevel.Codegen as CG
import MLogic.HighLevel.Codegen (ExtInstr(..))
import MLogic.HighLevel.Types
import MLogic.HighLevel.TC.Types
import MLogic.HighLevel.CallGraph

import MLogic.Constants

import MLogic.Assembly.GenAsm
import MLogic.Assembly.GenAsm.Types
import MLogic.Assembly.Op
import MLogic.Assembly.GenAsm.Utils 

import qualified Data.ByteString as B

import Control.Monad.Identity

import System.IO
import System.Environment
import System.Exit
import Text.Parsec
import Control.Monad.Reader

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

genInputVarsIns :: [(String, Maybe CL)] -> GenAsm ()
genInputVarsIns ((varName, lit):xs) = do
  rhs <- case lit of
              Just (AtConstant s) -> return (var s)
              Just (DoubleLit d) -> return (double d)
              Just (StrLit s) -> return (str s)
              Just TrueL -> return trueC
              Just FalseL -> return falseC
              Just Null -> return nullC
              Nothing -> return nullC
              _ -> fail $ "internal error: genInputVarIns: nonVarTyCL: " <> show lit
  addInstr $ SetI (var varName) rhs
  genInputVarsIns xs
genInputVarsIns [] = return ()

genAsmFromExtIns :: Endo [ExtInstr] -> [(String, Endo [ExtInstr])] -> Map String String -> GenAsm ()
genAsmFromExtIns mainInstr otherFunInstr funcEntry = do
    case otherFunInstr of
      (_:_) -> 
          case M.lookup "main" funcEntry of
            Just mainEntry -> do
              jumpLabel mainEntry
              aux otherFunInstr
              addLab mainEntry
              addExtInstrs (runEndo mainInstr [])
            Nothing -> fail "entry point for main not found in funcEntry"
      [] -> addExtInstrs (runEndo mainInstr [])
  where
    addExtInstrs :: [ExtInstr] -> GenAsm ()
    addExtInstrs = mapM_ addExtInstr
    addExtInstr :: ExtInstr -> GenAsm ()
    addExtInstr (ExtInstr ins) = addInstr ins
    addExtInstr (ExtFunCallIns funName retAddr) = do
      addInstr $ OpI "add" [var retAddr, counterC, double 1]
      case M.lookup funName funcEntry of
        Just entry -> jumpLabel entry
        Nothing -> fail $ "internal error: function call entry label not in funcEntry"
    addExtInstr (ExtFunRet retAddr) = addInstr $ SetI counterC retAddr
    aux :: [(String, Endo [ExtInstr])] -> GenAsm ()
    aux ((funName, funInstrs):xs) = do
      case M.lookup funName funcEntry of
        Just entryLab -> do
          addLab entryLab
          addExtInstrs (runEndo funInstrs [])
        Nothing -> fail $ "internal error: genAsmFromExtIns: entry label for function " <> funName <> " not found"
      aux xs
    aux [] = return ()
main :: IO ()
main = do
  (inputFile:_) <- getArgs
  source <- B.readFile inputFile
  tokenizeResult <- runParserT T.tokenize () inputFile source
  case tokenizeResult of
    Left tokenizeError -> hPutStrLn stderr $ "tokenizer error: " <> show tokenizeError
    Right tokens -> do
      let parserConfig = P.ParserConfig { P.parserLogLevel = P.ParserLogNone
                                        , P.parserLogger = hPutStrLn stderr
                                        }
      parseResult <- flip runReaderT parserConfig $ runParserT P.program () inputFile (filter (\ (T.TokenLoc t _) -> t /= T.CommentT) tokens)
      case parseResult of
        Left parseError -> do
          hPutStrLn stderr $ "tokenization result:"
          hPutStrLn stderr $ show tokens
          hPutStrLn stderr $ show parseError
          exitFailure
        Right prog -> do
            tcResult <- runTC initTCState (TC.tcProgram prog)
            case tcResult of
              Left tcError -> do
                hPutStrLn stderr $ tcError
                exitFailure
              Right (tcProg, tcState) -> do
                let callGraph = generateCallGraph (tcBuiltinFunctionMap tcState) (functions tcProg)
                    recCalls = hasRecursiveCalls callGraph
                when (recCalls /= False) $ do
                  hPutStrLn stderr $ "error: recursive calls not supported yet"
                  exitFailure
                codegenResult <- CG.runCodegen (CG.codegenProgram
                                                  (S.toList (tcInputVarSet tcState))
                                                  (M.keys (tcDeclaredLinkConstantsMap tcState))
                                                  (tcBuiltinFunctionMap tcState)
                                                  (tcFunctionSigMap tcState)
                                                  tcProg)   
                case codegenResult of
                  Left codegenError -> hPutStrLn stderr codegenError
                  Right (codegenMap, codegenSt) ->
                    let entryMap = CG.codegenFuncEntryMap codegenSt
                        varCounter = CG.codegenVarCounter codegenSt
                        labelCounter = CG.codegenLabelCounter codegenSt
                        insVarSet = CG.codegenInsVarSet codegenSt
                    in
                        case M.lookup "main" codegenMap of
                          Nothing -> hPutStrLn stderr "error: no main function"
                          Just mainInstrs -> do
                            let nonMainFuncInstrs = M.delete "main" codegenMap
                                genAsmInitSt = GenAsmState { genAsmVarCounter = varCounter
                                                           , genAsmLabVar = labelCounter
                                                           , genAsmInstrs = Endo id
                                                           , genAsmVars = insVarSet }
                                finalAsmResult = runIdentity $ flip runGenAsmM genAsmInitSt $ do
                                       genInputVarsIns $ M.toList (tcInputVarMap tcState)
                                       genAsmFromExtIns mainInstrs (M.toList nonMainFuncInstrs) entryMap 
                            case finalAsmResult of
                              Left genAsmError -> hPutStrLn stderr genAsmError
                              Right (asmSt, ()) ->
                                let labeledInstrs = runEndo (genAsmInstrs asmSt) []
                                    unlabeledInstrs = filter ((/= OpLabel) . getOp)
                                                          $ map (replaceLabelWithLoc
                                                                    (getLabelLoc labeledInstrs)) labeledInstrs
                                in mapM_ (putStrLn . show) unlabeledInstrs

