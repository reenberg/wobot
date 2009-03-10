\section{User Interaction}

%if False
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

-- Copyright (c) 2006-2008
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Driver
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Driver (
    MonadDriver(..),
    DriveState,
    emptyDriveState,
    DriveM,
    runDriveM,
    evalDriveM,
    userInteract,
    compileFile,
    evalFile,
    evalExp,
    evalStmt
  ) where

import Control.Exception (SomeException)
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.IORef
import Data.Maybe (mapMaybe)
import IO
import System
import System.Directory
import System.FilePath (splitExtension)
import System.IO (openTempFile)
import System.Process (runProcess,
                       waitForProcess)
import qualified System.Console.Editline.Readline as Readline

import qualified Check.F as Check.F
import qualified Check.Hs as Check.H
import Compiler
import Control.Monad.CGen as CGen
import Control.Monad.ContextException
import Control.Monad.Exception
import Control.Monad.Ref
import Control.Monad.Trace
import Control.Monad.Unique
import Data.Loc
import Data.Name
import qualified Eval.F
-- import qualified Language.F.Parser as F.P
import qualified Language.F as F
import qualified Language.Hs as H
import qualified Language.Hs.Parser as H.P
import Text.PrettyPrint.Mainland
import qualified Transform.F.Simplify as Simplify
import qualified Transform.F.ToC as ToC
import qualified Transform.Hs.Desugar as Desugar
import qualified Transform.Hs.Rename as Rename

#if defined(NESC)
import Language.NesC.Syntax
import Language.NesC.Quote
#else /* !defined(NESC) */
import Language.C.Syntax
import Language.C.Quote
#endif /* !defined(NESC) */
\end{code}
%endif

\begin{code}
class MonadIO m => MonadDriver m where
    getPrompt :: m String
    setPrompt :: String -> m ()

    isDone  :: m Bool
    setDone :: Bool -> m ()
\end{code}

\begin{code}
newtype DriveM a = DriveM { unDriveM :: StateT DriveState (ErrorT SomeException IO) a }

data DriveState = DriveState
    {  prompt      :: String
    ,  done        :: Bool
    ,  uniq        :: IORef Uniq
    ,  opts        :: Opts
    ,  contextenv  :: ContextEnv
    ,  traceenv    :: TraceEnv
    ,  tcenv       :: Check.H.TcEnv IORef DriveM
    ,  ftcenv      :: Check.F.TcEnv
    ,  simplenv    :: Simplify.SimplEnv
    ,  cgenenv     :: CGen.CGenEnv
    ,  tocenv      :: ToC.ToCEnv DriveM
    ,  evalenv     :: Eval.F.EvalEnv IORef DriveM
    }

emptyDriveState :: Opts -> IO DriveState
emptyDriveState opts = do
    u <- newRef 0
    evalenv  <- Eval.F.emptyEvalEnv
    return DriveState  {  prompt      = "> "
                       ,  done        = False
                       ,  uniq        = u
                       ,  opts        = opts
                       ,  contextenv  = emptyContextEnv
                       ,  traceenv    = emptyTraceEnv
                       ,  tcenv       = Check.H.emptyTcEnv
                       ,  ftcenv      = Check.F.emptyTcEnv
                       ,  simplenv    = Simplify.emptySimplEnv
                       ,  cgenenv     = CGen.emptyCGenEnv
                       ,  tocenv      = ToC.emptyToCEnv
                       ,  evalenv     = evalenv
                       }

runDriveM  ::  DriveM a
           ->  DriveState
           ->  IO (Either SomeException (a, DriveState))
runDriveM m s = runErrorT (runStateT (unDriveM m) s)

evalDriveM  ::  DriveM a
            ->  DriveState
            ->  IO (Either SomeException a)
evalDriveM m s = runErrorT (evalStateT (unDriveM m) s)
\end{code}

\begin{code}
instance Monad DriveM where
    m >>= f   = DriveM $ unDriveM m >>= unDriveM . f
    m1 >> m2  = DriveM $ unDriveM m1 >> unDriveM m2
    return    = DriveM . return
    fail msg  = throwException $ StrMsgException msg

instance MonadIO DriveM where
    liftIO = DriveM . liftIO

instance MonadRef IORef DriveM where
    newRef a       = DriveM $ lift $ lift $ newRef a
    readRef r      = DriveM $ lift $ lift $ readRef r
    writeRef r a   = DriveM $ lift $ lift $ writeRef r a
    modifyRef f a  = DriveM $ lift $ lift $ modifyRef f a

instance MonadError SomeException DriveM where
    throwError e            = DriveM $  throwError e
    m `catchError` handler  = DriveM $  unDriveM m `catchError`
                                        \e -> unDriveM (handler e)

instance MonadState DriveState DriveM where
    get    = DriveM $ get
    put s  = DriveM $ put s
\end{code}

\begin{code}
instance MonadUnique DriveM where
    newUnique = do
        ref <- gets uniq
        uniq <- readRef ref
        writeRef ref (uniq + 1)
        return uniq

instance MonadOpts DriveM where
    optVal opt    = DriveM $ gets (opt . opts)
    getOpts       = DriveM $ gets opts
    setOpts opts  = DriveM $ modify $ \s -> s{ opts=opts }

instance MonadContextException DriveM where
    getContextEnv      = DriveM $ gets    $ \s -> contextenv s
    putContextEnv env  = DriveM $ modify  $ \s -> s { contextenv = env }

instance MonadTrace DriveM where
    getTraceEnv      = DriveM $ gets traceenv
    putTraceEnv env  = DriveM $ modify $ \s -> s { traceenv = env }
\end{code}

\begin{code}
instance Eval.F.MonadEval IORef DriveM where
   getEvalEnv      = DriveM $ gets evalenv
   putEvalEnv env  = DriveM $ modify $ \s -> s { evalenv = env }
\end{code}

\begin{code}
instance Check.H.MonadTc IORef DriveM where
   getTcEnv      = gets tcenv
   putTcEnv env  = modify $ \s -> s { tcenv = env }
\end{code}

\begin{code}
instance Check.F.MonadTc DriveM where
    getTcEnv      = DriveM $ gets ftcenv
    putTcEnv env  = DriveM $ modify (\s -> s { ftcenv = env })
\end{code}

\begin{code}
instance Simplify.MonadSimpl DriveM where
    getSimplEnv      = DriveM $ gets simplenv
    putSimplEnv env  = DriveM $ modify (\s -> s { simplenv = env })
\end{code}

\begin{code}
instance CGen.MonadCGen DriveM where
   getCGenEnv      = gets cgenenv
   putCGenEnv env  = modify $ \s -> s { cgenenv = env }
\end{code}

\begin{code}
instance ToC.MonadToC IORef DriveM where
   getToCEnv      = gets tocenv
   putToCEnv env  = modify $ \s -> s { tocenv = env }
\end{code}

\begin{code}
instance MonadCompiler DriveM where
\end{code}

\begin{code}
instance MonadDriver DriveM where
    getPrompt         = gets prompt
    setPrompt prompt  = modify $ \s -> s{ prompt=prompt }

    isDone        = gets done
    setDone done  = modify $ \s -> s{ done=done }
\end{code}

\begin{code}
verb  :: (MonadIO m, MonadOpts m)
      =>  Doc
      ->  m ()
verb doc = do
    do_verb <- optVal (isFlagSet Opt_verbose)
    when do_verb $
        (liftIO . putStrLn . pretty 80) doc
\end{code}

\begin{code}
compileFile :: String -> DriveM ()
compileFile filename =
    do  fdecls <-
            case splitExtension filename of
              (_, ".f")  -> compileF filename
              _          -> do  prelude_fdecls  <- forPrelude [] compileHs
                                file_fdecls     <- compileHs filename
                                return $ prelude_fdecls ++ file_fdecls
        fdecls   <- optimizeF filename [F.var "main"] fdecls
        compile  <- optVal (isFlagSet Opt_compile)
        when compile $ do
            ToC.transDecls fdecls
            c      <- genC
            dumpc  <- optVal (isFlagSet Opt_d_dump_c)
            let (basename, _) = splitExtension filename
            (cfile, h)  <-  if dumpc
                            then do  let (basename, _) = splitExtension filename
                                     let filename = basename ++ ".dump.c"
                                     h <- liftIO $ openFile filename WriteMode
                                     return (filename, h)
                            else do  let (basename, _) = splitExtension filename
                                     let filename = basename ++ ".c"
                                     liftIO $ openTempFile "/tmp" filename
            liftIO $ do
                hPutStr h c
                hClose h
                ph <- runProcess  "gcc" [cfile, "-I.", "-o", basename]
                                  Nothing Nothing Nothing Nothing Nothing
                waitForProcess ph
            return ()
    `catchError`
    (\e -> liftIO $ hPutStrLn stderr $ show e)
\end{code}

\begin{code}
genC :: DriveM String
genC = do
    let gmain = F.Var $ mkName "main"
    has_main <- Check.F.isInscope gmain
    if has_main
      then do
          tau_main  <- Check.F.lookupVar gmain
          mmain     <- ToC.lookupCVar gmain
          cexp      <- mmain
          cdefs     <- CGen.getCDefs
          cstms     <- CGen.getCInitStms
          printstm  <- ToC.cprint tau_main cexp
          return $ pretty 80 $
              text "#include <stdlib.h>" </>
              text "#include <stdio.h>" </>
              text "#include \"../massaman.h\"" </>
              stack (map ppr [$cunit|
$edecls:cdefs

int main(int argc, char** argv)
{
    $stms:cstms
    $stm:printstm
}
|]) <> line
      else return ""
\end{code}

\begin{code}
evalFile :: String -> DriveM ()
evalFile filename =
    do  do_verb <- optVal (isFlagSet Opt_verbose)
        s <- catchIO $ B.readFile filename
        let pos = Pos filename 1 1 1
        (H.Body _ topdecls) <-  liftM unLoc $
                                runHsParser s pos True H.P.parseBody
        verb (stack $ map ppr topdecls)
        topdecls' <- Rename.rename topdecls >>= Desugar.desugar
        verb (stack $ map ppr topdecls')
        let typeDeclGroups  =  Check.H.analyzeTypeDeclDependencies $
                               typedecls topdecls'
        let bindingGroups   =  Check.H.analyzeBindingDependencies $
                               Check.H.bindingsFromDecls $
                               decls topdecls'
        when do_verb $ liftIO $ putStrLn $ pretty 80 $
            case typeDeclGroups of
              []  -> (empty <>)
              _   -> ((text "Type declarations:" </>
                      seplines line (map pprTypeDef typeDeclGroups)) </>)
            $
            case bindingGroups of
              []  -> empty
              _   -> text "Binding groups:" </>
                     seplines line (map pprDef bindingGroups)
        fdecls   <- Check.H.checkTopDecls topdecls'
        dump "f" Opt_d_dump_f filename "eval" fdecls
        check_f <- optVal (isFlagSet Opt_d_check_f)
        when check_f $
             Check.F.checkDecls fdecls
        Eval.F.evalDecls fdecls
    `catchError`
    (\e -> liftIO $ hPutStrLn stderr $ show e)
  where
    typedecls :: [H.Decl] -> [H.Decl]
    typedecls topDecls = mapMaybe go topDecls
      where
        go :: H.Decl -> Maybe H.Decl
        go decl@(H.TypeDecl _ _ _ _)        = Just decl
        go decl@(H.DataDecl _ _ _ _ _ _ _)  = Just decl
        go _                                = Nothing

    decls :: [H.Decl] -> [H.Decl]
    decls topdecls = mapMaybe go topdecls
      where
        go :: H.Decl -> Maybe H.Decl
        go decl@(H.VarBindDecl _ _ _ _)  = Just decl
        go decl@(H.PatBindDecl _ _ _)    = Just decl
        go decl@(H.SigDecl _ _ _)        = Just decl
        go _                             = Nothing

pprTypeDef :: Check.H.TypeDef -> Doc
pprTypeDef (Check.H.TypeDef typedecl) =
    pprDef (NonRec typedecl)

pprTypeDef (Check.H.RecTypeDef (aliasdecls,
                                datadecls,
                                classdecls)) =
    pprDef (Rec (aliasdecls ++ datadecls ++ classdecls))

pprDef :: (Ord a, Pretty a) => Rec a -> Doc
pprDef (NonRec x)  = text "Let:" </> ppr x
pprDef (Rec xs)    = text "Letrec:" </> stack (map ppr xs)

interactivePos :: Pos
interactivePos = Pos "<interactive>" 1 1 1

evalExp :: String -> DriveM ()
evalExp s =
    do  exp <-  liftM unLoc $
                runHsParser (B.pack s) interactivePos False H.P.parseExp
        verb (ppr exp)
        exp' <- Rename.fixinfix exp >>= Desugar.desugar
        verb (ppr exp')
        (ty, gexp) <- do
            (ty, mgexp)  <- Check.H.inferExpRho exp'
            [(ty', _)]   <- Check.H.generalize [ty]
            gexp         <- mgexp
            return (ty', gexp)
        Check.H.insertVar (H.Var (mkName "it")) ty
        val <- Eval.F.evalExp gexp
        Eval.F.insertVar (F.Var (mkName "it")) val
        (liftIO . putStrLn . pprint) val
    `catchError`
    (\e -> liftIO $ hPutStrLn stderr $ show e)

evalStmt :: String -> DriveM ()
evalStmt s =
    do  stmt <-  liftM unLoc $
                 runHsParser (B.pack s) interactivePos False H.P.parseStmt
        verb (ppr stmt)
        stmt' <- Rename.rename stmt >>= Desugar.desugar
        verb (ppr stmt')
        (ty, gexp) <- do
            (ty, mgexp)  <- Check.H.inferStmtRho stmt'
            [(ty', _)]   <- Check.H.generalize [ty]
            gexp         <- mgexp
            return (ty', gexp)
        Check.H.insertVar (H.Var (mkName "it")) ty
        val <- Eval.F.evalExp gexp
        Eval.F.insertVar (F.Var (mkName "it")) val
        (liftIO . putStrLn . pprint) val
    `catchError`
    (\e -> liftIO $ hPutStrLn stderr $ show e)
\end{code}

\begin{code}
kindType :: String -> DriveM ()
kindType s =
    do  ty <-  liftM unLoc $
               runHsParser (B.pack s) interactivePos False H.P.parseType
        verb (ppr ty)
        expandedTy <- Check.H.expandSynonyms ty
        kind <- Check.H.inferTypeKind expandedTy >>= Check.H.compress
        liftIO $ putStrLn $ pretty 80 $ ppr ty <+> text "::" <+> ppr kind
    `catchError`
    (\e -> liftIO $ hPutStrLn stderr $ show e)
\end{code}

\begin{code}
typeExp :: String -> DriveM ()
typeExp s =
    do  exp <-  liftM unLoc $
                runHsParser (B.pack s) interactivePos False H.P.parseExp
        verb (ppr exp)
        exp' <- Rename.fixinfix exp >>= Desugar.desugar
        verb (ppr exp')
        (_, ctx, qty) <- do
            (ty, _)     <- Check.H.inferExpRho exp'
            [(ty', _)]  <- Check.H.generalize [ty]
            return $ Check.H.destructForAll ty'
        liftIO $ putStrLn $ pretty 80 $ ppr exp <+> text "::"
            <+> case ctx of
                  []  -> ppr qty
                  _   -> ppr ctx <+> ppr qty
    `catchError`
    (\e -> liftIO $ hPutStrLn stderr $ show e)
\end{code}

\begin{code}
parseCmd :: String ->  Either (String, String) String
parseCmd s =
    case trim s of
      (':' : rs) ->
          let (cmd, args) = span isAlpha rs
          in
            Left (':' : map toLower cmd, trim args)
      s -> Right s
    where
      trim s = reverse $ dropWhile isSpace (reverse $ dropWhile isSpace s)
\end{code}

\begin{code}
readLine :: DriveM String
readLine =
    do  prompt <- getPrompt
        line <- liftIO $ Readline.readline prompt
        case line of
          Nothing                   -> do  liftIO $ putStrLn "Bye!"
                                           liftIO $ exitWith ExitSuccess
          Just cs | all isSpace cs  -> readLine
          Just s                    -> do  liftIO $ Readline.addHistory s
                                           return s
\end{code}

\begin{code}
userInteract :: DriveM ()
userInteract =
    do  line <- readLine
        case (parseCmd line) of
          Right ('!' : rest) -> do  liftIO $ System.system rest
                                    return ()
          Right s -> evalStmt s
          Left (cmd, arg) ->
              case cmd of
                ":quit" -> setDone True
                ":load" -> evalFile arg
                ":t" -> typeExp arg
                ":k" -> kindType arg
                ":cd" ->
                    liftIO $ (setCurrentDirectory arg)
                             `catch`
                             (\_ -> putStrLn $  "Could not change to directory: "
                                                ++ arg)
                ":pwd" ->
                    liftIO $ do  dir <- getCurrentDirectory
                                 putStrLn dir
                             `catch`
                             (\_ -> putStrLn "Could not get current directory.")
                _ -> liftIO $ putStrLn ("Unknown command: " ++ cmd)
        done <- isDone
        if done
            then liftIO $ putStrLn "Bye!"
            else userInteract
\end{code}

\begin{code}
runHsParser  ::  B.ByteString
             ->  Pos
             ->  Bool
             ->  H.P.P a
             ->  DriveM a
runHsParser s pos isModule m = do
    opts  <- getOpts
    case H.P.parse s pos isModule opts H.P.ParseDirect m of
      Left e   ->  throwError e
      Right a  ->  return a
\end{code}

\begin{spec}
runGParser  ::  B.ByteString
            ->  Pos
            ->  F.P.P a
            ->  DriveM a
runGParser s pos m = do
    opts  <- getOpts
    case F.P.parse s pos opts m of
      Left e   ->  throwError e
      Right a  ->  return a
\end{spec}
