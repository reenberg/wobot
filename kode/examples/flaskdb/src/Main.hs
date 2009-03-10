{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}

import Prelude hiding (exp)

import Control.Monad
import Data.IORef
import System.Environment (getArgs)

import Compiler
import Control.Monad.CGen
import Control.Monad.NesCGen
import Data.Loc
import Data.Name
import qualified Language.F as F
import qualified Language.Hs.Syntax
import qualified Language.Hs.Syntax as H
import Language.Hs.Quote
import Language.NesC.Syntax as C
import qualified Language.NesC.Syntax
import Language.NesC.Quote
import qualified Transform.F.ToC as ToC
import Text.PrettyPrint.Mainland

import Flask
import Sql

main :: IO ()
main = do
    args           <- getArgs
    (opts, files)  <- parseOpts defaultOpts args
    q              <- parseQuery (head files)
    writeIORef Language.Hs.Quote.quasiOpts defaultOpts
    env    <- emptyFlaskState opts
    result <- (flip evalFlaskM) env $ do
              fdecls <- forPrelude [] compileHs
              modifyFlaskEnv $ \s -> s { f_fdecls = fdecls }
              compileQuery q
    case result of
      Left err  -> fail $ show err
      Right a   -> return a
  where
    defaultOpts :: Opts
    defaultOpts = Opts  {  output    = Just "Flask"
                        ,  prelude   = Nothing
                        ,  flags     = [Opt_OptC, Opt_ImplicitPrelude]
                        }

parseQuery :: FilePath -> IO Query
parseQuery filepath = do
    s <- readFile filepath
    parseq s

update' :: forall a . Reify a
        => N a
        -> (S (a, a) -> S a)
        -> S ((Bool, a), a)
        -> S (Maybe a, a)
update' n f s = S $ do
    e <- nexp n
    let (x, sigma) = sunzip s
    let (flag, a) = sunzip x
    let updated :: S a = f (a `szip` sigma)
    let s' :: S (Maybe a, a) = flag `szip` updated >>> smap [$exp| \(flag, sigma) -> if flag then (Just sigma, $exp:e) else (Nothing, sigma) |]
    unS s'

compileQuery :: Query -> FlaskM ()
compileQuery (Select es _ period) =
    case compileAggExps es clk of
      (QueryS  (zero :: N a) (s :: S a) (update :: S (a, a) -> S a) (_ :: S a -> S (Float, c))) ->
          genStream $ S $ do
          let tau_a = reify (undefined :: a)
          tauf_a <- toF tau_a

          addCExpImport "eval_local"  [$ty|$ty:tau_a -> ()|]
                                      (ToC.CSignal (tauf_a F.--> unitGTy) (C.Id "eval_local"))
          (params, _) <- ToC.flattenParams tauf_a
          usesProvides True [$ncusesprovides|uses event void eval_local($params:params);|]

          addCExpImport "eval"  [$ty|$ty:tau_a -> ()|]
                                (ToC.CSignal (tauf_a F.--> unitGTy) (C.Id "eval"))
          (params, _) <- ToC.flattenParams tauf_a
          usesProvides True [$ncusesprovides|uses event void eval($params:params);|]

          addCImport "node_id" [$ty|Float|] [$cexp|TOS_LOCAL_ADDRESS|]

          let eval_local :: S a -> S ()  =  smap [$exp|eval_local|]
          let eval       :: S a -> S ()  =  smap [$exp|eval|]
          let r          :: S a          =  recv 1 Passive
          let sin        :: S (Bool, a)  =  smerge  (r >>> smap [$exp|\x -> (False, x)|])
                                                    (s >>> strace eval_local >>> smap [$exp|\x -> (True, x)|])
          unS $ sloop 1 zero (update' zero update) sin >>> sjust >>> strace eval >>> send 1 Passive
  where
    clk = compilePeriod period

data QueryS = forall a c . (Reify a, Reify c) =>
              QueryS { q_zero   :: N a,
                       q_stream :: S a,
                       q_update :: S (a, a) -> S a,
                       q_reduce :: S a -> S (Float, c)
                     }

compileAggExps :: [AggExpr] -> S () -> QueryS
compileAggExps  []        _   = error "empty select statement"
compileAggExps  [e]       clk = case compileAggExp e clk of
                                  AggS zero s update (reduce :: S a -> S Float)  ->
                                      QueryS zero s update ((\s -> s >>> reduce >>> smap [$exp| \x -> (x, ()) |])
                                                            :: S a -> S (Float, ()))
compileAggExps  (e : es)  clk = qszip (compileAggExp e clk) (compileAggExps es clk)
  where
    qszip :: AggS -> QueryS -> QueryS
    qszip  (AggS    (zero1 :: N a) (s1 :: S a) (update1 :: S (a, a) -> S a) (reduce1 :: S a -> S Float))
           (QueryS  (zero2 :: N b) (s2 :: S b) (update2 :: S (b, b) -> S b) (reduce2 :: S b -> S (Float, c))) =
        QueryS zero s update reduce
      where
        zero :: N (a, b)
        zero = nzip zero1 zero2

        s :: S (a, b)
        s = szip s1 s2

        update :: S ((a, b), (a, b)) -> S (a, b)
        update s = szip  (update1 (szip s1a s1b))
                         (update2 (szip s2a s2b))
          where
            (s1, s2)   = sunzip s
            (s1a, s2a) = sunzip s1
            (s1b, s2b) = sunzip s2

        reduce :: S (a, b) -> S (Float, (Float, c))
        reduce s = szip (reduce1 s1 >>> smap [$exp|fst|]) (reduce2 s2)
          where
            (s1, s2) = sunzip s

data AggS = forall a b . (Reify a) =>
              AggS { agg_zero   :: N a,
                     agg_stream :: S a,
                     agg_update :: S (a, a) -> S a,
                     agg_reduce :: S a -> S Float
                   }

compileAggExp :: AggExpr -> S () -> AggS
compileAggExp (AvgAgg e) clk  = AggS zero s update reduce
  where
    zero :: N (Float, Float)
    zero = liftN $ [$exp|(0.0, 0.0)|]

    s :: S (Float, Float)
    s = compileExpr e clk >>> smap [$exp| \x -> (1.0, x)|]

    update :: S ((Float, Float), (Float, Float)) -> S (Float, Float)
    update = smap [$exp| \((count1, sum1), (count2, sum2)) -> (count1 + count2, sum1 + sum2)|]

    reduce :: S (Float, Float) -> S Float
    reduce = smap [$exp| \(count, sum) -> sum/count |]

compileAggExp (CountAgg e) clk  = AggS zero s update reduce
  where
    zero :: N Float
    zero = liftN $ [$exp|0.0|]

    s :: S Float
    s = compileExpr e clk >>> smap [$exp| \_ -> 1.0|]

    update :: S (Float, Float) -> S Float
    update = smap [$exp| \(count1, count2) -> count1 + count2|]

    reduce :: S Float -> S Float
    reduce = id

compileAggExp (NonAgg e) clk  = AggS zero s update reduce
  where
    zero :: N Float
    zero = liftN $ [$exp|0.0|]

    s :: S Float
    s = compileExpr e clk

    update :: S (Float, Float) -> S Float
    update = smap [$exp|fst|]

    reduce :: S Float -> S Float
    reduce = id

{-
compileCond :: Cond -> S () -> S Bool
compileCond (AndCond c1 c2)  clk  = logCond c1 c2 [$exp| \(x, y) -> x && y|] clk
compileCond (OrCond c1 c2)   clk  = logCond c1 c2 [$exp| \(x, y) -> x || y|] clk
compileCond (EqCond e1 e2)   clk  = eqCond e1 e2 [$exp| \(x, y) -> x == y|] clk
compileCond (NeCond e1 e2)   clk  = eqCond e1 e2 [$exp| \(x, y) -> x /= y|] clk
compileCond (LtCond e1 e2)   clk  = eqCond e1 e2 [$exp| \(x, y) -> x < y|] clk
compileCond (GtCond e1 e2)   clk  = eqCond e1 e2 [$exp| \(x, y) -> x > y|] clk
compileCond (LeCond e1 e2)   clk  = eqCond e1 e2 [$exp| \(x, y) -> x <= y|] clk
compileCond (GeCond e1 e2)   clk  = eqCond e1 e2 [$exp| \(x, y) -> x >= y|] clk

logCond :: Cond -> Cond -> Hs.Exp -> S () -> S Bool
logCond c1 c2 f clk = szip s1 s2 >>> smap f
  where
    s1 = compileCond c1 clk
    s2 = compileCond c2 clk

eqCond :: Expr -> Expr -> Hs.Exp -> S () -> S Bool
eqCond e1 e2 f clk = szip s1 s2 >>> smap f
  where
    s1 = compileExpr e1 clk
    s2 = compileExpr e2 clk
-}

compilePeriod :: Period -> S ()
compilePeriod DefaultPeriod  = clock 1000
compilePeriod (Period p)     = clock p

compileExpr :: Expr -> S () -> S Float
compileExpr TempExpr     clk = clk >>> adc "Temperature"
compileExpr IdExpr       clk = clk >>> sconst [$exp|node_id|]

compileExpr (IntExpr i)  clk = clk >>> sconst [$exp|$flo:f|]
  where
    f = fromInteger i

compileExpr (AddExpr e1 e2)  clk = binExpr e1 e2 [$exp| \(x, y) -> x + y|] clk
compileExpr (SubExpr e1 e2)  clk = binExpr e1 e2 [$exp| \(x, y) -> x - y|] clk
compileExpr (MulExpr e1 e2)  clk = binExpr e1 e2 [$exp| \(x, y) -> x * y|] clk
compileExpr (DivExpr e1 e2)  clk = binExpr e1 e2 [$exp| \(x, y) -> x / y|] clk

compileExpr (UminusExpr e)   clk = compileExpr e clk >>> smap [$exp| \x -> -x|]

binExpr :: Expr -> Expr -> H.Exp -> S () -> S Float
binExpr e1 e2 f clk = szip s1 s2 >>> smap f
  where
    s1 = compileExpr e1 clk
    s2 = compileExpr e2 clk
