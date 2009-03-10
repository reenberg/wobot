module Sql where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Query = Select [AggExpr] (Maybe Cond) Period
  deriving (Show)

data AggExpr  =  AvgAgg Expr
              |  CountAgg Expr
              |  NonAgg Expr
  deriving (Show)

data Cond  =  AndCond Cond Cond
           |  OrCond Cond Cond
           |  EqCond Expr Expr
           |  NeCond Expr Expr
           |  LtCond Expr Expr
           |  GtCond Expr Expr
           |  LeCond Expr Expr
           |  GeCond Expr Expr
  deriving (Show)

data Period  =  DefaultPeriod
             |  Period Int
  deriving (Show)

data Expr  =  TempExpr
           |  IdExpr
           |  IntExpr Integer
           |  AddExpr Expr Expr
           |  SubExpr Expr Expr
           |  MulExpr Expr Expr
           |  DivExpr Expr Expr
           |  UminusExpr Expr
  deriving (Show)

idchar  = lower <|> upper
ident   = many1 idchar

lexeme p    = do{ x <- p; spaces; return x  }
symbol name = lexeme (string name)
parens p    = between (symbol "(") (symbol ")") p

comma = lexeme (symbol ",")

query :: CharParser st Query
query =
    do  symbol "SELECT"
        agg_es  <-  sepBy1 agg_expr comma
        cond    <-  option Nothing $
                    do  symbol "WHERE"
                        cond <- lexeme cond
                        return $ Just cond
        p       <-  option DefaultPeriod period
        return $ Select agg_es cond p

period :: CharParser st Period
period  = do{ symbol "PERIOD"; i <- int; return $ Period (fromInteger i) }

cond :: CharParser st Cond
cond  =    eq_cond
      <|>  log_cond
      <|>  parens cond

log_cond :: CharParser st Cond
log_cond  = do  c1 <- lexeme cond
                op <- lexeme log_op
                c2 <- lexeme cond
                return $ op c1 c2

log_op :: CharParser st (Cond -> Cond -> Cond)
log_op  =    do{ symbol "AND";  return AndCond }
        <|>  do{ symbol "OR";   return OrCond }

eq_cond :: CharParser st Cond
eq_cond  = do  e1 <- lexeme expr
               op <- lexeme eq_op
               e2 <- lexeme expr
               return $ op e1 e2

eq_op :: CharParser st (Expr -> Expr -> Cond)
eq_op  =    do{ symbol "=";   return EqCond }
       <|>  do{ symbol "!=";  return NeCond }
       <|>  do{ symbol "<";   return LtCond }
       <|>  do{ symbol ">";   return GtCond }
       <|>  do{ symbol "<=";  return LeCond }
       <|>  do{ symbol ">=";  return GeCond }

agg_expr :: CharParser st AggExpr
agg_expr  =    do  symbol "AVG"
                   e <- parens expr
                   return $ AvgAgg e
          <|>  do  symbol "COUNT"
                   e <- parens expr
                   return $ CountAgg e
          <|>  do  e <- expr
                   return $ NonAgg e

expr :: CharParser st Expr
expr = term   `chainl1` mulop

term :: CharParser st Expr
term = factor `chainl1` addop

factor :: CharParser st Expr
factor  =    parens expr
        <|>  do{ i <- int; return $ IntExpr i }
        <|>  do{ symbol "TEMP"; return TempExpr }
        <|>  do{ symbol "ID"; return IdExpr }
        <|>  do{ symbol "-"; e <- expr; return $ UminusExpr e }

mulop :: CharParser st (Expr -> Expr -> Expr)
mulop  =    do{ symbol "*"; return MulExpr }
       <|>  do{ symbol "/"; return DivExpr }

addop :: CharParser st (Expr -> Expr -> Expr)
addop  =    do{ symbol "+"; return AddExpr }
       <|>  do{ symbol "-"; return SubExpr }

int :: CharParser st Integer
int = lexeme $ do{ ds <- many1 digit ; return $ read ds }

parseExpr :: Monad m => String -> m Expr
parseExpr s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  spaces
            e <- expr
            eof
            return e

parseq :: Monad m => String -> m Query
parseq s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  spaces
            q <- query
            eof
            return q
