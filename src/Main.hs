module Main where
import qualified Data.Char as C
import qualified Data.Map as M
import qualified OpParse as OP
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

data Expr = Literal Integer |
  Pos Expr |
  Neg Expr |
  Plus Expr Expr |
  Minus Expr Expr |
  Times Expr Expr |
  Divide Expr Expr |
  Exp Expr Expr
  deriving (Show)

my_prefix_ops :: M.Map String (Expr -> Expr)
my_prefix_ops = M.fromList [
  ("+", Pos),
  ("-", Neg)]

my_infix_ops :: [M.Map String (OP.Assoc, (Expr -> Expr -> Expr))]
my_infix_ops = [
  M.fromList [("+", (OP.LeftAssoc, Plus)),
              ("-", (OP.LeftAssoc, Minus))],
  M.fromList [("*", (OP.LeftAssoc, Times)),
              ("/", (OP.LeftAssoc, Divide))],
  M.fromList [("^", (OP.RightAssoc, Exp))]]

parse_space :: P.Parser ()
parse_space = do
  P.many (P.char ' ')
  return ()

parse_nat :: P.Parser Integer
parse_nat = do
  raw_digits <- P.many1 P.digit
  let digits = map (\d -> toInteger (C.ord d - C.ord '0')) raw_digits
  return (foldl (\a d -> 10 * a + d) 0 digits)

parse_literal :: P.Parser Expr
parse_literal = do
  parse_space
  n <- parse_nat
  return (Literal n)

parens :: P.Parser Expr -> P.Parser Expr
parens parser = do
  parse_space
  P.char '('
  result <- parser
  parse_space
  P.char ')'
  return result

parse_input :: P.Parser Expr
parse_input = do
  result <- OP.parse_expr (OP.ParseOpts {
    OP.optSpace = parse_space,
    OP.optParens = parens,
    OP.optLiteral = parse_literal,
    OP.optPrefix = my_prefix_ops,
    OP.optInfix = my_infix_ops})
  parse_space
  P.eof
  return result

main :: IO ()
main = do
  s <- getLine
  print (P.parse parse_input "" s)
  main
