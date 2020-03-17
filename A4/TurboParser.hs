module TurboParser where

import Control.Applicative

import ParserLib
import TurboDef

-- This can help testing by reading from file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Stmt)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

-- parse for natural numbers, convert to double
parseNatural :: Parser Double
parseNatural = do
    input <- natural               -- get the natural number
    let frac = realToFrac (input)  -- convert to double
    return frac

-- parse for double
parseFrac :: Parser Double
parseFrac = do
    num1 <- natural
    let str1 = show num1
    string "."
    num2 <- natural
    let str2 = show num2             
    let result = str1 ++ "." ++ str2  -- combine the number as string again
    let frac = read result :: Double  -- convert to double
    return frac

-- parse for literals
parseLiteral :: Parser RealExpr
parseLiteral = RLit <$> ( parseFrac <|> parseNatural)

-- parse for reserved words
reserved = identifier ["pendown", "penup", "turn", "forward", "for", "to"]

-- parse for variables
parseVar :: Parser RealExpr
parseVar = RVar <$> reserved

-- parse for expr in brackets
parseBrackets :: Parser RealExpr
parseBrackets = terminal "(" *> parseExpr <* whitespaces <* terminal ")"

-- parse for negative expr
parseNeg :: Parser RealExpr
parseNeg = Neg <$> (terminal "-" *> base)

-- parse for multiply/divide operator
multDivOpers = do 
    { operator "*"; return (:*)
    }
    <|> do
        { operator "/"; return (:/)
        }

-- parse for add/subtract operator
addSubOpers = do
    { operator "+"; return (:+)
    }
    <|> do
        { operator "-"; return (:-)
        }

-- base case of parsing for expr op expr is
-- negative expr, literals, variables, expr in brackets
base = parseNeg <|> parseLiteral <|> parseVar <|> parseBrackets

-- precedence is anything in the base case, and then multiply/divide
multDiv = base `chainl1` multDivOpers

-- parse for expr op expr
-- precedence is anything in base case, then multiply/divide, then add/subtract
parseOper :: Parser RealExpr
parseOper = multDiv `chainl1` addSubOpers

-- parse for expr
-- expr is any of the following:
-- expr op expr, -expr, literal, variable, (expr)
parseExpr :: Parser RealExpr
parseExpr = parseOper <|> base

-- parse for an assigning stmt
parseAssign :: Parser Stmt
parseAssign = do
    var <- reserved       -- variable
    terminal "="          -- =
    expr <- parseExpr     -- expr
    return (var := expr)

-- parse for penup
parseUp :: Parser Stmt
parseUp = do
    keyword "penup"
    return PenUp

-- parse for pendown
parseDown :: Parser Stmt
parseDown = do
    keyword "pendown"
    return PenDown

-- parse for pen-cmd
-- pen-cmd has to be either penup or pendown
parsePen :: Parser Stmt
parsePen = parseUp <|> parseDown

-- parse for turn
parseTurn :: Parser Stmt
parseTurn = Turn <$> (keyword "turn" *> parseExpr)

-- parse for forward
parseForward :: Parser Stmt
parseForward = Forward <$> (keyword "forward" *> parseExpr)

-- parse for for-loop
parseFor :: Parser Stmt
parseFor = do
    keyword "for"                     -- for
    str <- reserved                   -- variable
    terminal "="                      -- =
    expr1 <- parseExpr                -- lower bound
    keyword "to"                      -- to
    expr2 <- parseExpr                -- upper bound
    seq <- parseCurly                 -- {stmts}
    return (For str expr1 expr2 seq)

-- one stmt inside the for-loop
forOne = do
    stmt <- parseStmt  -- the stmt
    terminal ";"       -- ;
    return stmt

-- parse for many stmts in for-loop
parseStmts :: Parser [Stmt]
parseStmts = many forOne

-- parse for {stmts}
parseCurly :: Parser [Stmt]
parseCurly = terminal "{" *> parseStmts <* terminal "}"

-- parse for {stmts}, convert to seq
parseSeq :: Parser Stmt
parseSeq = Seq <$> parseCurly

-- parse for stmt
-- a stmt is one of the following:
-- assign, pen-cmd, turn, forward, for-loop, seq
parseStmt :: Parser Stmt
parseStmt = parseAssign <|> parsePen <|> parseTurn <|> 
    parseForward <|> parseFor <|> parseSeq

-- skips whitespaces in front of stmt, check for eof after stmt
mainParser :: Parser Stmt
mainParser = whitespaces *> parseStmt <* eof
