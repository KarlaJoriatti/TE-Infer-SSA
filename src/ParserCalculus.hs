{-# LANGUAGE FlexibleContexts #-}
module ParserCalculus where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Control.Monad.RWS

data Expr = Free String
          | Number Integer
          | Text String
          | UnitValue
          | TrueValue
          | FalseValue
          | Let String Expr Expr
          | Lambda String Expr
          | If Expr Expr Expr
          | Application Expr Expr
          | Operation ParserCalculus.Operator Expr Expr
          | Where [(String, Expr)] Expr
        --   | Handler String [(Maybe String, Expr)] Expr


-- TODO: Change it for Application.
data Operator = Sum
              | Sub
              | Mul
              | Div
              | Lt
              | Gt
              | Eq
              | EqEq
              
instance Show Expr where
    show expr = 
       let (_, _, w) = runRWS (worker expr) () (0, 0) in w
       where
           worker (Free s) = do
               emit s
           worker (Number n) = do
               emit (show n)
           worker (Text s) = do
               emit (show s)
           worker (UnitValue) = do
               emit "()"
           worker (TrueValue) = do
               emit "True"
           worker (FalseValue) = do
               emit "False"
           worker (Let i e e') = do
               saveIndentation
               j <- getIndentation
               emit (i ++ " <- ")
               worker e
               newline
               putIndentation j
               indent
               worker e'
           worker (Lambda i e) = do
               saveIndentation
               j <- getIndentation
               emit (i ++ " = do ")
               newline
               putIndentation j
               indent
               worker e
           worker (If c e e') = do
               saveIndentation
               i <- getIndentation
               emit "if "
               worker c
               newline
               putIndentation i
               indent
               emit "then "
               worker e
               newline
               putIndentation i
               indent
               emit "else "
               worker e'
           worker (Application e e') = do
               emit "("
               worker e
               emit " "
               worker e'
               emit ")"
           worker (Operation op e e') = do
               emit "("
               worker e
               emit (" " ++ show op ++ " ")
               worker e'
               emit ")"
           worker (Where bindings e) = do
               saveIndentation
               i <- getIndentation
               emitBindings bindings
               newline
               putIndentation i
               indent
               worker e
           
           --emit :: String -> RWS () String (Int, Int) ()
           emit str = do
               (a, b) <- get 
               tell str
               put (a + length str, b)

           newline = do
               tell "\n"
               (_, b) <- get
               put (0, b)
           
           --getIndentation :: RWS () String (Int, ()) ()
           getIndentation = do
               (_, b) <- get 
               return b
            
           putIndentation b = do
               (a, _) <- get
               put (a, b)

           saveIndentation :: RWS () String (Int, Int) ()
           saveIndentation = do
               (a, _) <- get
               putIndentation a
           
           indent :: RWS () String (Int, Int) ()
           indent = do
               (a, b) <- get
               if a /= 0 then error "We Screwed up identation"
               else emit (replicate b ' ')
           
           emitBindings [(i, exp)] = do
               emit ("let " ++ i ++ " ")
               worker exp
           emitBindings ((i, exp):xs) = do
               emit ("let " ++ i ++ " ")
               worker exp
               newline
               emitBindings xs
               
    --show (Where bind e) = show e ++ " where { " ++ showBindings bind ++ "}"
    --  where showBindings [] = ""
    --        showBindings ((i, exp): xs) = i ++ " = " ++ show exp ++ "\n" ++ showBindings xs
    --show (If c e e') = "if " ++ show c ++ " then " ++ show e ++ " else " ++ show e'
    --show (Let i e e') = "let " ++ i ++ " = " ++ show e ++ " in " ++ show e' 
    --show (Operation i e e') = "(" ++ show e ++ show i ++ show e' ++ ")"
    --show (Application e e') = "(" ++ show e ++ " " ++ show e' ++ ")"
    --show (Lambda i e) = "\\" ++ i ++ " -> " ++ show e
    --show (FalseValue) = "False"
    --show (TrueValue) = "True"
    --show (Number i) = show i
    --show (UnitValue) = "()"
    --show (Text s) = show s
    --show (Free s) = s
    
    
instance Show ParserCalculus.Operator where
    show ParserCalculus.Sum = " + "
    show Sub = " - "
    show Mul = " * "
    show Div = " / "
    show Lt  = " < "
    show Gt  = " > "
    show Eq  = " = "
    show EqEq = " == "


lingDef = emptyDef
          {
            T.identStart = letter <|> char '_'
            , T.identLetter = alphaNum
            , T.reservedNames = ["let", "where", "in", "if", "then", "else", "True", "False"]
            , T.reservedOpNames = ["=", "_", "+", "->"]
          }

lexico        = T.makeTokenParser lingDef
reserved      = T.reserved lexico
reservedOp    = T.reservedOp lexico
identifier    = T.identifier lexico
natural       = T.integer lexico
stringLiteral = T.stringLiteral lexico


partida = do {e <- parseNonApp; eof; return e}

parseExpr = runParser partida [] "calculus"

varParser = do {i <- identifier; return (Free i)}

textParser = do {i <- stringLiteral; return (Text i)}

lambdaParser = do char '\\'
                  i <- identifier
                  reservedOp "->"
                  e <- highParser
                  return (Lambda i e)
                    
unitP = do reservedOp "("
           reservedOp ")"
           return (UnitValue)
           
numberParser = do i <- natural
                  return (Number i)
                  
trueParser = do reserved "True"
                return (TrueValue)

falseParser = do reserved "False"
                 return (FalseValue)

boolParser = do trueParser
             <|> falseParser
             
operator = do {reservedOp "+"; return (ParserCalculus.Sum)}
           <|> do {reservedOp "-"; return (Sub)}
           <|> do {reservedOp "/"; return (Div)}
           <|> do {reservedOp "*"; return (Mul)}
           <|> do {reservedOp "<"; return (Lt)}
           <|> do {reservedOp ">"; return (Gt)}
           <|> do {reservedOp "="; return (Eq)}
           <|> do {reservedOp "=="; return (EqEq)}

opParser = do reservedOp "("
              e <- highParser
              i <- operator
              e' <- highParser
              reservedOp ")"
              return (Operation i e e')
                  
ifParser = do reserved "if"
              c <- highParser
              reserved "then"
              e <- highParser
              reserved "else"
              e' <- highParser
              return (If c e e')

letParser = do reserved "let"
               i <- identifier
               reservedOp "="
               e <- highParser
               reserved "in"
               e' <- highParser
               return (Let i e e')

bindings = do i <- identifier
              ls <- many identifier
              reservedOp "="
              e' <- highParser
              exp <- constructLambda ls e'
              return (i, exp)

whereParser = do e <- parseNonApp
                 reserved "where"
                 reservedOp "{"
                 ass <- many bindings
                 reservedOp "}"
                 return (Where ass e)

constructLambda [] e = return e
constructLambda [x] e = return (Lambda x e)
constructLambda (x:xs) e = do e' <- constructLambda xs e
                              return (Lambda x e')

expr :: Parsec String u Expr
expr = chainl1 (between spaces spaces highParser) $ return $ Application

appParser = do reservedOp "("
               e <- expr
               reservedOp ")"
               return e

highParser = try (whereParser)
             <|> parseNonApp

parseNonApp = try (opParser)
              <|> try (appParser)
              <|> unitP
              <|> ifParser
              <|> numberParser
              <|> boolParser
              <|> lambdaParser
              <|> letParser
              <|> varParser
              <|> textParser

-------- Main -----------

parseCalculus s = case parseExpr s of
                     Left er -> print er
                     Right e -> print e

main = do putStr "Arquivo: "
          e <- getLine
          file' <- readFile e
          parseCalculus file' 