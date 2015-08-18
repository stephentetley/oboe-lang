{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  OboeLang.Parser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser.
--
--------------------------------------------------------------------------------

module OboeLang.Parser 
  where

import OboeLang.SyntaxCommon
import OboeLang.TopSyntax
import OboeLang.Utils.Common (emptyH, snocH)

import Text.Parsec                              -- package: parsec
import Text.Parsec.Error 
import Text.Parsec.Language
import Text.Parsec.Pos ( initialPos )
import Text.Parsec.String
import qualified Text.Parsec.Token as P

import Control.Monad.Identity
import System.Directory
import System.FilePath ( takeBaseName ) 

type OboeParser a       = ParsecT String () Identity a
type OboeLexer          = P.GenTokenParser String () Identity

parseModule :: FilePath -> IO (Either ParseError Module)
parseModule path = 
    let name = moduleName path in safeParseFromFile (whiteSpace >> oboeModule name) path

safeParseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
safeParseFromFile p file_path = do 
    b <- doesFileExist file_path
    if b then parseFromFile p file_path 
         else return (Left missing_error)
  where
    missing_error = newErrorMessage (SysUnExpect missing_text) 
                                    (initialPos file_path)
    missing_text  = "ERROR: cannot find file - " ++ file_path



oboeModule :: Ident -> OboeParser Module
oboeModule name = 
    (\binds -> Module { module_name = name
                      , module_binds = binds })
      <$> sepEndBy topBinding semi


moduleName :: FilePath -> Ident
moduleName = Ident . takeBaseName


-- TODO - formVars exprVars...
topBinding :: OboeParser TopBinding
topBinding = explicitB <|> implicitB
  where
    explicitB   = ExplicitBind <$> ident  <*> (reservedOp "=" *> bindingRhs)
    implicitB   = ImplicitBind <$> formLoadExpr


formLoadExpr :: OboeParser Expr
formLoadExpr = loadCore <|> loadUser 
  where
    loadCore = FormLoad LOAD_CORE <$> (reserved "loadCore" *> parens stringLiteral)
    loadUser = FormLoad LOAD_USER <$> (reserved "load"     *> parens stringLiteral)


binding :: OboeParser Binding
binding = simpleBinding
       <?> "binding"

simpleBinding :: OboeParser Binding
simpleBinding = Binding <$> ident <*> (reservedOp "=" *> bindingRhs)


bindingRhs :: OboeParser BindingRhs
bindingRhs = arglist >>= bindingRhsK


bindingRhsK :: [Ident] -> OboeParser BindingRhs
bindingRhsK args = monDefn <|> pureDefn
  where
    monDefn   = BindsM args <$> doBlock
    pureDefn  = BindsP args <$> expression

arglist :: OboeParser [Ident]
arglist = pargs <|> return []
  where
    pargs = (reservedOp "\\" *> many1 ident) <* reservedOp "->"




--------------------------------------------------------------------------------
-- Do expressions

doBlock :: OboeParser DoBlock
doBlock = doMulti
  where
    doMulti  = reserved "do" *> braces (semiSep doStmt)

doBlock1 :: OboeParser DoBlock
doBlock1 = doMulti <|> doSingle
  where
    doMulti  = reserved "do" *> braces (semiSep doStmt)
    doSingle = (\a -> [a]) <$> doStmt

doStmt :: OboeParser Stmt
doStmt = pReturn <|> pDoLet <|> pDoLam <|> pDoIf <|> doAction
      <?> "do-statement"
  where
    pReturn   = Return  <$> (reserved "return" *> expression)
    pDoLet    = DoLet   <$> (reserved "let"    *> declaration)
    pDoLam    = DoLam   <$> (reserved "letfun" *> ident)
                        <*> many1 ident
                        <*> (reservedOp "=" *> doBlock)
    pDoIf     = DoIf    <$> (reserved "if" *> expression)
                        <*> (reserved "then" *> doBlock)
                        <*> (reserved "else" *> doBlock)

-- | var id is common prefix
-- 
doAction :: OboeParser Stmt
doAction = varBind >>= step
  where
    step (Bind1 vid)    =  DoBind (Bind1 vid) <$> (reservedOp "<-" *> doBlock1)
                       <|> Call (Var vid) <$> parens (commaSep expression)

    step tup            = DoBind tup <$> (reservedOp "<-" *> doBlock)



declaration :: OboeParser Decl
declaration = Decl <$> ident <*> (reservedOp "=" *> expression)

varBind :: OboeParser VarBind
varBind = varTup <|> var1
  where
    varTup = BindTuple <$> parens (commaSep ident)
    var1   = Bind1     <$> ident 


--------------------------------------------------------------------------------
-- Expressions


-- Possibly we can live with this one (not sure if general 
-- application will be supported):
--
expression :: OboeParser Expr
expression = expression1 <?> "expression"


expression1 :: OboeParser Expr
expression1 = expression2

expression2 :: OboeParser Expr
expression2 = expression3 `chainr1` (BinE <$> pLogOr)
  where
    pLogOr  = LOG_OR <$ reservedOp "||"

expression3 :: OboeParser Expr
expression3 = expression4 `chainr1` (BinE <$> pLogAdd)
  where
    pLogAdd = LOG_AND <$ reservedOp "&&"


expression4 :: OboeParser Expr
expression4 = expression5 `chainl1` (RelE <$> p)
  where
    p = choice [ EQU            <$ reservedOp "=="
               , NEQU           <$ reservedOp "/="
               , LESSTHAN       <$ reservedOp "<"
               , GREATTHAN      <$ reservedOp ">"
               , LTEQU          <$ reservedOp "<="
               , GTEQU          <$ reservedOp ">="
               ]


-- | Form-concat is considered equivalent to list concat
-- 
-- (++) : infixr 5
-- 
-- [ Lower prec than (+) : infixl 6 ]
--
expression5 :: OboeParser Expr
expression5 = expression6 `chainr1` (FormExtend <$ reservedOp "@")

-- | (+) and (-) left assoc
--
expression6 :: OboeParser Expr
expression6 = expression7 `chainl1` (BinE <$> p)
  where
    p    = pAdd <|> pSub
    pAdd = ADD <$ reservedOp "+"
    pSub = SUB <$ reservedOp "-"

-- | (*) and (/) left assoc
--
expression7 :: OboeParser Expr
expression7 =  (UnaryE <$> uNeg <*> expression)
           <|> (expression8 `chainl1` (BinE <$> pOps))
  where
    uNeg  =  NEG <$ reservedOp "-"
    pOps  = (MUL <$ reservedOp "*") <|> (DIV <$ reservedOp "/")


-- | No (^) power-of operator - use is discouraged in Csound.
--
expression8 :: OboeParser Expr
expression8 = expression9


expression9 :: OboeParser Expr
expression9 =  pCond 
           <|> expression10
  where
    pCond = Cond <$> (reserved "cond" *> expression) 
                 <*> (reservedOp "?"  *> expression)
                 <*> (reservedOp ":"  *> expression)



-- Dot operator (...) in Ocaml has higher precedence than applicatiction
--
expression10 :: OboeParser Expr
expression10 = go <$> primitiveExpression 
                  <*> optionMaybe (optionEither funBody derefBody)
  where
    funBody           = parens $ commaSep primitiveExpression
    derefBody         = reservedOp "." *> ident
    go e1 Nothing     = e1
    go e1 (Just body) = case body of { Left args -> App e1 args
                                     ; Right name -> FormDeref e1 name }


primitiveExpression :: OboeParser Expr 
primitiveExpression = 
    literalExpr <|> formExpr <|> parensExpr <|> variableExpr


formExpr :: OboeParser Expr
formExpr = PrimForm <$> (braces $ semiSep binding)

parensExpr :: OboeParser Expr
parensExpr = disamb <$> parens (commaSep expression)
  where
    -- Do we need nil () ?
    disamb [e] = e
    disamb es  = Tuple es


variableExpr :: OboeParser Expr
variableExpr = Var <$> ident



literalExpr :: OboeParser Expr
literalExpr = Lit <$> choice [ floatLit, intLit, stringLit, boolLit ]
           <?> "literal"  
  where 
    intLit      = Int    <$> int
    floatLit    = Float  <$> decimal
    stringLit   = String <$> stringl
    boolLit     = Bool   <$> bool


--------------------------------------------------------------------------------
-- Identifiers


ident                   :: OboeParser Ident
ident                   = Ident <$> identifier


--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------



oboe_lex           :: OboeLexer
oboe_lex           = P.makeTokenParser oboe_def

parens              :: OboeParser a -> OboeParser a
parens              = P.parens oboe_lex 


braces              :: OboeParser a -> OboeParser a
braces              = P.braces oboe_lex

-- brackets            :: OboeParser a -> OboeParser a
-- brackets            = P.brackets oboe_lex

-- | skips trailing space
semi                :: OboeParser String
semi                = P.semi oboe_lex

semiSep             :: OboeParser a -> OboeParser [a]
semiSep             = P.semiSep oboe_lex


-- semiSep1            :: OboeParser a -> OboeParser [a]
-- semiSep1            = P.semiSep1 oboe_lex

commaSep            :: OboeParser a -> OboeParser [a]
commaSep            = P.commaSep oboe_lex

-- commaSep1           :: OboeParser a -> OboeParser [a]
-- commaSep1           = P.commaSep1 oboe_lex


identifier          :: OboeParser String
identifier          = P.identifier oboe_lex

stringLiteral       :: OboeParser String
stringLiteral       = P.stringLiteral oboe_lex

symbol              :: String ->  OboeParser String
symbol              = P.symbol oboe_lex

reserved            :: String ->  OboeParser ()
reserved s          = P.reserved oboe_lex s
                   <?> "reserved: " ++ s

reservedOp          :: String -> OboeParser ()
reservedOp          = P.reservedOp oboe_lex

int                 :: OboeParser Int
int                 = fromIntegral <$> P.integer oboe_lex

decimal             :: OboeParser Decimal
decimal             = realToFrac <$> try (P.float oboe_lex)

stringl             :: OboeParser String
stringl             = P.stringLiteral oboe_lex

bool                :: OboeParser Bool
bool                = true <|> false
  where
    true  = True  <$ symbol "True"
    false = False <$ symbol "False"

-- pfield              :: OboeParser Int
-- pfield              = fromIntegral <$> try (string "p" *> P.integer oboe_lex)

whiteSpace          :: OboeParser ()
whiteSpace          = P.whiteSpace oboe_lex

--------------------------------------------------------------------------------
-- Lang def
--------------------------------------------------------------------------------


-- | ...
--
oboe_def :: LanguageDef st
oboe_def = emptyDef
    { P.commentLine       = "#"
    , P.identStart        = letter
    , P.identLetter       = alphaNum <|> oneOf "_"
    , P.reservedOpNames   = [ ".", "@" 
                            , "&", "<-", "=", "*"
                            , "/", "+", "-"
                            , ":", "\\", "->"
                            ]  
    , P.reservedNames     = [ "load", "loadCore"
                            , "do", "let", "letfun"
                            , "cond", "return"
                            , "if", "then", "else"
                            ]
    }



--------------------------------------------------------------------------------

oneAndMany :: OboeParser a -> OboeParser b -> OboeParser (a,[b])
oneAndMany p q = outer emptyH
  where
    outer ac = do { ans <- optionEither p q
                  ; case ans of
                      Left a -> inner ac a 
                      Right b -> outer (ac `snocH` b)
                  }
    inner ac a = final ac a <$> many q
    
    final ac a xs = (a, ac $ xs)

-- Wasn\'t this in Parsec...
optionEither :: OboeParser a -> OboeParser b -> OboeParser (Either a b)
optionEither p q = (Left <$> p) <|> (Right <$> q)
    