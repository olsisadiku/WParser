module WParser ( parse,
                 wprogram ) where

    import Data.Char
    import W

    import Control.Applicative (Applicative(..))
    import Control.Monad (liftM, ap)

    -----------------------------
    -- This is the main parser --
    -----------------------------
    wprogram :: Parser WStmt
    wprogram = whitespace >> many stmt >>= \ss -> return (Block ss)
    -- a program is a sequence of statements; the parser returns them
    -- as a single block-statement

    -- only two of the statement types above are supported, the rest are undefined.
    -- please implement them
    stmt = varDeclStmt +++ assignStmt +++ ifStmt +++ whileStmt +++ 
           blockStmt +++ emptyStmt +++ printStmt
           

    emptyStmt = 
      symbol ";" >>
      return Empty
  
    printStmt = 
      keyword "print" >>
      expr >>= \e ->
      symbol ";" >>
      return (Print e)

    ----- NEEEEDDDD TOO DOOOOOO
    varDeclStmt = 
      keyword "var" >>= \a ->
      whitespace >>= \_ ->
      many alphanum >>= \left ->
      whitespace >>= \_ ->
      symbol "=" >>= \f -> expr >>= \e ->
        symbol ";" >>
         return (VarDecl (left) (e))
      
    assignStmt = 
      many alphanum >>= \var -> 
        whitespace >>
        symbol "=" >>
        expr >>= \d ->
          whitespace >>
          symbol ";" >>
          return (Assign var d) 
    
    
    
    ifStmt = 
      keyword "if" >>= \a ->
        whitespace >>
        expr >>= \d ->
          whitespace >>
          stmt >>= \f ->
          keyword "else" >>
          stmt >>= \h ->
          return (If d f h)
          
    whileStmt = 
      keyword "while" >> 
        whitespace >>
        expr >>= \e ->
          whitespace >>
          stmt >>= \f ->
            whitespace >> 
            return (While e f)


    blockStmt = 
        symbol "{" >> many stmt >>= \d-> 
          whitespace >>
          symbol "}" >> 
        return (Block(d))      

    ------ NEEEED TOOOO DOOOOO


    expr = (andil >>= andSeq) +++ (stringLiteral +++ boolLiteral +++ intLiteral)
    -- stringLiterals can contain \n characters
    stringLiteral = char ('"') >>
                    many stringChar >>= \s ->
                    char ('"') >>
                    whitespace >>
                    return (Val (VString s))

    stringChar = (char '\\' >> char 'n' >> return '\n') 
                 +++ sat (/= '"')


    intLiteral = many1 digit >>= \d -> return (Val (VInt (read d)))

    boolLiteral = many1 letter >>= \d ->
                case (d) of "True" ->  return (Val (VBool True))
                            "False" -> return (Val (VBool False))
                            otherwise -> variableLiteral
                            
    variableLiteral = many1 alphanum >>= \d ->
                        return ((Var d))          
-- expression help

  
    andSeq left = ((symbol "&&" +++ symbol "||") >>= \s ->
                      andil >>= \right -> 
                      andSeq((toOp s) left right)
                    ) +++ return left
    and' = (
                boolLiteral >>= \d ->
                whitespace >>
                return (d) 
            ) +++ parens expr
    
    var = (
                variableLiteral >>= \d ->
                whitespace >>
                return (d) 
            ) +++ parens expr
    
    andil = equality >>= equalitySeq ----------



    equalitySeq left = ((symbol "==" +++ symbol "!=" +++ symbol "<=" +++ symbol ">=") >>= \s ->
                      equality >>= \right -> 
                      equalitySeq ((toOp s) left right)
                    ) +++ return left

    equality = grtity >>= grtSeq

    grtSeq left = ((symbol "<" +++ symbol ">" ) >>= \s ->
                      grtity >>= \right -> 
                      grtSeq ((toOp s) left right)
                    ) +++ return left
    
    grtity = term >>= termSeq  ----------    
    
    termSeq left = ( (symbol "+" +++ symbol "-") >>= \s ->
                    term >>= \right ->
                    termSeq ((toOp s) left right)
                  ) +++ return left

    term = factor >>= factorSeq 

    factorSeq left = ( (symbol "*" +++ symbol "/") >>= \s ->
                      factor >>= \right ->
                      factorSeq ((toOp s) left right)
                    ) +++ return left

    notSeq = (char '!') >> 
              factor >>= \right ->
                return (Not right)

    factor = ( (nat +++ negNat) >>= \i ->
              return (Val (VInt i))
            ) +++ and' +++ var +++ notSeq +++ parens expr

    negNat :: Parser Int
    negNat = char ('-') >>
              many1 digit >>= \d ->
                whitespace >>
                return (-1 * read d)
  

    toOp "+" = Plus
    toOp "-" = Minus
    toOp "*" = Multiplies
    toOp "/" = Divides
    toOp "==" = Equals
    toOp "!=" = NotEqual
    toOp "<" = Less
    toOp "<=" = LessOrEqual
    toOp ">" = Greater
    toOp ">=" = GreaterOrEqual
    toOp "&&" = And
    toOp "||" = Or
    toOp1 "!" = Not

    ----------------------
    -- Parser utilities --
    ----------------------

    keywords = words "var if else while"
    isKeyword s = s `elem` keywords

    keyword s = 
      identifier >>= \s' ->
      if s' == s then return s else failure     
       
    newtype Parser a = P (String -> [(a, String)])
    
    parse :: Parser a -> String -> [(a, String)]
    parse (P p) inp = p inp
    
    instance Functor Parser where
        fmap = liftM
     
    instance Applicative Parser where
        pure  = return
        (<*>) = ap
    
    instance Monad Parser where
        -- return :: a -> Parser a
        return v = P $ \inp -> [(v, inp)]
                 
        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= q = P $ \inp -> case parse p inp of 
                                [] -> []
                                [(v, inp')] -> let q' = q v in parse q' inp'
    
    failure :: Parser a
    failure = P $ \_ -> []
    
    item :: Parser Char 
    item = P $ \inp -> case inp of 
                         (x:xs) -> [(x, xs)]
                         [] -> []
    
    -- Parse with p or q
    (+++) :: Parser a -> Parser a -> Parser a
    p +++ q = P $ \inp -> case parse p inp of 
                              [] -> parse q inp
                              [(v, inp')] -> [(v, inp')]
    
    
    -- Simple helper parsers
    sat :: (Char -> Bool) -> Parser Char
    sat pred = item >>= \c ->
               if pred c then return c else failure
    
    digit, letter, alphanum :: Parser Char
    digit = sat isDigit
    letter = sat isAlpha
    alphanum = sat isAlphaNum
    
    char :: Char -> Parser Char
    char x = sat (== x)
    
    string = sequence . map char 
    
    many1 :: Parser a -> Parser [a]
    many1 p = p >>= \v ->
              many p >>= \vs ->
              return (v:vs)
    
    many :: Parser a -> Parser [a]
    many p = many1 p +++ return []
    
    -- Useful building blocks
    nat :: Parser Int
    nat = many1 digit >>= \s ->
          whitespace >>
          return (read s)
    
    identifier :: Parser String
    identifier = letter >>= \s ->
                 many alphanum >>= \ss ->
                 whitespace >>
                 return (s:ss)
    
    whitespace :: Parser ()
    whitespace = many (sat isSpace) >> comment
    
    symbol s = 
        string s >>= \s' ->
        whitespace >>
        return s'    
    
    comment = ( string "//" >>
                many (sat (/= '\n')) >>
                whitespace ) +++ return ()
    
    parens p = 
        symbol "(" >> 
        p >>= \res ->
        symbol ")" >>
        return res
