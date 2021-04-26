module W ( WValue(..), 
           WExp(..), 
           WStmt(..), 
           exec, 
           eval) where

    data WValue = VInt Int 
                | VBool Bool 
                | VString String
                | VMarker
                  deriving Eq
    
    instance Show WValue where
        show (VInt i) = show i
        show (VBool b) = show b
        show (VString s) = s
        show (VMarker) = "_"

    data WExp = Val WValue
                
              | Var String
                
              | Plus WExp WExp
              | Minus WExp WExp
              | Multiplies WExp WExp
              | Divides WExp WExp
                
              | Equals WExp WExp
              | NotEqual WExp WExp
              | Less WExp WExp
              | Greater WExp WExp
              | LessOrEqual WExp WExp
              | GreaterOrEqual WExp WExp
    
              | And WExp WExp
              | Or WExp WExp
              | Not WExp
                deriving Show

    data WStmt = Empty 
               | VarDecl String WExp
               | Assign String WExp
               | If WExp WStmt WStmt
               | While WExp WStmt
               | Block [WStmt]
               | Print WExp
                 deriving Show

    -- some useful helper functions
    asInt (VInt v) = v
    asInt x = error $ "Expected a number, got " ++ show x
    
    asBool (VBool v) = v
    asBool x = error $ "Expected a boolean, got " ++ show x

    ----------
    -- eval --
    ----------
    type Memory = [(String, WValue)]
    
    marker = ("|", VMarker)
    isMarker (x, _) = x == "|"
    
    eval :: WExp -> Memory -> WValue
    
    eval (Val v) _ = v
    
    eval (Var s) m =
      case lookup s m of
        Nothing -> error $ "Unknown variable " ++ s ++ " in memory " ++ show m
        Just v -> v
    
    eval (Plus e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VInt $ asInt e1' + asInt e2'
    
    eval (Minus e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VInt $ asInt e1' - asInt e2'
    
    eval (Multiplies e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VInt $ asInt e1' * asInt e2'
    
    eval (Divides e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VInt $ asInt e1' `div` asInt e2'
    
    eval (Equals e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VBool $ e1' == e2'
    
    eval (NotEqual e1 e2) m = VBool $ not $ asBool $ eval (Equals e1 e2) m
    
    eval (Less e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VBool $ asInt e1' < asInt e2'
    
    eval (LessOrEqual e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VBool $ asInt e1' <= asInt e2'
    
    eval (Greater e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VBool $ asInt e1' > asInt e2'
    
    eval (GreaterOrEqual e1 e2) m =
      let e1' = eval e1 m
          e2' = eval e2 m
      in
       VBool $ asInt e1' >= asInt e2'
    
    eval (And e1 e2) m | not (asBool (eval e1 m)) = VBool False
                       | otherwise = VBool (asBool (eval e2 m))
    
    eval (Or e1 e2) m | asBool (eval e1 m) = VBool True
                      | otherwise = VBool (asBool (eval e2 m))
    
    eval (Not e) m = VBool $ not $ asBool $ eval e m
    
    ----------
    -- exec --
    ----------
    exec :: WStmt -> Memory -> IO Memory
    exec Empty m = return m
               
    exec (VarDecl s e) m | not (definedInThisScope m) = return $ (s, eval e m) : m
                         | otherwise = error $ "Variable " ++ s ++ " already defined in this scope"
        where
          definedInThisScope (hd@(d, _):ds) | isMarker hd = False
                                            | d == s = True
                                            | otherwise = definedInThisScope ds                                              
                                                          
    exec (Assign s e) m = return $ replaceFirstDef (eval e m) m
        where replaceFirstDef _ [] = error $ "Undefined variable " ++ s ++ " in assignment"
              replaceFirstDef v (hd@(n, _):m) | n == s = (n, v):m
                                              | otherwise = hd:replaceFirstDef v m 
                                                        
    exec (If e s1 s2) m | eval e m == VBool True = exec s1 m
                        | eval e m == VBool False = exec s2 m
                        | otherwise = error "Non-boolean in condition of if"
                                  
    exec (While e s) m | eval e m == VBool True = exec s m >>= \m' ->
                                                  exec (While e s) m'  
                       | eval e m == VBool False = return m
                       | otherwise = error "Non-boolean in condition of while"
    
    exec (Block ss) m = bexec ss (marker:m) >>= \m' -> return (popMarker m')
        where bexec [] m = return m
              bexec (s:ss) m = exec s m >>= \m' ->
                               bexec ss m'
              popMarker [] = []
              popMarker (x:xs) | isMarker x = xs
                               | otherwise = popMarker xs
    
    exec (Print e) m = putStr (show (eval e m)) >> return m
