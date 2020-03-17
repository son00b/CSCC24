module Turbo where

    import           Control.Applicative
    import           Data.Map (Map)
    import qualified Data.Map as Map
    
    import           TurboDef
    
    
    -- "Run" a Turbo program to produce SVG path commands.
    -- This is the only function that will be tested.
    runTurbo :: Stmt -> [SVGPathCmd]
    runTurbo stmt = snd (deState (turbo stmt) initTurboMem)
    -- But the actual execution engine and recursion are in "turbo" and "evalReal"
    -- below.
    
    -- Evaluate an expression. The State monad is needed to look up variables.
    evalReal :: RealExpr -> State TurboMem Double
    evalReal (RLit r) = do
        return r  -- the value
    evalReal (RVar var) = do
        r <- getVar var
        return r  -- the value
    evalReal (Neg expr) = do
        r <- evalReal expr  -- the value
        return (negate r)   -- the negated value
    evalReal (expr1 :+ expr2) = do
        -- evaluate both expressions, then add the values together
        r1 <- evalReal expr1
        r2 <- evalReal expr2
        return (r1 + r2)  -- the result value after addition
    evalReal (expr1 :- expr2) = do
        -- evaluate both expressions, and after getting both values,
        -- subtract the second result from the first
        r1 <- evalReal expr1
        r2 <- evalReal expr2
        return (r1 - r2)      -- the result value after subtraction
    evalReal (expr1 :* expr2) = do
        r1 <- evalReal expr1
        r2 <- evalReal expr2
        return (r1 * r2)      -- the result value after multiplication
    evalReal (expr1 :/ expr2) = do
        -- evaluate both expressions, and after getting both values,
        -- divide the first result by the second
        r1 <- evalReal expr1
        r2 <- evalReal expr2
        return (r1 / r2)      -- the result value after division
    
    -- Run a Turbo statement. Use the State monad to keep state. Return SVG path
    -- commands.
    turbo :: Stmt -> State TurboMem [SVGPathCmd]
    turbo (str := expr) = do 
        val <- evalReal expr  -- the value
        setVar str val        -- map the key to value
        return []
    turbo PenDown = do
        -- set pen to be down
        setPen True
        return []
    turbo PenUp = do
        -- set pen to be up
        setPen False
        return []
    turbo (Turn deg) = do
        degExpr <- evalReal deg  -- the angle in degrees
        -- turn by the angle
        turn degExpr
        return []
    turbo (Forward distanceExpr) = do
        distance <- evalReal distanceExpr  -- the distance to move
        angle <- getAngle                  -- the angle
        pen <- getPen                      -- whether pen is up or down
        -- if pen is down
        if pen == True
            then do
                -- draw a line at the right distance and angle
                return [(LineTo (distance * cos (angle * pi / 180))
                        (distance * sin (angle * pi / 180)))]
            else do
                -- move to the right distance and angle
                return [(MoveTo (distance * cos (angle * pi / 180)) 
                        (distance * sin (angle * pi / 180)))]
    turbo (Seq (x:xs)) = do
        y <- turbo x          -- evaluate the first stmt in the list
        ys <- turbo (Seq xs)  -- evaluate the rest of the stmt in the list
        return (y ++ ys)      -- join the results together
    turbo (Seq []) = do
        -- if list of stmts is empty, do nothing
        return []
    turbo (For str expr1 expr2 s) = do
        -- initialize the variable to be the lower bound of for loop
        eval1 <- evalReal expr1     -- the lower bound of for loop
        setVar str eval1
        eval2 <- evalReal expr2     -- the upper bound of for loop
        -- get the result of the for loop
        forTurbo str eval1 eval2 s
    
    -- helper function for the for loop of turbo
    -- given string str, double eval, double eval2, list of stmts
    -- for i (value of str) in range (eval1, eval2) do all stmts in the list
    forTurbo :: String -> Double -> Double -> [Stmt] -> State TurboMem [SVGPathCmd]
    forTurbo str eval1 eval2 s = do
        val <- getVar str  -- get i = value of the variable
        -- if i is in range, do statements. Otherwise, do nothing
        if val <= eval2
            then do 
                path1 <- turbo (Seq s)               -- result after doing stmts
                setVar str (val + 1)                 -- i++
                path2 <- forTurbo str eval1 eval2 s  -- all other iterations result
                return (path1 ++ path2)              -- join the results together
            else do return []
    
    -- Turbo state:
    -- * dictionary of variables->values
    -- * current direction (degrees away from x-axis, counterclockwise, e.g.,
    --   0 points west, 90 points north)
    -- * pen state (True means touching paper)
    data TurboMem = TurboMem (Map String Double) Double Bool
        deriving (Eq, Show)
    
    -- Initial Turbo state: No variables set, direction is 0 degrees, pen is up.
    initTurboMem = TurboMem Map.empty 0 False
    
    -- If you could code up the following helpers, your "turbo" implementation 
    -- could be pretty clean and easy to follow.  fmap, get, modify, Map.lookup,
    -- and Map.insert will get you a long way.
    
    -- Get current direction.
    getAngle :: State TurboMem Double
    getAngle = do
        (TurboMem dictionary direction pen) <- get  -- current state
        return direction                            -- direction of current state
    
    -- Change direction by adding the given angle.
    turn :: Double -> State TurboMem ()
    turn newAngle = do
        (TurboMem dictionary angle pen) <- get            -- current state
        put (TurboMem dictionary (angle + newAngle) pen)  -- update angle
    
    -- Get pen state.
    getPen :: State TurboMem Bool
    getPen = do
        (TurboMem dictionary direction pen) <- get  -- current state
        return pen                                  -- pen position
    
    -- Set pen state.
    setPen :: Bool -> State TurboMem ()
    setPen newPen = do
        (TurboMem dictionary angle pen) <- get  -- current state
        put (TurboMem dictionary angle newPen)  -- update pen
    
    -- Get a variable's current value.
    getVar :: String -> State TurboMem Double
    getVar str = do
        (TurboMem dictionary angle pen) <- get  -- current state
        -- if the variable is in dictionary, return the value. otherwise, return 0
        case Map.lookup str dictionary of
            Nothing -> return 0
            Just val -> return val
    
    -- Set a variable to value.
    setVar :: String -> Double -> State TurboMem ()
    setVar str val = do 
        (TurboMem dictionary angle pen) <- get                    -- current state
        put (TurboMem (Map.insert str val dictionary) angle pen)  -- update map
    