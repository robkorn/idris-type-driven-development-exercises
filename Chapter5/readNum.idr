
module readNum

public export
readNumber : IO (Maybe Nat)
readNumber = do
           i <- getLine
           if all isDigit (unpack i)
              then pure $ Just $ cast i
              else pure Nothing

public export
readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
            Just n1ok <- readNumber | Nothing => pure Nothing
            Just n2ok <- readNumber | Nothing => pure Nothing
            pure (Just (n1ok, n2ok))
