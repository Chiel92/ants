module AntBrains where

-- What do we want?

func :: FunctionDict -> String
func fd = move func2 fd
func2 :: FunctionDict -> String
func2 fd = move func3 fd
func3 :: FunctionDict -> String
func3 fd = move func fd

result = func []

-- Should translate to
--
--move 1 ; state 0
--move 2 ; state 1
--move 0 ; state 2


type FunctionDict = [(String, EntryPoint)]
type EntryPoint = Int -- Also entry point of function
type AntProgram = [(EntryPoint, String)]


lookupFunctionDict :: FunctionDict -> String -> (EntryPoint,FunctionDict)
lookupFunctionDict fd name =
  let
    findProgramResult = findProgram name fd
    Just foundEntry = findProgramResult
    newEntry = length fd
    newProgram = (name, length fd):fd
  in
    if findProgramResult==Nothing
    then (newEntry, newProgram)
    else (foundEntry, fd)


findProgram :: String -> FunctionDict -> Maybe EntryPoint
findProgram name ((name', entry):rest)
    | name == name' = Just entry
    | otherwise = findProgram name rest
findProgram name [] = Nothing


move :: (FunctionDict -> String) -> FunctionDict -> String
move k fd = "move " ++ show (lookupFunctionDict fd (k fd))

