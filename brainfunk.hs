{-# LANGUAGE DeriveDataTypeable #-}
                
import Language.Brainfuck
import System.Console.CmdArgs

versionNumber :: String
versionNumber = "0.1"

data Config = Config
    { bufferSize :: Maybe Int
    , debug :: Bool 
    , file :: Maybe String 
    } deriving (Show, Data, Typeable)

config :: Config
config = Config
    { bufferSize = def &= help "Maximum buffer size" 
    , debug      = def &= help "Debugging mode -- CURRENTLY DISABLED" 
    , file       = def &= args 
    } &= summary ("Brainfunk " ++ versionNumber ++ " -- Brainfuck interpreter in Haskell")

main :: IO ()
main = do
    cfg <- cmdArgs config
    let bufsize = 
          case bufferSize cfg of
            Just s -> s
            _      -> 30270
    fileContents <-
      case file cfg of
        Just f -> readFile f
        _      -> fail "Need file. See -? or --help for help."

    let context = (([], replicate bufsize 0), ("", fileContents))
    _ <- runProgram context
    return ()
