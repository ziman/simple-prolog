import qualified Data.Set as S
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import System.Environment
import System.IO
import System.IO.Error
import System.Console.Haskeline hiding (catch)
import Text.Parsec

import Term
import Parse
import SLD

consult :: String -> Program -> IO Program
consult fn cs = do
    result <- parse program fn <$> safeRead (fn ++ ".pl")
    case result of
        Left err -> putStrLn ("Parse error: " ++ show err) >> return cs
        Right cs -> return cs
  where
    safeRead fn = readFile fn `catchIOError` (\e -> print e >> return [])

printResults :: [Name] -> [Subst] -> IO ()
printResults _ [] = putStrLn "No."
printResults ns (theta:thetas) = do
    case ns of
        [] -> putStrLn "Yes."
        _  -> putStr $ "Yes: \n" ++ unlines (map format binds)
    putStr $ "(<enter> = more results, .<enter> = end) "
    hFlush stdout
    s <- getLine
    case s of
        "" -> printResults ns thetas
        _  -> return()
  where
    binds        = map (id &&& theta) ns
    format (n,t) = "  " ++ n ++ " = " ++ show t

runQuery :: Query -> Program -> IO ()
runQuery qs ps = printResults (S.toList $ vars qs) (sld ps qs)

prompt :: Program -> InputT IO ()
prompt cs = do
    q <- parse query "[cmdline]" . maybe "halt." id <$> getInputLine ":- "
    case q of
        Left err -> outputStrLn $ "Parse error: " ++ show err
        Right [Fun "halt" []] -> outputStrLn "Bye."
        Right [Fun "consult" [Fun fn []]] -> liftIO (consult fn cs) >>= prompt
        Right qs              -> liftIO (runQuery qs cs) >> prompt cs
        
main = runInputT defaultSettings (prompt [])
