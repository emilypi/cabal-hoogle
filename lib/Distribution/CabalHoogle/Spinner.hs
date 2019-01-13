{-# LANGUAGE LambdaCase #-}
module Distribution.CabalHoogle.Spinner where

import System.Console.ANSI

import Control.Concurrent (threadDelay, killThread, forkIO)
import Control.Monad ((>=>), forM_)
import Control.Monad.Catch (mask, onException)

import Data.Bool (bool)
import Data.IORef
import Data.List
import Data.Maybe (isNothing)
import Data.Monoid (mconcat)


data TerminalString
  = TerminalString_Normal String
  | TerminalString_Colorized Color String
  deriving (Eq, Show, Ord)

data Output
  = Output_Write [TerminalString]
  | Output_Overwrite [TerminalString]
  | Output_ClearLine
  deriving (Eq, Show, Ord)

type Spinner = [TerminalString]

type SpinnerTheme = [String]

newtype SpinnerStack = SpinnerStack { _mkStack :: IORef ([Bool], [TerminalString]) }

withSpinnerNoTrail
  :: SpinnerStack
  -> String
  -> IO a
  -> IO a
withSpinnerNoTrail ss s = withSpinner ss s Nothing

withSpinner
  :: SpinnerStack
  -> String
  -> Maybe (a -> String)
  -> IO a
  -> IO a
withSpinner (SpinnerStack stack) msg trail action = do
  bracket' run cleanup (const action)
  where
    run = do
      modifyStack pushSpinner >>= bool pureSpinner forkSpinner

    cleanup tids mResult = do
      mapM_ killThread tids
      -- clear the area
      putStr "\r" >> clearLine
      writeM <- modifyStack . popSpinner $ case mResult of
        Nothing ->
          ( TerminalString_Colorized Red "✖"
          , Just msg
          )
        Just result ->
          ( TerminalString_Colorized Green "✔"
          , trail <*> pure result
          )
      -- last message, newline
      forM_ writeM $ putStr . toString

    pushSpinner (flag, old) =
      ((isNothing trail : flag, TerminalString_Normal msg : old), null old)

    popSpinner (mark, mTrailMsg) (flag, old) =
      let inTempSpinner = or newFlag
          newFlag = drop 1 flag
          new = delete (TerminalString_Normal msg) old

      in ( (newFlag, new)
         , renderSpinnerStack mark . (: new) . TerminalString_Normal <$>
           (if inTempSpinner then Nothing else mTrailMsg)
         )

    forkSpinner = do
      spinnerThread <- forkIO $ runSpinner spinner $ \c -> do
        logs <- renderSpinnerStack c . snd <$> readStack
        putStrLn . toString $ logs
      pure [spinnerThread]
    pureSpinner = pure []
    readStack   = readIORef stack
    modifyStack = atomicModifyIORef stack
    spinner     = coloredSpinner spinnerMoon

renderSpinnerStack
  :: TerminalString
  -> [TerminalString]
  -> [TerminalString]
renderSpinnerStack mark = intersperse space . go . reverse
  where
    go [] = []
    go (x:[]) = mark : [x]
    go (x:xs) = arrow : x : go xs
    arrow = TerminalString_Colorized Blue "⇾"
    space = TerminalString_Normal " "

coloredSpinner :: SpinnerTheme -> Spinner
coloredSpinner = cycle . fmap (TerminalString_Colorized Cyan)

spinnerMoon :: SpinnerTheme
spinnerMoon = ["🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘"]

runSpinner
  :: Spinner
  -> (TerminalString -> IO ())
  -> IO ()
runSpinner spin k = forM_ spin $ k >=> const delay
  where
    delay = threadDelay 100000

bracket'
  :: IO a
  -> (a -> Maybe c -> IO b)
  -> (a -> IO c)
  -> IO c
bracket' acquire release use = mask $ \unmasked -> do
  resource <- acquire
  result <- unmasked (use resource) `onException` release resource Nothing
  _ <- release resource (Just result)
  return result

-- | Code to reset ANSI colors
resetCode :: String
resetCode = setSGRCode [Reset]

toString :: [TerminalString] -> String
toString = mconcat . fmap toString'

toString' :: TerminalString -> String
toString' = \case
  TerminalString_Normal s -> s
  TerminalString_Colorized c s -> render c s

render :: Color -> String -> String
render c s = mconcat
  [ setSGRCode [SetColor Foreground Vivid c]
  , s
  , resetCode
  ]
