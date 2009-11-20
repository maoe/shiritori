{-# LANGUAGE FlexibleContexts #-}
module Main where
import Text.Parsec
import Text.Parsec.Pos
import Control.Applicative ((<*>), (*>), (<*), (<$>))
import Control.Monad.Trans

main :: IO ()
main = let f = "doubutsu.txt"
       in readFile f >>=
          runParserT pNote [] f >>=
          either (print) (mapM_ print)

type Note = [Content]
data Content = Line String
             | Included Note
             deriving Show

pLine :: Stream s m Char => ParsecT s u m Content
pLine = Line <$> lexeme (many1 (satisfy (/= '\n')))

pInclude :: ParsecT String [Context] IO Content
pInclude = do
  Line f <- try (lexeme (string "include")) *> pLine
  note <- newContext $ pFile f
  return $ Included note

pFile :: SourceName -> ParsecT String [Context] IO Note
pFile f = do
  setPosition $ initialPos f
  setInput =<< liftIO (readFile f)
  pNote

pNote :: ParsecT String [Context] IO Note
pNote = many $ pInclude <|> pLine

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = p <* spaces

data Context = Context { ctxPos :: SourcePos
                       , ctxInp :: String }

newContext :: Monad m
           => ParsecT String [Context] m a
           -> ParsecT String [Context] m a
newContext act = do
  pushState =<< Context <$> getPosition <*> getInput
  r <- act
  Context pos inp <- popState
  setPosition pos
  setInput inp
  return r

pushState :: Monad m => a -> ParsecT s [a] m ()
pushState s = getState >>= putState . (s:)

popState :: Monad m => ParsecT s [a] m a
popState = do
  (s:ss) <- getState
  putState ss
  return s
