{-# LANGUAGE OverloadedStrings #-}
module Text.IPG.Simple ( parse, parseFile, interpret, interpretFile ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import qualified Data.Set as Set -- containers
import qualified Data.Map as Map -- containers

import Text.IPG.Check ( validate )
import Text.IPG.Core as Core
import Text.IPG.Full ( ExpHelpers(..), toCore )
import qualified Text.IPG.GenericExp as E
import qualified Text.IPG.Interpreter as I
import qualified Text.IPG.Parser as P
import Text.IPG.TopLevel.FileSplit ( splitFile )

type T = P.IdType
type Grammar' = Core.Grammar T T T P.Exp'

helper :: ExpHelpers P.IdType P.IdType P.IdType P.Exp'
helper = ExpHelpers {
    len = E.Int . fromIntegral . BS.length,
    add = E.Add,
    num = E.Int . fromIntegral,
    ref = E.Ref
  }  

parseFile
    :: Bool
    -> FilePath
    -> IO (Either [String] (LBS.ByteString, Grammar', [T], LBS.ByteString))
parseFile doValidation f = parse doValidation <$> LBS.readFile f

computeStartLine :: LBS.ByteString -> Int
computeStartLine "" = 1
computeStartLine s = 3 + fromIntegral (LBS.count '\n' s)

parse
    :: Bool
    -> LBS.ByteString
    -> Either [String] (LBS.ByteString, Grammar', [T], LBS.ByteString)
parse doValidation ipgInput' = do
    let (preamble, ipgInput, postamble) = splitFile ipgInput'

    let byteOffset = fromIntegral (LBS.length preamble) -- TODO: This isn't 100% correct.
    let startLine = computeStartLine preamble
    let startCol = 1

    case P.parseWithStartPos byteOffset startLine startCol ipgInput of
        Left err -> Left [err]
        Right (g, decls) -> do
            let core = E.simplify (toCore helper g)
            if doValidation then
                case validate (Set.fromList decls) core of
                    Just errs -> Left (map CBS.unpack errs)
                    Nothing -> Right (preamble, core, decls, postamble)
              else Right (preamble, core, decls, postamble)

interpretFile :: Bool -> FilePath -> LBS.ByteString -> IO (Either [String] (Maybe (I.Value a)))
interpretFile doValidation f input = do
    ipgInput <- LBS.readFile f
    return (interpret doValidation ipgInput input)

interpret :: Bool -> LBS.ByteString -> LBS.ByteString -> Either [String] (Maybe (I.Value a))
interpret doValidation ipgInput input =
    case parse doValidation ipgInput of
        Left errs -> Left errs
        Right (_, g, _, _) ->
            Right (postProcess <$> I.interpret g Map.empty [] (LBS.toStrict input))
  where postProcess (bs, _, _) = I.BINDINGS bs
