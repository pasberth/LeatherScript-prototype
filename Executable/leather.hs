import           Control.Applicative
import           Control.Monad
import qualified Data.Maybe                     as Maybe
import qualified Data.ByteString.Lazy.UTF8      as ByteString
import qualified Data.Text                      as Text
import qualified Data.HashSet                   as HashSet
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.Vector                    as Vector
import qualified Data.Aeson                     as Aeson
import qualified Language.LeatherScript.Tokenizer as Tokenizer
import qualified Language.LeatherScript.Parser  as Parser
import qualified Language.LeatherScript.AST     as AST
import qualified Language.LeatherScript.Generator as Generator
import qualified Language.LeatherScript.SyntaxDef as SyntaxDef
import qualified Language.LeatherScript.LeatherShield as LeatherShield
import qualified System.FilePath.Posix          as FilePath
import qualified System.Directory               as Directory
import qualified System.Environment             as Environment

data Option
  = Stx FilePath
  | Src FilePath

stxfiles :: [Option] -> [FilePath]
stxfiles (Stx x:xs) = x : stxfiles xs
stxfiles (x:xs) = stxfiles xs
stxfiles [] = []

srcfiles :: [Option] -> [FilePath]
srcfiles [] = []
srcfiles (Src x:xs) = x : srcfiles xs
srcfiles (_:xs) = srcfiles xs

parseArgs :: [String] -> [Option]
parseArgs ("--stx":x:xs) = Stx x : parseArgs xs
parseArgs xs = map Src xs

main :: IO ()
main = do
  opts <- parseArgs <$> Environment.getArgs
 
  let stx = stxfiles opts
  let srcs = srcfiles opts

  stxDefs <- mapM SyntaxDef.mkParserFromFile stx

  let (tokenDef, keywords, notations) = foldl (\(x1,y1,z1) (x2,y2,z2)  -> ((Vector.++) x1 x2, HashSet.union y1 y2, HashMap.union z1 z2)) (Vector.empty, HashSet.empty, HashMap.empty) (Maybe.catMaybes stxDefs)

  forM_ srcs $ \src -> do
    content <- Text.pack <$> readFile src
    let tokens = Tokenizer.runTokenizer (Tokenizer.tokenizeIgnoreSpaces src content) tokenDef
    case SyntaxDef.parseText src tokenDef keywords notations content of
      Left err -> print err
      Right st -> do
        let ast = AST.fromSyntaxTree tokens st

        --typ <- LeatherShield.runLeatherShieldT (LeatherShield.leatherShield ast) LeatherShield.emptyLeatherShield

        --case typ of
        --  Left typeError -> print typeError
        --  Right _ -> do
        --    return ()

        let json = Aeson.encode ast

        let lth_json = FilePath.replaceExtension src "lth.json"
        let js_json = FilePath.replaceExtension src "js.json"
        writeFile lth_json (ByteString.toString json)
        writeFile js_json (ByteString.toString (Aeson.encode (Generator.fromAST ast)))



