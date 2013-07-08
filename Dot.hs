module Dot (plugin) where

-- This plugin allows you to include a graphviz dot diagram
-- in a page like this:
--
-- ~~~ {.dot name="diagram1"}
-- digraph G {Hello->World}
-- ~~~
--
-- The "dot" executable must be in the path.
-- The generated png file will be saved in the static img directory.
-- If no name is specified, a unique name will be generated from a hash
-- of the file contents.

import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import Data.ByteString.Lazy.UTF8 (fromString)  -- from HackageDB
import Data.Digest.Pure.SHA (sha1, showDigest)  -- from HackageDB
import System.FilePath ((</>))
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "dot" `elem` classes = do
  cfg <- askConfig
  let outfile = case lookup "name" namevals of
                  Just fn   -> fn ++ ".svg"
                  Nothing   -> uniqueName contents ++ ".svg"
  liftIO $ do
    (ec, _out, _err) <- readProcessWithExitCode "dot"
                         [ "-Tsvg", "-o", staticDir cfg </> "img" </> outfile ]
                         contents
    if ec == ExitSuccess
--       then return $ RawHtml ("<p><object "
       then return $ RawBlock "html" ("<p><object "
                              ++ " type=\"image/svg+xml\""
                              ++ " data=\"" ++ ("/img" </> outfile) ++ "\""
--                              ++ " height=100%"
--                              ++ " width=100%"
                              ++ "></object></p>")
       else error $ "dot returned error status: " ++ show ec ++ "\n" ++ _out ++ "\n" ++ _err
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

