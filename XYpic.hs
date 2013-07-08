-- in bash, this script would be:

-- TEMP=$(mktemp tmp.XYpic-plugin.XXXXXXXXXX)
-- cat > ${TEMP}.tex << EOF
-- \thispagestyle{empty}
-- \documentclass{article}
-- \nofiles
-- \usepackage[all]{xy}
-- \begin{document}
-- \xymatrix{
-- EOF
-- cat $1 >> ${TEMP}.tex
-- cat >> ${TEMP}.tex << EOF
-- }
-- \end{document}
-- EOF
-- pdflatex ${TEMP}.tex
-- pdfcrop ${TEMP}.pdf
-- convert ${TEMP}-crop.pdf ${2}.png 
-- ### rm ${TEMP}*

module XYpic (plugin) where

-- This plugin allows you to include a graphviz xypic diagram
-- in a page like this:
--
-- ~~~ {.xypic name="diagram1"}
--   T^3 \ar[d]_{\mu T}  \ar[r]^{T \mu}              & T^2 \ar[d]^{\mu}  
-- & T   \ar[d]_{T \eta} \ar[r]^{\eta T} \ar@{=}[dr] & T^2 \ar[d]^{\mu}  \\
--   T^2 \ar[r]_{\mu}                                & T
-- & T^2 \ar[r]_{\mu}                                & T
-- a
-- ~~~
--
-- The "pdflatex", "pdfcrop", and "convert" executabless must be in the path.
-- The generated png file will be saved in the static img directory.
-- If no name is specified, a unique name will be generated from a hash
-- of the file contents.

import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>), (<.>))
import System.IO (writeFile)
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "xypic" `elem` classes = do
  cfg <- askConfig
  let (name, outfile) =  case lookup "name" namevals of
                           Just fn   -> ([Str fn], fn ++ ".png")
                           Nothing   -> ([], uniqueName contents ++ ".png")
  liftIO $ do
    let basePath = staticDir cfg </> "img" </> outfile 
    -- $ cat > ${TEMP}
    -- $ pdflatex ${TEMP}.tex
    let infilePath = outfile <.> "tex"
    writeFile infilePath (makebody contents)
    performUnixCommand "pdflatex" [infilePath]
    -- $ pdfcrop ${TEMP}.pdf
    let infilePath = outfile <.> "pdf"
    performUnixCommand "pdfcrop" [infilePath]
    -- $ convert ${TEMP}-crop.pdf ${2}.png 
    let infilePath = outfile ++ "-crop" <.> "pdf"
    let outfilePath = basePath
    performUnixCommand "convert" [infilePath, outfilePath]
    -- # html
    return $ Para [Image name ("/img" </> outfile, "")]
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

makebody xx = unlines [ "\\documentclass{article}"
                      , "\\thispagestyle{empty}"
                      , "\\nofiles"
                      , "\\usepackage[all]{xy}"
                      , "\\begin{document}"
                      , "\\xymatrix{"
                      , xx 
                      , "}"
                      , "\\end{document}"
                      ]

performUnixCommand cmd args = do
  (exitcode, stdout, stderr) <- readProcessWithExitCode cmd args ""
  if exitcode == ExitSuccess
    then return ()
    else error $ concat [ "Error! "
                        , cmd
                        , " returned error status "
                        , show exitcode
                        , " with output: "
                        , stderr
                        ]

