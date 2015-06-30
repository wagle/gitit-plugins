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
------------------------------------------------------------------
-- # latex eqn.tex
-- # dvips -q -f -e 0 -E -D 10000 -x 10000 -o eqn.ps eqn.dvi
-- # pstoedit -f plot-svg -dt -ssp eqn.ps eqn.svg

module XYpic2 (plugin) where

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

------------------------------------------------------------------
-- # latex eqn.tex
-- # dvips -q -f -e 0 -E -D 10000 -x 10000 -o eqn.ps eqn.dvi
-- # pstoedit -f plot-svg -dt -ssp eqn.ps eqn.svg
plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "xypic2" `elem` classes = do
  cfg <- askConfig
  let (name, outfile) =  case lookup "name" namevals of
                           Just fn   -> ([Str fn], fn <.> "svg")
                           Nothing   -> ([], uniqueName contents <.> "svg")
  liftIO $ do
    let basePath = staticDir cfg </> "img" </> outfile 
    ---------------------------------------------------------------------------------
    -- $ cat > ${TEMP}.tex
    ---------------------------------------------------------------------------------
    let outfilePath = outfile <.> "tex"
    writeFile outfilePath (makebody contents)
    ---------------------------------------------------------------------------------
    -- $ latex ${TEMP}.tex
    ---------------------------------------------------------------------------------
    let infilePath = outfilePath
    let outfilePath = outfile <.> "pdf"
    performUnixCommand "pdflatex" [infilePath]
    ---------------------------------------------------------------------------------
    -- $ pdfcrop ${TEMP}.pdf
    ---------------------------------------------------------------------------------
    let infilePath = outfilePath
    let outfilePath = outfile ++ "-crop" <.> "pdf"
    performUnixCommand "pdfcrop" [infilePath]
    ---------------------------------------------------------------------------------
    -- $ pdf2ps ${TEMP}.pdf ${TEMP}.ps
    ---------------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = outfile ++ "-crop" <.> "ps"
    --performUnixCommand "pdf2ps" [infilePath,outfilePath]
    ---------------------------------------------------------------------------------
     -- $ dvips -q -f -e 0 -E -D 10000 -x 10000 -o ${TEMP}.ps ${TEMP}.dvi
    ---------------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = outfile <.> "ps"
    --performUnixCommand "dvips" ["-q", "-f", "-e", "0", "-E", "-D", "10000", "-x", "2000", "-o", outfilePath, infilePath]
    ---------------------------------------------------------------------------------
    -- $ pstoedit -f plot-svg -dt -ssp ${TEMP}.ps ${TEMP}.svg
    ---------------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = basePath
    --performUnixCommand "pstoedit" ["-f", "plot-svg", "-dt", "-ssp", infilePath, outfilePath]
    ---------------------------------------------------------------------------------
    -- $ pdf2svg ${TEMP}.pdf ${TEMP}.svg
    ---------------------------------------------------------------------------------
    let infilePath = outfilePath
    let outfilePath = basePath
    performUnixCommand "pdf2svg" [infilePath, outfilePath]
    ---------------------------------------------------------------------------------
    -- ### html
    ---------------------------------------------------------------------------------
    return $ RawBlock (Format "html") ("<p><object height=84 type=\"image/svg+xml\" data=\""
--    return $ RawHtml ("<p><object height=84 type=\"image/svg+xml\" data=\""
                      ++ ("/img" </> outfile)
                      ++ "\"></object></p>")
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

makebody xx
    = unlines
      [ "\\documentclass{article}"
--      , "\\raggedbottom"
      , "\\thispagestyle{empty}"
      , "\\nofiles"
      , "\\usepackage[all]{xy}"
      , "\\begin{document}"
      , "\\xymatrix{"
      , xx 
      , "}"
      , "\\pagebreak"
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

