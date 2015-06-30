module LaTeX (plugin) where

-- This plugin allows you to include a LaTeX body in a page like this:
--
-- ~~~ { .latex }
-- \begin{tikzpicture}[sibling distance=48pt]
--   \Tree [ .{$a \wedge b$}
--           [ .{$\neg a \vee c$} 
--             [ .{$\neg c \vee \neg b$}
--               [ .\node(n1){$\neg c1$}; {$\neg a$} \node(n2){$c$}; ]
--               [ .\node(m1){$\neg b$}; [ .{$a$} \node(m2){$b$}; ] ]
--             ]
--           ]
--         ]
--   \draw[->] (n2)..controls +(north east:1) and +(east:1)..(n1);
--   \draw[->] (m2)..controls +(east:1) and +(east:1)..(m1);
-- \end{tikzpicture}
-- ~~~
--
-- Study the skeleton of the LaTeX source file at the end of this file.
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

------------------------------------------------------------------------------
-- # latex eqn.tex
-- # dvips -q -f -e 0 -E -D 10000 -x 10000 -o eqn.ps eqn.dvi
-- # pstoedit -f plot-svg -dt -ssp eqn.ps eqn.svg
plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "latex" `elem` classes = do
  cfg <- askConfig
  let (name, outfile) =  case lookup "name" namevals of
                           Just fn   -> ([Str fn], fn <.> "svg")
                           Nothing   -> ([], uniqueName contents <.> "svg")
  liftIO $ do
    let basePath = staticDir cfg </> "img" </> outfile 
    --------------------------------------------------------------------------
    -- $ cat > ${TEMP}.tex
    --------------------------------------------------------------------------
    let outfilePath = outfile <.> "tex"
    writeFile outfilePath (makebody contents)
    --------------------------------------------------------------------------
    -- $ latex ${TEMP}.tex
    --------------------------------------------------------------------------
    let infilePath = outfilePath
    let outfilePath = outfile <.> "pdf"
    performUnixCommand "pdflatex" [infilePath]
    --------------------------------------------------------------------------
    -- $ pdfcrop ${TEMP}.pdf
    --------------------------------------------------------------------------
    let infilePath = outfilePath
    let outfilePath = outfile ++ "-crop" <.> "pdf"
    performUnixCommand "pdfcrop" [infilePath]
    --------------------------------------------------------------------------
    -- $ pdf2ps ${TEMP}.pdf ${TEMP}.ps
    --------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = outfile ++ "-crop" <.> "ps"
    --performUnixCommand "pdf2ps" [infilePath,outfilePath]
    --------------------------------------------------------------------------
     -- $ dvips -q -f -e 0 -E -D 10000 -x 10000 -o ${TEMP}.ps ${TEMP}.dvi
    --------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = outfile <.> "ps"
    --performUnixCommand "dvips" ["-q", "-f", "-e", "0", "-E", "-D", "10000", "-x", "2000", "-o", outfilePath, infilePath]
    --------------------------------------------------------------------------
    -- $ pstoedit -f plot-svg -dt -ssp ${TEMP}.ps ${TEMP}.svg
    --------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = basePath
    --performUnixCommand "pstoedit" ["-f", "plot-svg", "-dt", "-ssp", infilePath, outfilePath]
    --------------------------------------------------------------------------
    -- $ pdf2svg ${TEMP}.pdf ${TEMP}.svg
    --------------------------------------------------------------------------
    let infilePath = outfilePath
    let outfilePath = basePath
    performUnixCommand "pdf2svg" [infilePath, outfilePath]
    --------------------------------------------------------------------------
    -- ### html
    --------------------------------------------------------------------------
--    return $ RawHtml ("<p><object type=\"image/svg+xml\" data=\""
    return $ RawBlock (Format "html") ("<p><object type=\"image/svg+xml\" data=\""
                      ++ ("/img" </> outfile)
                      ++ "\"></object></p>")
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

performUnixCommand cmd args = do
  (exitcode, stdout, stderr) <- readProcessWithExitCode cmd args ""
  if exitcode == ExitSuccess
    then return ()
    else error $ concat [ "\nError! "
                        , cmd
                        , " returned error status "
                        , show exitcode
                        , "\n  with stdout: \n"
                        , stdout
                        , "\n  with stdout: \n"
                        , stderr
                        ]

makebody xx
    = unlines
      [ "\\documentclass{article}"
--      , "\\raggedbottom"
      , "\\thispagestyle{empty}"
      , "\\nofiles"
      , "\\usepackage[left=1mm,right=1mm,top=1mm,bottom=1mm]{geometry}"
      , "\\usepackage{amsmath}"
--      , "\\usepackage{wasysym}"
      , "\\usepackage[all]{xy}"
      , "\\usepackage{tikz}"
      , "\\usepackage{tikz-qtree}"
      , "\\usepackage{braket}"
      , "\\begin{document}"
--      , "\\xymatrix{"
      , xx 
--      , "}"
      , "\\pagebreak"
      , "\\end{document}"
      ]
