-- test reification of global definitions
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax
import System.IO


g :: Int
g = 1

main :: IO ()
main =
    $(do
      let printTypeOf n = do
            addModFinalizer $ do
              VarI _ t _ <- reify (mkName n)
              runIO $ hPutStrLn stderr (n ++ " :: " ++ show t)
      printTypeOf "g"
      ds <- [d| f = True |]
      addTopDecls ds
      printTypeOf "f"
      [| return () |]
     )
