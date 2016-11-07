{-# LANGUAGE TypeFamilies, DataKinds, TypeApplications, AllowAmbiguousTypes, UndecidableInstances, RankNTypes, TypeOperators, ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}

import GHC.TypeLits
import Data.Proxy

data {-kind-} Chunk = I | S | C Nat

type family PrintfParse (chars :: [Nat]) :: [Chunk] where
  PrintfParse (37 ': 100 ': xs) = I ': PrintfParse xs -- %d
  PrintfParse (37 ': 115 ': xs) = S ': PrintfParse xs -- %s
  PrintfParse (x ': xs) = C x ': PrintfParse xs       -- any character
  PrintfParse '[] = '[]

class PrintfChunks (xs :: [Chunk]) where
  type PrintfT xs :: *
  do_printf :: proxy xs -> IO () -> PrintfT xs

instance PrintfChunks '[] where
  type PrintfT '[] = IO ()
  do_printf _ io = io

instance (KnownNat n, PrintfChunks xs) => PrintfChunks (C n ': xs) where
  type PrintfT (C n ': xs) = PrintfT xs
  do_printf _ io = do_printf (Proxy :: Proxy xs) (io >> putChar (toEnum $ fromInteger $ natVal (Proxy :: Proxy n)))

instance PrintfChunks xs => PrintfChunks (I ': xs) where
  type PrintfT (I ': xs) = Int -> PrintfT xs
  do_printf _ io val = do_printf (Proxy :: Proxy xs) (io >> putStr (show val))

instance PrintfChunks xs => PrintfChunks (S ': xs) where
  type PrintfT (S ': xs) = String -> PrintfT xs
  do_printf _ io val = do_printf (Proxy :: Proxy xs) (io >> putStr val)

printf :: forall (format :: Symbol). PrintfChunks (PrintfParse (SymbolVal format)) => PrintfT (PrintfParse (SymbolVal format))
printf = do_printf (Proxy @(PrintfParse (SymbolVal format))) (return ())

main = printf @"int: %d string: %s\n" 17 "foo"
