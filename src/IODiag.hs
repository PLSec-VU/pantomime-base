module IODiag where
import GHC.Base (returnIO, bindIO)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE usePure #-}
usePure :: a -> IO a
usePure x = pure x

{-# NOINLINE useReturnIO #-}
useReturnIO :: a -> IO a
useReturnIO x = returnIO x

{-# NOINLINE useBind #-}
useBind :: IO a -> (a -> IO b) -> IO b
useBind m f = m >>= f

{-# NOINLINE useUnsafe #-}
useUnsafe :: IO a -> a
useUnsafe m = unsafePerformIO m
