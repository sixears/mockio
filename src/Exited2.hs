{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Exited2
  ( Exited( Exited ), ToExitCode( toExitCode )

  , die, dieAbnormal, dieInternal, dieUsage, doMain, doMain'

  , exitCodeSuccess , exitSuccess
  , exitCodeAbnormal, exitAbnormal
  , exitCodeUsage   , exitUsage
  , exitCodeInternal, exitInternal

  , exited, exitWith, exitWith'
  )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import qualified  System.Exit

import Control.Exception       ( Exception, handle, throwIO )
import Control.Monad           ( (>>=), (>>), return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Either             ( Either( Left, Right ) )
import Data.Function           ( ($), id )
import Data.Word               ( Word8 )
import System.Environment      ( getProgName )
import System.Exit             ( ExitCode( ExitFailure, ExitSuccess ) )
import System.IO               ( IO, hPutStrLn, putStrLn, stderr )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode  ( (≡) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadError  ( ѥ )

--------------------------------------------------------------------------------

{- | Marker that we have exited (cf. `()`, which doesn't tell you anything). -}

data Exited = Exited

exited ∷ Exited
exited = Exited

----------------------------------------

{- | Things that may be treated as an ExitCode; specifically, to allow for use
     of a `Word8`. -}
class ToExitCode ξ where
  toExitCode ∷ ξ → ExitCode

instance ToExitCode ExitCode where
  toExitCode = id

instance ToExitCode Word8 where
  toExitCode 0 = ExitSuccess
  toExitCode i = ExitFailure $ fromIntegral i

instance ToExitCode () where
  toExitCode () = ExitSuccess

------------------------------------------------------------

{- | Like `System.Exit.exitWith`, but allows for `Word8`; lifts to `MonadIO`,
     and returns `Exited` -}
exitWith ∷ (MonadIO m, ToExitCode ξ) ⇒ ξ → m Exited
exitWith x = liftIO $ do
  _ ← System.Exit.exitWith (toExitCode x)
  return exited

exitWith' ∷ MonadIO μ ⇒ Word8 → μ Exited
exitWith' = exitWith

----------------------------------------

{- | Issue a dying wail, and exit with a given code -}

die ∷ (MonadIO μ, ToExitCode δ, Printable ρ) ⇒ δ → ρ → μ Exited
die ex msg = liftIO $ hPutStrLn stderr (toString msg) >> exitWith ex

{- | Issue an explanation before exiting, signalling a successful run but with
     abnormal results (e.g., a grep that found nothing). -}

dieAbnormal ∷ (MonadIO μ, Printable ρ) ⇒ ρ → μ Exited
dieAbnormal = die exitCodeAbnormal

{- | Issue an explanation before exiting, signalling a usage error. -}

dieUsage ∷ (MonadIO μ, Printable ρ) ⇒ ρ → μ Exited
dieUsage = die exitCodeUsage

{- | Issue an explanation before exiting, signalling an internal issue (e.g.,
     irrefutable pattern was refuted). -}

dieInternal ∷ (MonadIO μ, Printable ρ) ⇒ ρ → μ Exited
dieInternal = die exitCodeInternal

----------------------------------------

{- | Run a "main" function, which returns an exit code but may throw an
     `Exception`.  Any Exception is caught, displayed on stderr, and causes a
     general failure exit code.  Care is taken to not exit ghci if we are
     running there.
 -}
doMain ∷ (Printable ε, Exception ε, ToExitCode σ, MonadIO μ) ⇒
         ExceptT ε IO σ → μ ()
doMain io = liftIO $ do
  m ← ѥ io
  p ← getProgName
  let handler = if p ≡ "<interactive>"
                -- in ghci, always rethrow an exception (thus, we get an
                -- 'ExitSuccess' exception); ExitFailure 0 can never match
                then \ case ExitFailure 0 → return Exited; e → throwIO e
                -- in normal running, an ExitSuccess should just be a return
                else \ case ExitSuccess   → return Exited; e → throwIO e
  Exited ← handle handler $
    case m of
      Left  e → hPutStrLn stderr (toString e) >> exitInternal
      Right x → exitWith x
  return ()

doMain' ∷ ToExitCode σ ⇒ IO σ → IO ()
doMain' io = io >>= exitWith >> return ()

------------------------------------------------------------

{- | Exit code for successful termination. -}
exitCodeSuccess  ∷ Word8
exitCodeSuccess  = 0

{- | Exit after successful run. -}
exitSuccess ∷ MonadIO μ ⇒ μ Exited
exitSuccess = exitWith exitCodeSuccess

--------------------

{- | Exit code for abnormal termination (e.g., grep; everything worked, but
     nothing was found). -}
exitCodeAbnormal ∷ Word8
exitCodeAbnormal = 1

{- | Exit after successful but abnormal run (e.g., grep ran successfully, but
     found nothing). -}
exitAbnormal ∷ MonadIO μ ⇒ μ Exited
exitAbnormal = exitWith exitCodeAbnormal

--------------------

{- | Exit code for usage error (and calling @--help@). -}
exitCodeUsage    ∷ Word8
exitCodeUsage    = 2

{- | Exit after usage error (and calling @--help@). -}
exitUsage ∷ MonadIO μ ⇒ μ Exited
exitUsage = exitWith exitCodeUsage

--------------------

{- | Exit code for internal issue (e.g., irrefutable pattern was refuted). -}
exitCodeInternal ∷ Word8
exitCodeInternal = 255

{- | Exit after internal issue (e.g., irrefutable pattern was refuted). -}
exitInternal ∷ MonadIO μ ⇒ μ Exited
exitInternal = exitWith exitCodeInternal

-- that's all, folks! ----------------------------------------------------------
