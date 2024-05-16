{-# LANGUAGE DataKinds #-}
module Agda.LSP.Utils where

import Control.Monad

import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Aeson
import Data.Proxy

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

import Agda.LSP.Monad.Base

-- | Send a basic workspace edit request to the client.
sendWorkspaceEdit :: MonadLsp a m => Text.Text -> Map.Map Uri [TextEdit] -> m ()
sendWorkspaceEdit title changes =
  let
    edit = ApplyWorkspaceEditParams (Just title) $ WorkspaceEdit
      { _changes = Just changes
      , _documentChanges = Nothing
      , _changeAnnotations = Nothing
      }
  in void $ sendRequest SMethod_WorkspaceApplyEdit edit (const (pure ()))

-- | Show a message on the client side.
showWorkspaceMessage :: MonadLsp a m => MessageType -> Text.Text -> m ()
showWorkspaceMessage kind msg = sendNotification SMethod_WindowShowMessage $ ShowMessageParams kind msg
