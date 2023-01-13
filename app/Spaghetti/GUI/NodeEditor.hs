{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Spaghetti.GUI.NodeEditor where

import Relude
import Spaghetti.GUI.Window
import qualified DearImGui.Raw as Raw
import DearImGui
import DearImGui.NodeEditor
import Optics

class HasEditorContext s where
  getEditorContext :: s -> EditorContext'

class RenderNode n s | n -> s where
  nodeL :: Lens' n GraphNode
  renderHeader :: RenderState s m => n -> m ()
  renderBody :: RenderState s m => n -> m ()
  postRenderHeader :: RenderState s m => n -> ImVec2 -> ImVec2 -> m ()
  postRenderBody :: RenderState s m => n -> ImVec2 -> ImVec2 -> m ()
  default postRenderHeader :: RenderState s m => n -> ImVec2 -> ImVec2 -> m ()
  postRenderHeader _ _ _ = pass
  default postRenderBody :: RenderState s m => n -> ImVec2 -> ImVec2 -> m ()
  postRenderBody _ _ _ = pass

class RenderPin p where
  pinL :: Lens' p NodePin
  renderPin :: RenderState s m => p -> m ()

class RenderLink l s | l -> s where
  linkL :: Lens' l (GraphLink s)
  renderLink :: RenderState s m => l -> m ()

instance RenderLink (Text, GraphLink s) s where
  linkL = _2
  renderLink _ = pass
data GraphNode = GraphNode
  { nodeId :: Int
  , nodeTitle :: Text
  } deriving stock ( Generic )

data GraphLink s = GraphLink
  { linkId :: Int
  , fromId :: Int
  , toId :: Int
  } deriving stock ( Generic )

data NodePin = NodePin
  { pinId :: Int
  , name :: Text
  , connection :: Maybe Int
  } deriving stock ( Generic, Show )

zipWithTails :: (a -> c) -> (b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithTails l r f (a:as) (b:bs) = f a b : zipWithTails l r f as bs
zipWithTails _ r _ [] bs = fmap r bs
zipWithTails l _ _ as _ = fmap l as

data NodeEditor node link s = NodeEditor
  { nodes :: [node]
  , links :: [link]
  }

instance (RenderNode node s, RenderLink link s, HasEditorContext s) => Renderable (NodeEditor node link s) s where
  renderWidget ne = do
    s <- get
    newSize <- getContentRegionAvail
    withNodeEditor (getEditorContext s) "node editor" newSize $ do
      -- make all our nodes
      forM_ (nodes ne) $ \node -> do
        withNode (nodeId (view nodeL node)) $ withVertical "node" $ do
          withHorizontal "header" (renderHeader node)
          hmin <- Raw.getItemRectMin
          hmax <- Raw.getItemRectMax
          spring 1
          withHorizontal "body" (renderBody node)
          bmin <- Raw.getItemRectMin
          bmax <- Raw.getItemRectMax
          postRenderHeader node hmin hmax
          postRenderBody node bmin bmax
      forM_ (links ne) $ \link -> do
        linkNodes (view (linkL % #linkId) link) (view (linkL % #fromId) link) (view (linkL % #toId) link)
