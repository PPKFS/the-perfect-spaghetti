module Spaghetti.GUI.ListBox
  ( GuiListBox(..)
  , ListBoxSelection(..)
  ) where

import Relude
import DearImGui
import Spaghetti.GUI.Window
import qualified Data.StateVar as SV
import Data.List ((!!))
import Optics

newtype ListBoxSelection = ListBoxSelection { selectToInt :: Int } 
  deriving stock ( Show, Generic )
  deriving newtype ( Num, Eq, Ord )

data GuiListBox s = GuiListBox 
  { label :: Text
  , listContents :: [Text]
  , selectionL :: Lens' s ListBoxSelection
  , listHeight :: Int
  , onUpdate :: forall m. RenderState s m => Text -> ListBoxSelection -> m ()
  }

instance Renderable (GuiListBox s) s where
  renderWidget lb = do
    i <- ask
    setNextItemWidth (-1)
    changed <- listBox
      (label lb <> "##listbox")
      (stateVarFromLens i $ selectionL lb % coerced) 
      (listContents lb)
      (listHeight lb)
    when changed $ do
      s <- get
      let index = s ^. selectionL lb
          e = listContents lb !! coerce index
      onUpdate lb e index

stateVarFromLens :: IORef s -> Lens' s a -> SV.StateVar a
stateVarFromLens r l = SV.makeStateVar (view l <$> readIORef r) (modifyIORef r . set l)
