module Spaghetti.GUI.Window
  ( GuiWindow(..)
  , Renderable(..)
  , RenderState
  , gridToAbsolute
  , HasGrid(..)
  , GridCoord(..)
  , LayoutGrid(..)
  ) where

import DearImGui
import Relude
import Spaghetti.GUI.SDL
import UnliftIO (MonadUnliftIO)
import Linear
import qualified Data.StateVar as SV

type RenderState s m = (MonadState s m, MonadReader (IORef s) m, MonadUnliftIO m)

class Renderable widget s | widget -> s where
  renderWidget :: RenderState s m => widget -> m ()

class HasGrid s where
  getGrid :: s -> LayoutGrid

newtype GridCoord = GridCoord { toInt :: Int }
  deriving newtype ( Show, Eq, Num, Real, Ord, Enum, Integral )
  deriving stock ( Generic )

newtype LayoutGrid = LayoutGrid { unGrid :: V2 Int }
  deriving newtype ( Show, Eq, Num )
  deriving stock ( Generic )

data GuiWindow s a = GuiWindow 
  { title :: Text
  , isMovable :: Bool
  , isResizable :: Bool
  , initialSize :: V2 a
  , initialPosition :: V2 a
  , renderComponents :: forall m. RenderState s m => m ()
  }

gridToAbsolute ::
  LayoutGrid
  -> V2 Int
  -> V2 GridCoord
  -> ImVec2
gridToAbsolute (LayoutGrid (V2 gx gy)) (V2 wx wy) (V2 lx ly) = 
  let scale g w l = (fromIntegral l / fromIntegral g) * fromIntegral w 
  in ImVec2 (scale gx wx lx) (scale gy wy ly)

instance (HasGrid s, HasSDLWindow s) => Renderable (GuiWindow s GridCoord) s where
  renderWidget w = do
    s <- get
    let grid = getGrid s
    size <- liftIO $ windowSize s
    let absCoords = gridToAbsolute grid size (initialPosition w)
        absSize = gridToAbsolute grid size (initialSize w)
    renderWindowAt absCoords absSize w


renderWindowAt ::
  RenderState s m
  => ImVec2 
  -> ImVec2 
  -> GuiWindow s a
  -> m ()
renderWindowAt pos size w = do
  setNextWindowPos (SV.makeGettableStateVar (pure pos))
      (if isMovable w then ImGuiCond_FirstUseEver else ImGuiCond_Always) Nothing
  setNextWindowSize (SV.makeGettableStateVar (pure size))
      (if isResizable w then ImGuiCond_FirstUseEver else ImGuiCond_Always)
  withWindowOpen (title w <> "##window") $ do
    renderComponents w