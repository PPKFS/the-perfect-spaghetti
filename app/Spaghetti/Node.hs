module Spaghetti.Node where

import Relude
import Spaghetti.GUI.NodeEditor
import Spaghetti.Types
import DearImGui
import Optics
import qualified Data.StateVar as SV
import DearImGui.NodeEditor

data GuiNode a s = GuiNode
  { graphNode :: GraphNode
  , nodeDetails :: a
  } deriving stock ( Generic )

data NodeDetails = RNode RecipeNode | INode InputNode | ONode OutputNode-- | PNode PowerNode 
  deriving stock (Show)

data RecipeNode = RecipeNode 
  { inputPins :: [NodePin]
  , outputPins :: [NodePin]
  --, fuelPin :: Maybe Pin
  , recipe :: Recipe 'Truth
  } deriving stock ( Show )

newtype ConstantNode = ConstantNode Text
newtype InputNode = InputNode [NodePin] deriving stock (Show)
newtype OutputNode = OutputNode [NodePin] deriving stock (Show)

class HasIOs i where
  getInputs :: i -> [NodePin]
  getOutputs :: i -> [NodePin]
  
instance HasIOs NodeDetails where
  getInputs (RNode rn) = inputPins rn
  getInputs (ONode (OutputNode n)) = n
  getInputs _ = []
  getOutputs (RNode rn) = outputPins rn
  getOutputs (INode (InputNode n)) = n
  getOutputs _ = []
instance HasIOs a => RenderNode (GuiNode a s) s where
  nodeL = #graphNode
  renderHeader gn = text (gn & view (#graphNode % #nodeTitle))
  renderBody (GuiNode _ dets) = do
    withVertical "inputs" $ do
      let is = getInputs dets
      forM_ is \(NodePin i n _) -> do
          withPin i True $ do
            text n
    withVertical "Middle" $ dummy (SV.makeGettableStateVar (pure (ImVec2 30 0)))
    withVertical "outputs" $ do
      let os = getOutputs dets
      forM_ os \(NodePin i n _) -> do
        withPin i False $ do
          text n