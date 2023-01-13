{-# LANGUAGE TupleSections #-}
module Main ( main ) where

import Relude

import Spaghetti.LoadJSON
import Spaghetti.Planner
import Control.Monad.Managed
import DearImGui
import qualified SDL
import qualified Data.Map as Map
import Spaghetti.GUI.SDL
import Spaghetti.Node
import Spaghetti.GUI.Window
import UnliftIO ( MonadUnliftIO (..) )
import Spaghetti.GUI.ListBox
import Spaghetti.Types
import Optics.State.Operators
import DearImGui.NodeEditor
import Data.Bits ((.|.), Bits (..))
import qualified Data.Bits as A
import Linear
import Optics
import Spaghetti.GUI.NodeEditor
import Data.Graph.Inductive hiding (Node)
import qualified Data.Graph.Inductive as Graph
import Data.List (elemIndex)
import qualified Data.Graph.Inductive as G

main :: IO ()
main = runManaged $ do
  window <- initialiseWindow "The Perfect Spaghetti" (V2 1920 1080)
  edCxt <- createEditorContext
  r <- liftIO loadRaw
  case parseAllSchemas r of
    Left err -> error err
    Right sc -> do

      s <- newIORef $ GS window (LayoutGrid (V2 100 100)) 0 edCxt (fromProdChain (fakeProdChain sc)) sc (WorkState Nothing) (UIComponents 0)
      liftIO $ prettyPrint (production $ fakeProdChain sc)
      void $ liftIO $ flip runReaderT s $ runIOReaderT mainLoop

fromProdChain :: 
  ProductionChain (Recipe 'Truth) 'Truth
  -> NodeEditor (GuiNode NodeDetails GlobalState) (Text, GraphLink GlobalState) GlobalState
fromProdChain ProductionChain{production, nodeMap} = NodeEditor (map (makeNode . context production) $ G.nodes production) (flip evalState 
    ( map edgeLabel $ out production ((snd (nodeRange production))-1)
    , map edgeLabel $ inn production ((snd (nodeRange production))), 1000000) $ mapM (makeLink production) $ labEdges production)

makeLink :: MonadState ([Text], [Text], Int) m => Gr (ProductionChainNode (Recipe 'Truth)) Text -> LEdge Text -> m (Text, GraphLink GlobalState)
makeLink gr (from, to', n) = do
  (i, o, s) <- get
  put (i, o, (s+1))
  traceShow ("the link is from " <> (show $ (100000*from) + 1 + (getOutputNumber n (i, o) $ lab gr from)) <> " to " <> (show $ (1000*to') + 1 + (getInputNumber n (i, o) $ lab gr to')) <> "for the link " <> show n) pass
  pure (n, GraphLink
    { linkId = s+1
    , fromId = (100000*from) + 1 + (getOutputNumber n (i, o) $ lab gr from)
    , toId  = (1000*to') + 1 + (getInputNumber n (i, o) $ lab gr to')
    } )

getOutputNumber :: Text -> ([Text], [Text]) -> Maybe (ProductionChainNode (Recipe 'Truth)) -> Int
getOutputNumber _ _ Nothing = error "oop"
getOutputNumber t (i, _) (Just InputPNode) = fromMaybe (error "e") $ elemIndex t i
getOutputNumber t (_, o) (Just OutputPNode) = fromMaybe (error "e") $ elemIndex t o
getOutputNumber t _ (Just (RecipePNode rcp)) = let idx = elemIndex t $ map (\o -> view #name o) $ rcp ^. #outputs in traceShow ("the output index of " <> t <> " in the recipe " <> (rcp ^. #name) <> " is " <> show idx) $ fromMaybe (error "ooo") $ idx

getInputNumber :: Text -> ([Text], [Text]) -> Maybe (ProductionChainNode (Recipe 'Truth)) -> Int
getInputNumber _ _ Nothing = error "oop"
getInputNumber t (i, _) (Just InputPNode) = fromMaybe (error "e") $ elemIndex t i
getInputNumber t (_, o) (Just OutputPNode) = fromMaybe (error "e") $ elemIndex t o
getInputNumber t _ (Just (RecipePNode rcp)) = fromMaybe (error "ooo") $ elemIndex t $ map (\o -> view #name o) $ rcp ^. #ingredients

makeNode :: Data.Graph.Inductive.Context (ProductionChainNode (Recipe 'Truth)) Text -> GuiNode NodeDetails GlobalState
makeNode (_inList, nodeId, InputPNode, outList) = 
  let n = GuiNode (GraphNode nodeId "Input") (INode $ InputNode $ evalState (mapM makePin outList) (100000*nodeId))
  in traceShow (mconcat ["Made an input node with ID", show nodeId, " with pins", show (nodeDetails n), show outList]) n
makeNode (inList, nodeId, OutputPNode, _outList) = 
  let n = GuiNode (GraphNode nodeId "Output") (ONode $ OutputNode $ evalState (mapM makePin inList) (1000*nodeId))
  in traceShow (mconcat ["Made an output node with ID", show nodeId, " with pins", show (nodeDetails n)]) n
makeNode (inList, nodeId, RecipePNode recipe, outList) = GuiNode (GraphNode nodeId (recipe ^. #name)) (
  let i = flip evalState (1000*nodeId) $ do
        mapM makePin inList
      
      o = flip evalState (100000*nodeId) $ 
        mapM makePin outList
  in
  traceShow (mconcat ["Made a recipe node, ", (recipe ^. #name), " with ID", show nodeId, " with input pins", show i, " and output pins", show o]) $
    RNode $ RecipeNode i o recipe)

makePin :: MonadState Int m => (Text, Graph.Node) -> m NodePin
makePin (name, l) = do
  s <- get
  put (s+1)
  traceShow (mconcat ["Made a pin with name ", name, "and id ", (show (s+1))]) pass
  pure (NodePin (s+1) name (Just l))

newtype IOReaderT s m a = IOReaderT { runIOReaderT :: ReaderT (IORef s) m a } 
  deriving newtype (Functor, Applicative, Monad, MonadReader (IORef s), MonadIO, MonadUnliftIO, MonadTrans)

instance MonadIO m => MonadState s (IOReaderT s m) where 
  state f = IOReaderT $ do
    ior <- ask
    s <- liftIO (readIORef ior)
    let (a, r) = f s
    writeIORef ior r
    pure a

data WorkState = WorkState
  { selectedRecipe :: Maybe (Recipe 'Truth)
  } deriving stock ( Generic )

data GlobalState = GS
  { window :: SDL.Window
  , grid :: LayoutGrid
  , nodeIds :: Int
  , editorContext :: EditorContext'
  , prodGraphRender :: NodeEditor (GuiNode NodeDetails GlobalState) (Text, GraphLink GlobalState) GlobalState
  , schema :: Schema
  , workState :: WorkState
  , uiComponents :: UIComponents
  } deriving stock ( Generic )

instance HasGrid GlobalState where
  getGrid = view #grid
instance HasSDLWindow GlobalState where
  windowL = #window

instance HasEditorContext GlobalState where
  getEditorContext = view #editorContext
data UIComponents = UIComponents
  { recipeListSelection :: ListBoxSelection
  } deriving stock ( Generic )

recipeListBox :: Schema -> GuiListBox GlobalState
recipeListBox s = GuiListBox
  { label = "##recipes"
  , listContents = Map.keys (s ^. #recipes)
  , selectionL = #uiComponents % #recipeListSelection
  , onUpdate = \listElemName -> const $ do
      --traceShow (listElemName, Map.lookup listElemName (s ^. #recipes)) pass
      #workState % #selectedRecipe .= Map.lookup listElemName (s ^. #recipes)
      c <- use $ #workState % #selectedRecipe
      print c
  , listHeight = 20
  }

anotherWindow :: GuiWindow GlobalState GridCoord
anotherWindow = GuiWindow
  { title = "Other Stuff"
  , initialSize = fmap GridCoord (V2 25 75)
  , isMovable = False
  , isResizable = False
  , initialPosition = fmap GridCoord (V2 75 0)
  , renderComponents = pass
  }

nodeEd :: ProductionChain (Recipe 'Truth) 'Truth -> NodeEditor GlobalState (Recipe 'Truth) Text
nodeEd jRcp = NodeEditor
  { nodes = []
  , links = []
  }

aFourthWindow :: GuiWindow GlobalState GridCoord
aFourthWindow = GuiWindow
  { title = "Production Chain"
  , initialSize = fmap GridCoord (V2 100 100)--(V2 50 75)
  , isMovable = False
  , isResizable = False
  , initialPosition = fmap GridCoord (V2 0 0)--(V2 25 0)
  , renderComponents = do
      s <- get
      --rcp <- use $ #workState % #selectedRecipe
      --whenNothing rcp $ do
      renderWidget (s ^. #prodGraphRender)
        --renderWidget (nodeEd jRcp)
        {- winSize <- liftIO $ windowSize s
        newSize <- getContentRegionAvail
        withNodeEditor (editorContext s) "node editor" newSize $ do
          let perRow = zip [1000..] $ zipWithTails 
                (\x -> (Just $ getInputName x,Nothing)) 
                (\x -> (Nothing, Just $ getOutputName x)) 
                (\x y -> (Just $ getInputName x, Just $ getOutputName y)) 
                (jRcp ^. #ingredients) 
                (jRcp ^. #outputs)
          (hmin, hmax) <- withNode 1 $ do
            beginVertical "node"
            beginHorizontal "header"
            spring 1
            text (jRcp ^. #name)
            spring 1
            dummy (SV.makeGettableStateVar (pure $ ImVec2 0 28))
            --separator
            endHorizontal
            hmin <- Raw.getItemRectMin
            hmax <- Raw.getItemRectMax
            beginHorizontal "content"
            beginVertical "inputs"
            forM_ perRow \(i, (l, r)) -> do
              whenJust l $ \lname -> do
                withPin i True $ do
                  text lname
            endVertical
            spring 1
            {-beginVertical "Middle"
            dummy (SV.makeGettableStateVar (pure (ImVec2 30 0)))
            endVertical-}
            beginVertical "outputs"
            forM_ perRow \(i, (l, r)) -> do
              whenJust r $ \rname -> do
                withPin (2*i) False $ do
                  text rname
            endVertical
            endHorizontal
            endVertical
            
            pure $ (hmin, hmax)
          
          forM_ perRow \(i, (l, r)) -> do
            whenJust l $ \lname -> do
              withNode (10*i) $ do
                text lname
                withPin (11*i) True $ do
                  text lname
              linkNodes (15*i) (11*i) (i)
            whenJust r $ \rname -> do
              withNode (12*i) $ do
                text rname
                withPin (13*i) False $ do
                  text rname
              linkNodes (14*i) (13*i) (2*i)
          dl <- getNodeBackgroundDrawList 1
          
          liftIO $ (M.with hmin) $ \minptr -> (M.with hmax) $ \maxptr -> 
            Raw.addRectFilled dl minptr maxptr (imCol32 200 100 100 200) 0.2 ImDrawFlags_None-}
  }

fakeProdChain :: Schema -> ProductionChain (Recipe 'Truth) 'Truth
fakeProdChain gs = case runExceptT (flip runReaderT gs $ makeUnitlessProductionChain [
  "electronic-circuit"
  , "battery-mk00"
  , "capacitor1"
  , "inductor1"
  , "resistor1"
  , "pcb1"
  , "vacuum-tube"
  , "solder"
  , "ceramic"
  , "coal-gas"
  ]) of
  Left err -> error err
  Right (Left err) -> error err
  Right (Right res) -> res

colourFromChannels :: Word8 -> Word8 -> Word8 -> Word8 -> ImU32
colourFromChannels r g b a = fromIntegral $ r `A.shiftL` 24
    .|. g `A.shiftL` 16
    .|. b `A.shiftL` 8
    .|. a

vecSub :: ImVec2 -> ImVec2 -> ImVec2
vecSub (ImVec2 x1 y1) (ImVec2 x2 y2) = ImVec2 (x1+x2) (y1+y2)
getInputName :: Ingredient c -> Text
getInputName (IIngredient i) = i ^. #item % #name
getInputName (FIngredient i) = i ^. #fluid % #name

getOutputName :: Output c -> Text
getOutputName (IOutput i) = i ^. #item % #name
getOutputName (FOutput i) = i ^. #fluid % #name

infoWindow :: GuiWindow GlobalState GridCoord
infoWindow = GuiWindow
  { title = "Recipe Info"
  , initialSize = fmap GridCoord (V2 75 25)
  , isMovable = False
  , isResizable = False
  , initialPosition = fmap GridCoord (V2 25 75)
  , renderComponents = do
      s <- use (#workState % #selectedRecipe)
      whenJust s $ \sel -> do
        text (sel ^. #name <> "Crafting Time: " <> (show $ sel ^. #time) <> "s")
        text (mconcat $ map show $ sel ^. #ingredients)
        text (mconcat $ map show $ sel ^. #outputs)
        let countWidget l = text $ "[" <> show (length l) <> "]"
        pass
  }

recipeWindow :: GuiWindow GlobalState GridCoord
recipeWindow = GuiWindow
  { title = "Recipes"
  , initialSize = fmap GridCoord (V2 25 100)
  , isMovable = False
  , isResizable = False
  , initialPosition = fmap GridCoord (V2 0 0)
  , renderComponents = do
      s <- get
      renderWidget (recipeListBox (s ^. #schema))
      --listBox "##recipe box" (makeGetter (pure 1) :: IO Int) (Map.keys (s ^. #schema % #recipes) )
      -- Add a button widget, and call 'putStrLn' when it's clicked
      button "Clickety Click" >>= \case
        False -> return ()
        True  -> do
          liftIO $ prettyPrint (view #production $ fakeProdChain (s ^. #schema))
  }

mainLoop :: (MonadState GlobalState m, MonadReader (IORef GlobalState) m, MonadUnliftIO m) => m ()
mainLoop = unlessQuit $ do
  liftIO beginFrame
  mapM_ renderWidget [recipeWindow, infoWindow, anotherWindow, aFourthWindow]
  w <- gets window
  showDemoWindow
  liftIO $ renderFrame w

  mainLoop
