{-# LANGUAGE UndecidableInstances #-}
module Spaghetti.Planner where

import Solitude

import Control.Monad.Except
import Data.Graph.Inductive (Graph(..))
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Spaghetti.LoadJSON
import Spaghetti.Types
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.List (nub)
import Control.Monad.Writer.Strict
import qualified Data.Graph.Inductive as Gr

type SpatialArrangement = ()

data MachineConfig c = MachineConfig
  { recipe :: Recipe 'Truth
  , machine :: Machine
  , quantity :: Unit c
  }

deriving stock instance Eq (Unit c) => Eq (MachineConfig c)
deriving stock instance Show (Unit c) => Show (MachineConfig c)
deriving stock instance Ord (Unit c) => Ord (MachineConfig c)

data ProductionChain elem (c :: ChainType) = ProductionChain
  { inputs :: Set (Ingredient c)
  , outputs :: Set (Output c)
  , byproducts :: Set (Ingredient c)
  , production :: Gr (ProductionChainNode elem) (ProductionChainLink elem)
  , nodeMap :: ProductionNodeMap elem 
  , spatialArrangement :: SpatialArrangement -- a realisation of the recipe in terms of its layout
  } deriving stock ( Generic )

data GraphElement elem = GNode (ProductionChainNode elem) | GLink (ProductionChainLink elem) | GPin (ProductionChainPin elem)
data ProductionChainLink elem 
data ProductionNodeMap elem = ProductionNodeMap 
  { nodeMap :: Map Gr.Node (Either (ProductionChainNode elem) (ProductionChainLink elem))
  , nodeId :: Gr.Node
  }

type ProductionNodeMapM elem m = MonadState (ProductionNodeMap elem) m

scaleRecipePerSecond ::
  Recipe 'Actual
  -> Recipe 'Ideal
scaleRecipePerSecond recipe = 
  let currentTime = recipe ^. #time
      scaleFactor = (1 / currentTime)
  in scaleToIdeal scaleFactor recipe

data ProductionChainNode elem = 
  InputPNode 
  | OutputPNode 
  | RecipePNode elem
  deriving stock ( Eq, Ord, Show, Generic )

simpleLog ::
  MonadWriter c m
  => [c]
  -> m ()
simpleLog = tell . mconcat

makeUnitlessProductionChain ::
  MonadReader Schema m
  => MonadError Text m
  => MonadWriter Text m
  => NodeMapM (ProductionChainNode (Recipe 'Truth)) Gr 
  => [Text]
  -> m (ProductionChain (Recipe 'Truth) 'Truth)
makeUnitlessProductionChain recipes = do
  s <- ask
  actualRecipes <- forM ((nub . sort) recipes) $ \r ->
    lookupOrThrow r (s ^. #recipes)
  simpleLog ["We found ", show $ length actualRecipes, " recipes."]
  -- since our display cannot deal with 0-indexed nodes, we bump the map by a dummy node.
  let nodeMap0 = snd $ mkNode new (RecipePNode (Recipe @'Truth "dummy" [] [] Nothing 0 "" True ""))
  -- it's more convenient to do this in parts; first we want to make the default recipe nodes
      (recipeNodes, nodeMap1) = mkNodes nodeMap0 (map RecipePNode actualRecipes)
  simpleLog ["Made ", show $ length recipeNodes, " recipe nodes with ID range" ]
  -- then for each recipe, we can make nodes for its inputs and outputs
  let (pinNodes, nodeMap2) = mkNodesM a b
  -- and finally, we want to make nodes for each link
  -- yes, this is an abuse of the nodemap, but it makes it easier later to render it as we already have edge ids
  let (inputMap, outputMap) = bimap (Map.map S.toList) (Map.map S.toList) $ foldl' (\(im, om) r ->
        let updSet :: LabelOptic' "name" A_Lens i Text 
              => Map.Map Text  (S.Set (Recipe 'Truth)) 
              -> [i] 
              -> Map.Map Text (S.Set (Recipe 'Truth))
            updSet mapUpd l =  Map.unions $ map (\i -> mapUpd & at (i ^. #name) % non S.empty %~ S.insert r) l
          in (updSet im (r ^. #ingredients), updSet om (r ^. #outputs))) (Map.empty, Map.empty) actualRecipes
      genericInputNode = InputPNode
      genericOutputNode = OutputPNode
      (nodes', nodeMap') = mkNodes nodeMap [genericInputNode, genericOutputNode]
      edgesFromIn = fromMaybe (error "failed to make edges") $ mkEdges nodeMap' (Map.foldlWithKey' (\l k v -> 
        let possiblyFrom = case outputMap ^. at k of
              Nothing -> [genericInputNode]
              Just l' -> map RecipePNode l'
            possiblyTo = map RecipePNode v
        in
        [ (possOutput, possInput, k)
        | possOutput <- possiblyFrom
        , possInput <- possiblyTo
        ] <> l) [] inputMap)
      edgesFromOut = fromMaybe (error "failed to make edges out") $ mkEdges nodeMap' (Map.foldlWithKey' (\l k v -> 
        let possiblyTo = case inputMap ^. at k of
              Nothing -> [genericOutputNode]
              -- we want to entertain the possibility of an overflow
              -- ...but not right now
              Just l' -> {- [genericOutputNode] <> -}map RecipePNode l'
            possiblyFrom = map RecipePNode v
        in
        [ (possOutput, possInput, k)
        | possOutput <- possiblyFrom
        , possInput <- possiblyTo
        ] <> l) [] outputMap)
  pure $ ProductionChain 
    (S.fromList $ mconcat $ map (view #ingredients) $ actualRecipes) 
    (S.fromList $ mconcat $ map (view #outputs) $ actualRecipes)
    S.empty
    (mkGraph (nodes <> nodes') $ S.toList $ S.fromList (edgesFromIn <> edgesFromOut))
    nodeMap'
    ()

lookupOrThrow :: 
  MonadError c m
  => IsString c
  => Semigroup c
  => Show a
  => Ord a
  => a 
  -> Map a b 
  -> m b
lookupOrThrow k = maybe 
  (throwError $ "Could not find the element " <> show k) 
  pure . Map.lookup k



{-
runMachine :: Machine -> Recipe -> (Name, Int) -> Either SomeError ProductionUnit
runMachine mIn rIn (outputName, outputQuantity) = do
  {- verify - that the machine can run the recipe
  verify - we do actually produce the desired output with this recipe
  scale the recipe and produce the unit
  -}
  -- we make this many per second
  let rateOfOutput = 
          (fromJust $ rIn ^? #outputs % at outputName % _Just % #quantity) 
          / (rIn ^. #speed)
      oFrac = fromIntegral outputQuantity 
      scaledRecipe = scaleRecipe rIn (oFrac / rateOfOutput)
  pure $ ProductionUnit 
    { inputs = scaledRecipe ^. #inputs
    , outputs = scaledRecipe ^. #outputs
    , byproducts = Map.empty
    , production = Left $ MachineConfig rIn mIn (oFrac / rateOfOutput) (ceiling (oFrac / rateOfOutput)) []
    , spatialArrangement = ()
    }

scaleRecipe :: Recipe -> Double -> Recipe
scaleRecipe r ti = r & 
  #inputs %~ (fmap (scaleIngredient ti)) &
  #outputs %~ (fmap (scaleIngredient ti))

scaleIngredient :: Double -> Ingredient -> Ingredient
scaleIngredient by i = i & #quantity %~ (by*) 
-}