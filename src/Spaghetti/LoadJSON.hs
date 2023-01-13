{-# LANGUAGE TupleSections #-}
module Spaghetti.LoadJSON where

import Data.Aeson
import Data.Aeson.KeyMap ( toMap, keys )
import Data.Aeson.Types ( parseEither, Parser )
import Data.Graph ( graphFromEdges, topSort, transposeG )
import Solitude
import Spaghetti.Types
import System.FilePath ( (</>) )
import qualified Data.Aeson.Key as Aeson
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Vector as V

directory :: String
directory = "data"

readJson :: 
  FromJSON a 
  => HasCallStack 
  => String 
  -> IO a
readJson fn = eitherDecodeFileStrict' (directory </> fn) >>= either (error . toText) pure

data RawSchema = RawSchema
  { recipesObject :: Object
  , itemsObject :: Object
  , fluidsObject :: Object
  , technologiesObject :: Object
  , machinesObject :: Object
  } deriving stock (Show, Generic)

data Schema = Schema
  { recipes :: Map Text (Recipe 'Truth)
  , items :: Map Text Item
  , fluids :: Map Text Fluid
  , technologies :: Map Text Technology
  , machines :: Map Text Machine
  , technologyByRecipe :: Map Text Technology
  } deriving stock (Show, Generic)

loadRaw :: IO RawSchema
loadRaw = do
  recipes <- readJson "recipe.json"
  items <- readJson "item.json"
  fluids <- readJson "fluid.json"
  technologies <- readJson "technology.json"
  machines <- readJson "assembling-machine.json"
  pure $ RawSchema recipes items fluids technologies machines

parseAllSchemas :: 
  RawSchema
  -> Either Text Schema
parseAllSchemas raw = do
  items <- parseItems raw
  fluids <- parseFluids raw
  (rawByTechs, rawTechs) <- parseRawTechnologies raw
  let (sortedVertices, lookupVertex, _) = 
        graphFromEdges (map (\v -> (v, v ^. #name, toList $ v ^. #rawPrerequisites)) rawTechs)
         & _1 %~ topSort . transposeG
  recipes <- parseRecipes items fluids (`Map.lookup` rawByTechs) raw
  (technologyByRecipe, technologies) <- parseTechnologies rawByTechs recipes $
    map (view _1 . lookupVertex) sortedVertices
  
  machines <- parseMachines raw
  pure $ Schema
    { items
    , fluids
    , technologies
    , recipes
    , machines
    , technologyByRecipe
    }

parseFromSchema :: 
  Object
  -> (Object -> Parser a)  
  -> Either Text (Map Text a)
parseFromSchema l f = Map.traverseWithKey 
  (\k v -> first toText $ parseEither (withObject (show k) f) v) 
  (Map.mapKeys Aeson.toText $ toMap l)

parseRecipes :: 
  Map Text Item
  -> Map Text Fluid 
  -> (Text -> Maybe Text)
  -> RawSchema 
  -> Either Text (Map Text (Recipe 'Truth))
parseRecipes items fluids byTech r = parseFromSchema (recipesObject r) $ \obj -> do
  name <- obj .: "name"
  category <- obj .: "category"
  time <- obj .: "energy"
  ingredients <- (obj .: "ingredients") >>= asArrayOrEmptyObject 
    (withObject "ingredient" $ parseIngredient items fluids)
  outputs <- (obj .: "products") >>= asArrayOrEmptyObject (withObject "output" $ parseOutput items fluids)
  mainOutput <- obj .:? "main_product" >>= maybe (pure Nothing) (fmap Just . parseOutput items fluids)
  isHidden <- obj .: "hidden"
  let tech = fromMaybe "" $ byTech name
  pure $ Recipe
    { name
    , category
    , time
    , ingredients
    , outputs
    , mainOutput
    , tech
    , isHidden
    }

parseIngredient :: 
  Map Text Item
  -> Map Text Fluid 
  -> Object 
  -> Parser (Ingredient 'Truth)
parseIngredient items fluids obj = do
  ty <- obj .: "type"
  amount <- obj .: "amount"
  name <- obj .: "name"
  case ty of
    "item" -> do
      item <- maybe (fail "failed to find output ") pure $ Map.lookup name items
      pure $ IIngredient ItemIngredient 
        { item = item
        , amount = amount
        }
    "fluid" -> do
      fluid <- maybe (fail "failed to find output ") pure $ Map.lookup name fluids
      fluidBoxIndex <- obj .:? "fluidbox_index"
      pure $ FIngredient FluidIngredient 
        { fluid = fluid
        , amount = amount
        , fluidBoxIndex = fluidBoxIndex
        }
    x -> error $ "found neither an item or a fluid: " <> x

parseAmount :: Object -> Parser (Amount 'Truth)
parseAmount obj = 
  (FixedAmount <$> obj .: "amount") 
  <|> (do
    amountMin <- obj .: "amount_min"
    amountMax <- obj .: "amount_max"
    pure $ VariableAmount (amountMin, amountMax)
    )

parseOutput ::
  Map Text Item
  -> Map Text Fluid 
  -> Object
  -> Parser (Output 'Truth)
parseOutput items fluids obj = do
  ty <- obj .: "type"
  probability <- obj .: "probability"
  amount <- parseAmount obj
  name <- obj .: "name"
  case ty of
    "item" -> do
      item <- maybe (fail "failed to find output ") pure $ Map.lookup name items
      pure $ IOutput ItemOutput 
        { item = item
        , amount = amount
        , probability = probability
        }
    "fluid" -> do
      fluid <- maybe (fail "failed to find output ") pure $ Map.lookup name fluids
      fluidBoxIndex <- obj .:? "fluidbox_index"
      pure $ FOutput FluidOutput 
        { fluid = fluid
        , amount = amount
        , probability = probability
        , fluidBoxIndex = fluidBoxIndex
        }
    x -> error $ "found neither an item or a fluid: " <> x

parseFluids :: 
  RawSchema 
  -> Either Text (Map Text Fluid)
parseFluids r = parseFromSchema (fluidsObject r) $ \obj -> do
  name <- obj .: "name"
  defaultTemperature <- obj .: "default_temperature"
  maxTemperature <- obj .: "max_temperature"
  fuelValue <- obj .: "fuel_value"
  pure $ Fluid
    { name
    , defaultTemperature
    , maxTemperature
    , fuelValue
    }

data RawTechnology = RawTechnology
  { name :: Text
  , rawUnlocks :: [Text]
  , rawPrerequisites :: [Text]
  }
  deriving stock (Eq, Show, Ord, Generic)

asArrayOrEmptyObject :: 
  (Value -> Parser a)
  -> Value
  -> Parser [a]
asArrayOrEmptyObject f (Array arr) = mapM f (V.toList arr)
asArrayOrEmptyObject _ (Object _) = pure []
asArrayOrEmptyObject _ x = fail $ "found a " <> show x <> "when parsing tech unlocks"

-- do a first pass so we can topologically sort it
parseRawTechnologies :: 
  RawSchema 
  -> Either Text (Map Text Text, [RawTechnology])
parseRawTechnologies r = do
  rawTechs <- fmap Map.elems $ parseFromSchema (technologiesObject r) $ \obj -> do
    name <- obj .: "name"
    rawUnlocks <- catMaybes <$> 
      ((obj .: "effects") >>= asArrayOrEmptyObject
          (withObject "unlock" $ \e -> do
            (ty :: String) <- e .: "type"
            case ty of
              "unlock-recipe" -> e .: "recipe"
              _ -> pure Nothing))
    rawPrerequisites <- obj .: "prerequisites" >>= asArrayOrEmptyObject parseJSON
    pure RawTechnology 
      { name
      , rawUnlocks
      , rawPrerequisites
      }
  let asUnlocks = Map.unions $ map (\RawTechnology{name, rawUnlocks} -> 
        Map.fromList $ map (,name) rawUnlocks) rawTechs
  pure (asUnlocks, rawTechs)

-- we assume the list is topologically sorted by prereqs..
-- and this gives us a mapping of recipe names to their unlocking techs
-- and vice versa
parseTechnologies :: 
  Map Text Text
  -> Map Text (Recipe 'Truth)
  -> [RawTechnology]
  -> Either Text (Map Text Technology, Map Text Technology)
parseTechnologies byRecipe recipes rawTechs = do
  let updateSet lookupMap from = S.fromList <$> mapM (\x -> case Map.lookup x lookupMap of
          Nothing -> Left $ "Could not find " <> x <> " in " <> show lookupMap
          Just r -> pure r) from
  techs <- foldlM (\m rawTech -> do
    -- because we have topologically sorted the techs on their prereqs
    -- we can claim that they will be in the map or we have a cycle of research
    prerequisites <- updateSet m (rawPrerequisites rawTech)
    pure $ Map.insert (rawTech ^. #name) (Technology 
      { name = rawTech ^. #name
      , prerequisites
      , unlocks = S.fromList $ mapMaybe (`Map.lookup` recipes) (rawUnlocks rawTech)
      }) m
    ) Map.empty rawTechs
  rs <- Map.fromList <$> mapM (\(recipeName, tech) -> do
          case Map.lookup tech techs of
                Nothing -> Left $ "Could not find " <> tech <> " when trying to find a recipe"
                Just r -> pure (recipeName, r)
          ) (Map.toList byRecipe)
  return (rs, techs)

parseMachines :: 
  RawSchema
  -> Either Text (Map Text Machine)
parseMachines r = parseFromSchema (machinesObject r) $ \obj -> do
  name <- obj .: "name"
  powerUsage <- obj .: "energy_usage"
  speed <- obj .: "crafting_speed"
  categories <- S.fromList <$> (obj .: "crafting_categories" >>= asArrayOrEmptyObject parseJSON)
  ingredientCount <- obj .: "ingredient_count"
  size <- obj .: "collision_box" >>= parseCollisionBox
  powerSource <- obj .: "energy_source" >>= parseEnergySource
  pure $ Machine
    { name
    , powerUsage
    , speed
    , categories
    , ingredientCount
    , size
    , powerSource
    }

parseEnergySource :: 
  Object 
  -> Parser (Maybe PowerSource)
parseEnergySource obj = 
  (Just . EPowerSource <$> (obj .: "electric" >>= parseElectric))
  <|> (Just . BPowerSource <$> (obj .: "burner" >>= parseBurner))
  <|> (Nothing <$ ((obj .: "void" ) :: Parser Object))
  <|> (Just . FPowerSource <$> (obj .: "fluid" >>= parseFluidEnergySource))
  <|> fail ("could not parse power source " <> show obj)

parseFluidEnergySource :: 
  Object 
  -> Parser FluidPowerSource
parseFluidEnergySource obj = do
  effectivity <- obj .: "effectivity"
  burnsFluid <- obj .: "burns_fluid"
  scaleFluidUsage <- obj .: "scale_fluid_usage"
  usagePerTick <- obj .: "fluid_usage_per_tick"
  maxTemperature <- obj .:? "maximum_temperature"
  fluidBox <- obj .: "fluid_box" >>= parseFluidBox
  pure $ FluidPowerSource
    { effectivity
    , burnsFluid
    , scaleFluidUsage
    , usagePerTick
    , maxTemperature
    , fluidBox
    }

parseFluidBox :: 
  Object 
  -> Parser FluidBox
parseFluidBox obj = do
  index <- obj .: "index"
  isInputOnly <- obj .: "production_type" >>= \case
    "input" -> pure True
    "input-output" -> pure False
    x -> error $ "unexpected production type " <> x
  baseArea <- obj .: "base_area"
  baseLevel <- obj .: "base_level"
  height <- obj .: "height"
  volume <- obj .: "volume"
  pure $ FluidBox 
    { index
    , isInputOnly
    , baseArea
    , baseLevel
    , height
    , volume
    }

parseElectric :: 
  Object 
  -> Parser ElectricPowerSource
parseElectric obj = ElectricPowerSource <$> obj .: "drain"

parseBurner :: Object -> Parser BurnerPowerSource
parseBurner obj = do
  effectivity <- obj .: "effectivity"
  fuelCategories <- S.fromList <$> 
    (obj .: "fuel_categories" >>= withObject "fuel categories" (pure . map Aeson.toText . keys))
  pure $ BurnerPowerSource { effectivity, fuelCategories }

data Coord = Coord { x :: Double, y :: Double } 
  deriving stock ( Generic )
  deriving anyclass ( FromJSON )

parseCollisionBox :: 
  Object 
  -> Parser (Int, Int)
parseCollisionBox obj = do
  l <- obj .: "left_top"
  r <- obj .: "right_bottom"
  pure (ceiling $ x r - x l, ceiling $ y r - y l)

parseItems :: 
  RawSchema 
  -> Either Text (Map Text Item)
parseItems r = parseFromSchema (itemsObject r) (\obj -> do
  name <- obj .: "name"
  fuelValue <- obj .: "fuel_value"
  stackSize <- obj .: "stack_size"
  placeableInWorld <- isJust @String <$> obj .:? "place_result"
  tier <- obj .:? "tier"
  pure $ Item 
    { name
    , fuelValue
    , stackSize
    , placeableInWorld
    , tier
    }) 