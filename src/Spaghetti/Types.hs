{-# LANGUAGE UndecidableInstances #-}
module Spaghetti.Types where

import Solitude
import GHC.Show (Show(..))

data ChainType = 
  -- a recipe which is scaled to optimal (but likely not realisable) amounts
  Ideal 
  -- a recipe scaled to an integer number of machines
  | Actual 
  -- the actual recipe as it exists in factorio
  | Truth

type family Unit (c :: ChainType) where
  Unit 'Ideal = Double
  Unit 'Actual = Int
  Unit 'Truth = Int

class UFunctor (i :: ChainType -> Type) where
  umap :: (Unit c1 -> Unit c2) -> i c1 -> i c2

scaleToIdeal :: 
  UFunctor i 
  => Double
  -> i 'Actual 
  -> i 'Ideal
scaleToIdeal scaleFactor = umap ((scaleFactor *) . fromIntegral)

toIdeal :: 
  UFunctor i 
  => i 'Actual 
  -> i 'Ideal
toIdeal = scaleToIdeal 1

data Ingredient (c :: ChainType) =
  IIngredient (ItemIngredient c)  
  | FIngredient (FluidIngredient c)
  deriving stock ( Generic )

deriving stock instance Eq (Unit c) => Eq (Ingredient c)
deriving stock instance Show (Unit c) => Show (Ingredient c)
deriving stock instance Ord (Unit c) => Ord (Ingredient c)

instance UFunctor Ingredient where
  umap f (IIngredient i) = IIngredient (umap f i)
  umap f (FIngredient i) = FIngredient (umap f i)

instance LabelOptic "name" A_Lens (Ingredient c) (Ingredient c) Text Text where
  labelOptic = lens (\case 
    IIngredient i -> view #name i
    FIngredient i -> view #name i)
    (\f n -> case f of
      IIngredient i -> IIngredient (set #name n i)
      FIngredient i -> FIngredient (set #name n i))

instance LabelOptic "name" A_Lens (ItemIngredient c) (ItemIngredient c) Text Text where
  labelOptic = #item % #name

instance LabelOptic "name" A_Lens (FluidIngredient c) (FluidIngredient c) Text Text where
  labelOptic = #fluid % #name

data ItemIngredient (c :: ChainType) = ItemIngredient
  { item :: Item
  , amount :: Unit c
  }
  deriving stock ( Generic )

deriving stock instance Eq (Unit c) => Eq (ItemIngredient c)
deriving stock instance Show (Unit c) => Show (ItemIngredient c)
deriving stock instance Ord (Unit c) => Ord (ItemIngredient c)

instance UFunctor ItemIngredient where
  umap f i = ItemIngredient 
    { item = i ^. #item
    , amount = f (i ^. #amount)
    }

data FluidIngredient (c :: ChainType) = FluidIngredient
  { fluid :: Fluid
  , amount :: Unit c
  , fluidBoxIndex :: Maybe Int
  }
  deriving stock ( Generic )

deriving stock instance Eq (Unit c) => Eq (FluidIngredient c)
deriving stock instance Show (Unit c) => Show (FluidIngredient c)
deriving stock instance Ord (Unit c) => Ord (FluidIngredient c)

instance UFunctor FluidIngredient where
  umap f i = FluidIngredient 
    { fluid = i ^. #fluid
    , amount = f (i ^. #amount)
    , fluidBoxIndex = i ^. #fluidBoxIndex
    }

data Output (c :: ChainType) = IOutput (ItemOutput c) | FOutput (FluidOutput c)
  deriving stock ( Generic )

deriving stock instance Eq (Unit c) => Eq (Output c)
deriving stock instance Show (Unit c) => Show (Output c)
deriving stock instance Ord (Unit c) => Ord (Output c)

instance UFunctor Output where
  umap f (IOutput i) = IOutput (umap f i)
  umap f (FOutput i) = FOutput (umap f i)

instance LabelOptic "name" A_Lens (Output c) (Output c) Text Text where
  labelOptic = lens (\case 
    IOutput i -> view #name i
    FOutput i -> view #name i)
    (\f n -> case f of
      IOutput i -> IOutput (set #name n i)
      FOutput i -> FOutput (set #name n i))

instance LabelOptic "name" A_Lens (ItemOutput c) (ItemOutput c) Text Text where
  labelOptic = #item % #name

instance LabelOptic "name" A_Lens (FluidOutput c) (FluidOutput c) Text Text where
  labelOptic = #fluid % #name

data Item = Item
  { name :: Text
  , fuelValue :: Int
  , stackSize :: Int
  , placeableInWorld :: Bool
  , tier :: Maybe Int
  }
  deriving stock (Eq, Show, Ord, Generic)

data Amount (c :: ChainType) = 
  FixedAmount (Unit c) 
  | VariableAmount (Unit c, Unit c)

deriving stock instance Eq (Unit c) => Eq (Amount c)
deriving stock instance Show (Unit c) => Show (Amount c)
deriving stock instance Ord (Unit c) => Ord (Amount c)

instance UFunctor Amount where
  umap f (FixedAmount i) = FixedAmount (f i)
  umap f (VariableAmount (m, m2)) = VariableAmount (f m, f m2)
  
data ItemOutput (c :: ChainType) = ItemOutput
  { item :: Item
  , amount :: Amount c
  , probability :: Double
  }
  deriving stock ( Generic )

deriving stock instance Eq (Unit c) => Eq (ItemOutput c)
deriving stock instance Show (Unit c) => Show (ItemOutput c)
deriving stock instance Ord (Unit c) => Ord (ItemOutput c)

instance UFunctor ItemOutput where
  umap f i = ItemOutput 
    { item = i ^. #item
    , amount = umap f (i ^. #amount)
    , probability = i ^. #probability
    }

instance UFunctor FluidOutput where
  umap f i = FluidOutput
    { fluid = i ^. #fluid
    , amount = umap f (i ^. #amount)
    , probability = i ^. #probability
    , fluidBoxIndex = i ^. #fluidBoxIndex
    }

type Temperature = Double 

data Fluid = Fluid
  { name :: Text
  , fuelValue :: Int
  , defaultTemperature :: Temperature
  , maxTemperature :: Temperature
  }
  deriving stock (Eq, Show, Ord, Generic)

data FluidOutput (c :: ChainType) = FluidOutput
  { fluid :: Fluid
  , amount :: Amount c
  , fluidBoxIndex :: Maybe Int
  , probability :: Double
  }
  deriving stock ( Generic )

deriving stock instance Eq (Unit c) => Eq (FluidOutput c)
deriving stock instance Show (Unit c) => Show (FluidOutput c)
deriving stock instance Ord (Unit c) => Ord (FluidOutput c)

type Category = Text

data Technology = Technology
  { name :: Text
  , unlocks :: Set (Recipe 'Truth)
  , prerequisites :: Set Technology
  }
  deriving stock (Eq, Show, Ord, Generic)

data Recipe (c :: ChainType) = Recipe
  { name :: Text
  , ingredients :: [Ingredient c]
  , outputs :: [Output c]
  , mainOutput :: Maybe (Output c)
  , time :: Double
  , category :: Category
  , isHidden :: Bool
  , tech :: Text
  }
  deriving stock ( Generic )

deriving stock instance Eq (Unit c) => Eq (Recipe c)
--deriving stock instance Show (Unit c) => Show (Recipe c)
deriving stock instance Ord (Unit c) => Ord (Recipe c)

instance Show (Recipe c) where
  show r = toString $ r ^. #name

instance UFunctor Recipe where
  umap f r = Recipe 
    { name = r ^. #name
    , ingredients = umap f <$> r ^. #ingredients
    , outputs = umap f <$> r ^. #outputs
    , mainOutput = umap f <$> r ^. #mainOutput
    , time = r ^. #time
    , category = r ^. #category
    , isHidden = r ^. #isHidden
    , tech = r ^. #tech
    }

data Machine = Machine
  { name :: Text
  , powerUsage :: Double
  , speed :: Double
  , categories :: Set Category
  , ingredientCount :: Int
  , size :: (Int, Int)
  , powerSource :: Maybe PowerSource
  }
  deriving stock (Eq, Show, Ord, Generic)

data PowerSource = 
  FPowerSource FluidPowerSource 
  | BPowerSource BurnerPowerSource 
  | EPowerSource ElectricPowerSource
  deriving stock (Eq, Show, Ord, Generic)

data BurnerPowerSource = BurnerPowerSource
  { effectivity :: Int
  , fuelCategories :: Set FuelCategory
  }
  deriving stock (Eq, Show, Ord, Generic)

type FuelCategory = Text

newtype ElectricPowerSource = ElectricPowerSource
  { drain :: Double
  }
  deriving stock (Eq, Show, Ord, Generic)

data FluidPowerSource = FluidPowerSource
  { effectivity :: Int
  , burnsFluid :: Bool
  , scaleFluidUsage :: Bool
  , usagePerTick :: Double
  , maxTemperature :: Maybe Temperature 
  , fluidBox :: FluidBox
  }
  deriving stock (Eq, Show, Ord, Generic)

data FluidBox = FluidBox 
  { index :: Int
  , isInputOnly :: Bool
  , baseArea :: Int
  , baseLevel :: Int
  , height :: Int
  , volume :: Int
  }
  deriving stock (Eq, Show, Ord, Generic)