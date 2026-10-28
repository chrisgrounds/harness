-- | The average token rate of the model's replies: the mean of each reply's
-- rate, not a rate over the time the run has taken.
module Client.Tokens (TokenStats (..), emptyTokenStats, addRate, averageTokens) where

-- | Running statistics of the token rates observed so far.
data TokenStats = TokenStats
  { -- | Sum of every token rate seen.
    totalTokensPerSecond :: Double,
    -- | How many token rates have been seen.
    sampleCount :: Int
  }
  deriving (Eq, Show)

-- | The token-rate statistics before any rates are seen: an empty accumulator.
emptyTokenStats :: TokenStats
emptyTokenStats = TokenStats 0 0

-- | Adds one token rate, adding it to the running total and increasing the sample count.
addRate :: TokenStats -> Double -> TokenStats
addRate (TokenStats total n) r = TokenStats (total + r) (n + 1)

-- | The mean of the token rates added so far, or 'Nothing' when none have been added.
averageTokens :: TokenStats -> Maybe Double
averageTokens (TokenStats _ 0) = Nothing
averageTokens (TokenStats total n) = Just (total / fromIntegral n)

