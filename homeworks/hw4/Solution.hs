newtype Reader r a = Reader { runReader :: r -> a }
-- ^ runReader executes a Reader computation by supplying an environment `r`
--   and returning a result of type `a`.
instance Functor (Reader r) where
-- fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader rf) = Reader (\r-> f (rf r))
-- Transforms the result of a Reader while keeping access to the environment.
-- Intuition: fmap f r = "run r in the environment, then apply f to the result".

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure a= Reader(\r -> a)
  -- Wraps a value into a Reader that ignores the environment and just returns it.

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader tf) (Reader ra) = Reader $ \r ->
    let
        a = ra r 
    in (tf r) a
  -- Runs both computations in the same environment, then applies the first
  -- result (a function) to the second result (its argument).

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) f = Reader $ \s->
    let
        a = ra s
        (Reader rb)= f a
    in rb s
  -- Sequences two Reader computations, passing the same environment to both.
  -- The second computation may depend on the value produced by the first.


-- Retrieves the entire environment.
ask :: Reader r r
ask = Reader (\r->r)

-- Retrieves a value derived from the environment by applying a projection.
-- Example: asks interestRate :: Reader BankConfig Double
asks :: (r -> a) -> Reader r a
asks f = Reader f
-- Runs a subcomputation in a locally modified environment.
-- The modification is only visible inside the passed Reader — once it
-- returns, the outer environment is restored (conceptually — there is no
-- mutable state, the modified environment simply goes out of scope).
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader(\s-> ra (f s))



-- Configuration of the banking application.
data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

-- A bank account.
data Account = Account
  { accountId :: String  -- account identifier
  , balance   :: Int     -- current balance
  } deriving (Show)


-- Computes the interest accrued on the account, based on the configured rate.
-- The result should be an Int — round/truncate as you see fit, but be
-- consistent.
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
    rate <- asks interestRate
    return (round (fromIntegral (balance acc) * rate))

-- Deducts the transaction fee from the account and returns the updated account.
-- The accountId should remain unchanged.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
    fee<- asks transactionFee
    return acc{balance=balance acc -fee}


-- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
    minb<- asks minimumBalance
    return ((balance acc)>minb)

-- Runs the three operations above on a single account and combines their
-- results. The returned tuple contains:
--   * the account after the transaction fee has been applied,
--   * the interest computed from the ORIGINAL account,
--   * whether the ORIGINAL account meets the minimum balance requirement.
-- Prefer `do`-notation here — this is the function that demonstrates why
-- Reader is convenient: the configuration is threaded implicitly.
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc =do
    ir<- calculateInterest acc 
    new_acc<- applyTransactionFee acc
    is_min<-checkMinimumBalance acc
    return (new_acc,ir,is_min)

cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
acc = Account { accountId = "A-001", balance = 1000 }


main::IO()

main = do 
    print $ runReader (calculateInterest acc) cfg
    print $ runReader (applyTransactionFee acc) cfg
    print $ runReader (checkMinimumBalance acc) cfg
    print $ runReader (processAccount acc) cfg