{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Format
import Text.Printf

main = scotty 4052 $ do
    get "/" $ do
        html $ mconcat ["OK, no problema!"]
--    get "/tetra/:xvar" $ do
--        xstr <- param "xvar"
--        let x = read xstr :: Integer
--        let result = x^x :: Integer
        -- f :: Integer -> Data.Text
        -- f = Data.Text.pack . show
        -- instance PrintfType Data.Text.Internal.Lazy.Text
        -- html $ Data.Text.Format.print "{}" xstr
--        html $ text result 
