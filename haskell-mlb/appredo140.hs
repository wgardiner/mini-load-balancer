{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Format
import Text.Printf

main = scotty 4000 $ do
    get "/" $ do
        html $ mconcat ["OK, no problema!"]
    get "/tetra/:xvar" $ do
        xstr <- param "xvar"
        let x = read xstr :: Integer
        -- let f :: Integer -> Integer
        let f y = y^y
        html $ "2870284825233255143293425779773010474816549532792526447357674239116969889572016253931041863751075945071489639405752308838852607490495396193485292848748017850777600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" 
        return ()
