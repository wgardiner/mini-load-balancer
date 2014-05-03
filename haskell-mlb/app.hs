{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Format

main = scotty 4000 $ do
    get "/" $ do
        html $ mconcat ["OK, no problema!"]
    get "/tetra/:xvar" $ do
        xstr <- param "xvar"
        let x = read xstr :: Integer
        let f y = y^y
        Data.Text.Format.print "{}" $ Data.Text.Format.Only (f x)
        html $ "Tetration is output to console :/"
        return ()
