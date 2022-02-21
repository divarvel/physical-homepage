{-# LANGUAGE OverloadedStrings #-}

module Projects where

import           Model (Project (..))

projects :: [Project]
projects =
  [ Project
    "Biscuit haskell"
    "Haskell support for the biscuit tokens, along with helpers for use in servant web APIs"
    "https://github.com/divarvel/biscuit-haskell"
  , Project
    "Biscuit CLI"
    "CLI tool for biscuit tokens (creation, inspection / authorization, attenuation)"
    "https://github.com/biscuit-auth/biscuit-cli"
  ]
