{-# LANGUAGE OverloadedStrings #-}

module Talks where

import Model (Talk(..))

talks =
  [ Talk
      "Acknowledging boundaries"
      "Get consistency back in your μServices architecture: \
        \the best way to reduce complexity in a μServices architecture is to embrace boundaries. \
        \You'll see how to do it with the help of proper design and a good type system."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/acknowledging-boundaries.html")
      True
  , Talk
      "TDD as in Type-Driven Development"
      "Test-Driven Development is widely accepted as good practice. But can we do better? \
      \By specifying your program's behaviour with types, you can go a very long way, \
      \with more confidence and with less hassle than with tests."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/type-dd-devoxxfr.html")
      False
  , Talk
      "Functional patterns for scala practitioners"
      "Scala, by being both Functional and Object-Oriented is easy to get started with, especially for java developpers. \
      \However, to get the most of the language, you have to embrace its functional nature."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/fp-patterns-devoxx-be.html")
      True
  , Talk
      "Algebraic Data Types for fun and profit"
      "Algebraic Data Types (ADTs) are a powerful tool to model your domain.\
      \Moreover, they share interesting properties with additon and multiplication (hence their name)."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/adt-devoxx-be.html")
      False
  , Talk
      "Beyond flux: going full cycle with FRP"
      "React and flux have given us the single direction rendering loop. \
      \When you've started to follow this path, there's no going back. Using observables allow you to \
      \go one step further."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/frp-full-cycle-ncrafts.html")
      False
  , Talk
      "Tips and tricks for clean (relational) DB schemas"
      "Relational DBs are great at what they do. If you're going to use them for persitence, \
      \use them correctly, and don't use tools which only goal is to hide them from you."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/tips-tricks-rdbms-geecon.html")
      False
  ]
