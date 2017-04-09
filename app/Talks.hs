{-# LANGUAGE OverloadedStrings #-}

module Talks where

import Model (Talk(..))

talks =
  [ Talk
      "Acknowledging boundaries"
      "Get consistency back in your μServices architecture: \
        \the best way to reduce complexity in a μServices architecture is to embrace boundaries. \
        \You'll see how to do it with the help of proper design and a good type system."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/acknowledging-boundaries-devoxxfr.html")
      (Just "https://www.youtube.com/watch?v=VmVQmC33UVM&list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC&index=4")
      True
  , Talk
      "TDD as in Type-Driven Development"
      "Test-Driven Development is widely accepted as good practice. But can we do better? \
      \By specifying your program's behaviour with types, you can go a very long way, \
      \with more confidence and with less hassle than with tests."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/type-dd-devoxxfr.html")
      (Just "https://www.youtube.com/watch?v=iMITdKE4dys&list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC&index=2")
      False
  , Talk
      "Functional patterns for scala practitioners"
      "Scala, by being both Functional and Object-Oriented is easy to get started with, especially for java developpers. \
      \However, to get the most of the language, you have to embrace its functional nature."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/fp-patterns-devoxx-be.html")
      (Just "https://www.youtube.com/watch?v=iRGcbCvrtiw&list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC&index=1")
      True
  , Talk
      "Algebraic Data Types for fun and profit"
      "Algebraic Data Types (ADTs) are a powerful tool to model your domain.\
      \Moreover, they share interesting properties with additon and multiplication (hence their name)."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/adt-devoxx-be.html")
      (Just "https://www.youtube.com/watch?v=EPxi546vVHI&list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC&index=5")
      False
  , Talk
      "Beyond flux: going full cycle with FRP"
      "React and flux have given us the single direction rendering loop. \
      \When you've started to follow this path, there's no going back. Using observables allow you to \
      \go one step further."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/frp-full-cycle-ncrafts.html")
      Nothing
      False
  , Talk
      "Tips and tricks for clean (relational) DB schemas"
      "Relational DBs are great at what they do. If you're going to use them for persitence, \
      \use them correctly, and don't use tools which only goal is to hide them from you."
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/tips-tricks-rdbms-geecon.html")
      Nothing
      False
  , Talk
      "Rust: are web web yet?"
      "Rust is touted for systems programming, but there are web stacks available: \
      \iron, nickel, rouille, now rocket. Web dev in rust, is it doable yet?"
      (Just "https://clementd-files.cellar.services.clever-cloud.com/blog/rust-web.html")
      Nothing
      False
  , Talk
      "Systemd: why use it? How does it work?"
      "After some heat, systemd has replaced initd in many GNU/Linux distros. \
      \Why? Is it hard to write a systemd config file? How does it work? How do you replace cron?"
      (Just "https://docs.google.com/presentation/d/17z2USTEqp3jcIilSbzfjpTV31PFUnier916Qytyqu6k/pub?start=false&loop=false&delayms=3000")
      Nothing
      False
  ]
