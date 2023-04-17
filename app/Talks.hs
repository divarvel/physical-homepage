{-# LANGUAGE OverloadedStrings #-}

module Talks where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map           as M
import           Data.Text          (Text)

import           Model              (Item, Lang (..), Talk (..))

(//) :: Lang -> Text -> M.Map Lang (NonEmpty Item)
l // u = M.singleton l (pure (pure u))

(///) :: Lang -> NonEmpty Item -> M.Map Lang (NonEmpty Item)
(///) = M.singleton

talks :: [Talk]
talks =
  [ 
    Talk
    "Distributed auth: don't give up"
    "A tour of distributed auth patterns, the associated challenges and how biscuit makes things easier"
    (Just "2023")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/devoxx-dist-auth.html")
    M.empty
    True
  , Talk
    "Authn/Authz: a tale of cookies and tokens"
    "A tour of authentication, authorization, relevant models and tooling"
    (Just "2022")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/devfest-biscuit.html")
    (Fr // "https://www.youtube.com/watch?v=8_-QGBMT7-k")
    False
  , Talk
    "« It’s traverse! »"
    "The `traverse` function is so pervasive in functional programming that it became a joke: \
    \`traverse` is the new `42`. \
    \Since it's a bit abstract until you actually encounter it, let's dig a little and review \
    \some case where… well, it was _actually_ `traverse`."
    (Just "2020")
    (En /// ( (Just "DDDDD", "https://clementd-files.cellar-c2.services.clever-cloud.com/ddddd-its-traverse.html")
           :| [(Just "LambdaLille", "https://clementd-files.cellar-c2.services.clever-cloud.com/lambdalille-its-traverse.html")]
            )
    )
    ( En /// pure (Just "DDDDD", "https://www.youtube.com/watch?v=sIJr3SCKhjo")
   <> Fr /// pure (Just "LambdaLille", "https://www.youtube.com/watch?v=qB_hDiQtrMo")
    )
    True
  , Talk
    "Application design with a stack of monads"
    "Functional Programming is often seen as a nice tool for small-scale concerns, not as a tool for application design. \
    \However, functional programming comes with interesting tools aimed at solving these boring (or deemed boring) concerns: \
    \dependency inversion, observability).\
    \A functional solution, called a monad stack, allows to combine all this cross-cutting concerns and behaviours with no magic \
    \(or not too much). Let's see how it works and how it relates to the current \"Tagless Final\" trend"
    (Just "2019")
    (Fr // "https://clementd-files.cellar-c2.services.clever-cloud.com/ncrafts-monad-stacks.html")
    (En // "http://videos.ncrafts.io/video/338475892")
    False
  , Talk
    "Config as Code? Yup, but properly. Have some Dhall"
    "Config files are getting bigger and bigger. Config languages are limited (by design). Using a general purpose language for config \
    \looks like a good idea but has severe drawbacks. Dhall is designed to bring flexibility and abstraction to config, while keeping \
    \ key properties."
    (Just "2019")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/web2day-dhall.html")
    (En // "https://www.youtube.com/watch?v=sb4LRqJsYPc")
    True
  , Talk
    "Dev environments: use the nix, Luke!"
    "Spinning up dev environments got incredibly complex. We now cheat by using containers or VMs, but these are extremely heavyweight solutions.\
    \We only care about the easy way to run software, not about the runtime isolation. Thankfully, there is nix, that does just that: \
    \allowing us to run software on your machine without trashing your global environment."
    (Just "2019")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/devoxx-nix.html")
    (Fr // "https://www.youtube.com/watch?v=v0NY2VNcoWU")
    False
  , Talk
    "Hoist me up, before you lift lift"
    "Servant allows you to describe and build web API is a type-safe and composable way. It also provides a powerful way \
    \to structure your applications by letting you choose a monad stack, either for the whole API or for specific parts of \
    \it. This mechanism, called `hoistServer` can be used eg. for dependency injection as well as access control."
    (Just "2019")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/hoist-haskell-paris.html")
    M.empty
    False
  , Talk
    "TDD as in Type-Driven Development"
    "Test-Driven Development is widely accepted as good practice. But can we do better? By specifying your program's \
    \behaviour with types, you can go a very long way, with more confidence and with less hassle than with tests."
    (Just "2015 - 2018")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/type-dd-devoxxfr.html")
    (En // "https://www.youtube.com/watch?v=iMITdKE4dys&amp;list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC&amp;index=2")
    False
  , Talk
    "Create a Rust CLI tool in a flash "
    "\"Tools not rules\". A process without a tool to guide is mistakes waiting to happen."
    (Just "2018")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/rust-cli-devoxxfr.html")
    (Fr // "https://www.youtube.com/watch?v=1SV2jQkegAY")
    False
  , Talk
    "Acknowledging boundaries"
    "Get consistency back in your μServices architecture: the best way to reduce complexity in a μServices architecture is to embrace boundaries. \
    \You'll see how to do it with the help of proper design and a good type system."
    (Just "2017")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/acknowledging-boundaries-devoxxfr.html")
    ( Fr // "https://www.youtube.com/watch?v=Daburx0jSvw" <>
      En // "https://www.youtube.com/watch?v=VmVQmC33UVM&amp;list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC&amp;index=4"
    )
    False
  , Talk
    "Haskell in production? In a startup? Yup"
    "Haskell is trendy, but too often for side-projects. At Fretlink, they put haskell in production, on real projects."
    (Just "2017")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/scala-io-haskell.html")
    (Fr // "https://www.youtube.com/watch?v=yKA-hYZa2tQ")
    False
  , Talk
    "All my team is using rust. HALP!"
    "Something strange happened at Clever Cloud: everybody has started using rust, little by little. Many greenfield projects are using rust. \
    \That's not to say we've stopped using scala, java, or ruby (and even a bit of go and haskell). As a CTO, how do I keep complexity and language diversity in check? How do I choose which language to use for projects? Do I let others choose?"
    (Just "2017")
    (Fr // "https://clementd-files.cellar-c2.services.clever-cloud.com/scala-io-rust.html")
    (Fr // "https://www.youtube.com/watch?v=gH5mPaLU8_g")
    False
  , Talk
    "Purescript, des sensations pures"
    "Dans le monde du compile-to-js, il y a des langages proches (genre coffeescript), \
    \et des langages qui vont un peu plus loin (genre typescript). Et puis il y a purescript. \
    \Ce soir, on verra le pourquoi et le comment, avec la bibliothèque halogen, qui permet de créer des applications/interface graphiques."
    (Just "2017")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/purescript-nantesjs.html")
    M.empty
    False
  , Talk
    "Algebraic data types for fun and profit"
    "As domain driven design practitioners, we have to design datastructures a lot. Often we have to encode our knowledge into a \
    \not-so-expressive type system. That's when the trouble starts: our types don't represent exactly what we have."
    (Just "2016")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/adt-devoxx-be.html")
    (En // "https://www.youtube.com/watch?v=EPxi546vVHI&amp;list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC&amp;index=5")
    False
  , Talk
    "Beyond flux: going full cycle with functional reactive programming."
    "React and flux have shown us how to design UI in a modular fashion. One of the core ideas, is that data should always flow in one direction. \
    \With this concept, it's easier to build applications from independent modules. I'll show what react core concepts are and how they allow developers to manage complexity in big applications."
    (Just "2016")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/frp-full-cycle-ncrafts.html")
    (Fr // "https://www.youtube.com/watch?v=ZAm-9i1O_HY&amp;t=34s")
    False
  , Talk
    "Functional patterns for scala practitioners"
    "Scala, by being both Functional and Object-Oriented is easy to get started with, especially for java developpers. \
    \However, to get the most of the language, you have to embrace its functional nature."
    (Just "2016")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/fp-patterns-devoxx-be.html")
    (En // "https://www.youtube.com/watch?v=iRGcbCvrtiw&amp;list=PLvjEkX1131rDgetaKc2wLqGT92ThIjEaC&amp;index=1")
    False
  , Talk
    "Tips and tricks for clean relational db schemas"
    "It can be usual for software developers to let the ORM take care of the database schema. In many cases it's a \
    \bad idea as it makes the data stored in your database brittle and hard to use confidently. \
    \I'll show a few tricks which will help you cleanly store and query data by using your database engine to its full power."
    (Just "2014")
    (En // "https://clementd-files.cellar-c2.services.clever-cloud.com/blog/tips-tricks-rdbms-geecon.html")
    (Fr // "https://www.youtube.com/watch?v=-JOE8c0v84Q")
    False
  , Talk
    "Survival guide: modern CSS for the grumpy backend developer"
    "Backend programming sounds hard. But we all know what really strikes fear in the hearts of backend devs: CSS. \
    \Building a simple page layout is a daunting task: you need to juggle with floats, margins and all that sort of things. \
    \But what if I told you handling layout in CSS has improved dramatically? Or, Djikstra forbid, that it's now kind of easy? \
        \Let's see what we can do with flexbox and grid; I promise you you won't be scared of CSS layout anymore."
    Nothing
    M.empty
    M.empty
    False
  ]
