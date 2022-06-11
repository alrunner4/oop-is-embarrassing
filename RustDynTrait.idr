||| https://doc.rust-lang.org/book/ch17-02-trait-objects.html
|||
||| Rust isn't object-oriented, and takes inspiration for its traits from Haskell's typeclasses, so
||| this Idris translation doesn't differ by a huge amount, but I think it's worth taking a moment
||| to appreciate just how much less boilerplate and line noise is involved in this version even at
||| such a miniscule scale.
|||
||| The style of this implementation is of course by no means the only encoding that might be
||| considered functional, but I've chosen to highlight here a static prototype interface style that
||| closely resembles the inherit-to-implement object-oriented style without requiring even so much
||| as a type parameter. Perhaps more importantly, languages like Idris (and Haskell, et al) that
||| distinguish pure functions from effects enable us to additionally distinguish between
||| constructors for "classes" that have effects (like initializing a mutable variable) from ones
||| that don't.
|||
module RustDynTrait
import Data.IORef

public export
record Component where
   constructor ComponentDefinition
   draw: IO ()
   tick: Double -> IO ()

export
record Screen where
   constructor ScreenDefn
   components: List Component

export
run: Screen -> IO ()
run self = do
   traverse_ draw self.components
   traverse_ (`tick` 0.16) self.components

public export
Button: Int -> Int -> String -> Component
Button width height label = ComponentDefinition {
   draw = putStr "code to actually draw a button\n" ,
   tick = \_ => putStr "some time passes for a button\n" }

public export
SelectBox: {width, height: Int} -> {options: List String} -> IO Component
SelectBox = do
   selection <- newIORef (length options)
   pure$ ComponentDefinition {
      draw = putStr "code to actually draw select box selection \{show !(readIORef selection)}\n" ,
      tick = \_ => putStr "some time passes for a select box" }

main: IO ()
main = do
   selectBox <- SelectBox {
      width  = 75  ,
      height = 10  ,
      options = [
         "Yes"   ,
         "Maybe" ,
         "No"    ] }
   run $ ScreenDefn [
      selectBox ,
      Button 50 10 "OK" ]
