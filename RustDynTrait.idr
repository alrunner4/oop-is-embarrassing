||| https://doc.rust-lang.org/book/ch17-02-trait-objects.html
|||
||| Rust isn't object-oriented, and takes inspiration for its traits from Haskell's typeclasses, so
||| this Idris translation doesn't differ by a huge amount, but I think it's worth taking a moment
||| to appreciate just how much less boilerplate and line noise is involved in this version even at
||| such a miniscule scale.
|||
||| I've chosen to deviate from using a DPair representation within Screen to more closely mirror
||| the Rust reference, or even a reified interface-as-record, just to demonstrate that often when
||| "heterogeneous" list is the supposed requirement, a homogeneous list is often a simpler and
||| clearer choice.

module RustDynTrait

public export
interface Draw t where
   draw: t -> IO ()

export
record Screen where
   constructor ScreenDefn
   components: List (IO ())

export
run: Screen -> IO ()
run self = sequence_ self.components

public export
record Button where
   constructor ButtonDefn
   width:  Int
   height: Int
   label:  String

export
Draw Button where
   draw _ = putStr "code to actually draw a button\n"

public export
record SelectBox where
   constructor SelectBoxDefn
   width:   Int
   height:  Int
   options: List String

export
Draw SelectBox where
   draw _ = putStr "code to actually draw a select box\n"

main: IO ()
main = run$ScreenDefn {
   components = [
      draw$SelectBoxDefn {
         width  = 75 ,
         height = 10 ,
         options = [
            "Yes"   ,
            "Maybe" ,
            "No"    ] } ,
      draw$ButtonDefn {
         width  = 50  ,
         height = 10  ,
         label = "OK" } ] }
