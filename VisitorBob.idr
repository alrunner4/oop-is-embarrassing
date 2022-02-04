||| https://web.archive.org/web/20151022084246/http://objectmentor.com/resources/articles/visitor.pdf
module VisitorBob
import OOP.VisitorPattern

import Control.Monad.ST
import Control.Monad.State
import Data.IORef
import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String

namespace TestHelper

   public export
   assertSequence: Applicative f => List (f Bool) -> f Bool
   assertSequence checks = ( and . map delay ) <$> sequence checks

namespace Vendor

   public export
   record Modem ( id : Type ) where
      constructor ModemDefn
      Dial:   IO ()
      Hangup: IO ()
      Send:   IO ()
      Recv:   IO ()
      Config: IORef id

   export
   record Hayes where
      constructor HayesConfig
      configurationString: String

   export
   record Zoom where
      constructor ZoomConfig
      configurationValue: Int

   export
   record Ernie where
      constructor ErnieConfig
      internalPattern: String

   namespace Dummy

      dummyModem: String -> m -> IO( Modem m )
      dummyModem name cfg = ModemDefn
         ( putStrLn $ "Dial "         ++ name )
         ( putStrLn $ "Hang up "      ++ name )
         ( putStrLn $ "Send to "      ++ name )
         ( putStrLn $ "Recieve from " ++ name )
         <$> newIORef cfg

      export
      HayesModem: IO( Modem Hayes )
      HayesModem = dummyModem "Hayes" (HayesConfig "")

      export
      ZoomModem: IO( Modem Zoom )
      ZoomModem = dummyModem "Zoom" (ZoomConfig 0)

      export
      ErnieModem: IO( Modem Ernie )
      ErnieModem = dummyModem "Ernie" (ErnieConfig "")


namespace ClassicVisitor

   ||| `KnownModem` enumerates our supported modems in the same way that the "ModemVisitor" base
   |||    class does in the reference source.
   public export
   data KnownModem
      = Hayes (Modem Vendor.Hayes)
      | Zoom  (Modem Vendor.Zoom )
      | Ernie (Modem Vendor.Ernie)

   public export
   KnownModemVisitor: Type -> Type
   KnownModemVisitor a = KnownModem -> IO a

   export total
   configureForUnix: KnownModemVisitor ()
   configureForUnix = \case
      Hayes hayesModem =>
         modifyIORef hayesModem.Config { configurationString $= const "&s1=4&D=3" }
      Zoom  zoomModem  =>
         modifyIORef zoomModem.Config { configurationValue $= const 42 }
      Ernie ernieModem =>
         modifyIORef ernieModem.Config { internalPattern $= const "C is too slow" }

namespace AcyclicVisitor

   ||| If we need to be able to handle more variety in concrete `Modem` type than is reasonable to
   |||    enumerate in one place, we can use a `Dynamic` value, much akin to the reference source's
   |||    use of an empty base class to unite a hierarchy of visitor types.
   export
   DynamicModemVisitor: Type -> Type
   DynamicModemVisitor a = Dynamic Modem -> IO a

   ||| Since `Modem t` doesn't constrain `t`, we can't write cases for all possible `t`, and end up
   |||    with a catchall case equivalent to the reference source's failure to cast to a known modem
   |||    type.
   export
   UnixModemConfigurator: DynamicModemVisitor ()
   UnixModemConfigurator = \case
      ( Ernie ** ernieModem ) =>
         modifyIORef ernieModem.Config { internalPattern $= const "C is too slow" }
      ( Hayes ** hayesModem ) =>
         modifyIORef hayesModem.Config { configurationString $= const "&s1=4&D=3" }
      ( Zoom  ** zoomModem  ) =>
         modifyIORef zoomModem.Config { configurationValue $= const 42 }
      _ => pure ()

   namespace TestModemVisitor

      export
      testHayesForUnix: IO Bool
      testHayesForUnix = do
         h <- HayesModem
         UnixModemConfigurator( dynamic h )
         readIORef h.Config <&> \c => c.configurationString == "&s1=4&D=3"

      export
      testZoomForUnix: IO Bool
      testZoomForUnix = do
         z <- ZoomModem
         UnixModemConfigurator( dynamic z )
         readIORef z.Config <&> \c => c.configurationValue == 42

      export
      testErnieForUnix: IO Bool
      testErnieForUnix = do
         e <- ErnieModem
         UnixModemConfigurator( dynamic e )
         readIORef e.Config <&> \c => c.internalPattern == "C is too slow"

namespace ReportGenerator

   public export
   record Part p where
      constructor PartDetails
      partNumber:  String
      description: String
      part: p

   Functor Part where
      map f p = { part $= f } p

   PartVisitor: Type -> Type
   PartVisitor t = DynamicVisit Part (Part t)

   ||| A convenience function to make a `Part` `Visitable`.
   VisitablePart: {t: Type} -> PartVisitor t => String -> String -> t -> Visitable Part
   VisitablePart partNumber description part
      = visitable$ PartDetails { partNumber, description, part }

   export
   Eq p => Eq (Part p) where
      l == r =
         l.partNumber == r.partNumber &&
         l.description == r.description &&
         l.part == r.part

   export
   data Piece = MkPiece Double

   export Eq Piece where MkPiece l == MkPiece r = l == r

   DynamicVisit Part (Part Piece) where
      (.visit) piece visitor = visitor( dynamic piece )

   ||| Augment the `Part` record with projection of cost for `Piece`s.
   export
   (.cost): Part Piece -> Double
   (.cost) piece = let MkPiece c = piece.part in c

   export
   data Assembly = MkAssembly (List (Visitable Part))

-- NOTE: Proving totality of Assembly equality is nontrivial since matching on types gets involved.
--    Ostensibly, all this Visitable machinery is so that we can do more flexible collection than
--    homogeneougs Eq should be able to handle, anyway.
-- export
-- Eq Assembly where
--    MkAssembly( dl :: ls ) == MkAssembly( dr :: rs )
--       = case ( dl, dr ) of
--          ((Piece    ** (l, _)), (Piece    ** (r, _))) => l == r && MkAssembly ls == MkAssembly rs
--          ((Assembly ** (l, _)), (Assembly ** (r, _))) => l == r && MkAssembly ls == MkAssembly rs
--          _ => False
--    MkAssembly [] == MkAssembly [] = True
--    _ == _ = False

   export
   add: {p: Type} -> PartVisitor p => Part p -> Part Assembly -> Part Assembly
   add newPart assembly = let MkAssembly parts = assembly.part in
      { part := MkAssembly( visitable newPart :: parts ) } assembly

   ||| Augment the `Part` record with projection of `Assembly` sub-parts.
   export
   (.parts): Part Assembly -> List (Visitable Part)
   (.parts) p = let MkAssembly ps = p.part in ps

   ||| Defines a pre-order visitation for `Assembly` `Part`s and their sub-parts.
   export
   DynamicVisit Part (Part Assembly) where
      (.visit) assembly visitor = map concat $
         sequence( visitor( dynamic assembly ) :: map (\x => x.visit visitor) assembly.parts )

   export
   ExplodedCostVisitor: Visitable Part -> Double
   ExplodedCostVisitor p = p.concat {Monoid = Additive} $ \case
      (Piece ** piece) => piece.cost
      _ => 0

   public export
   record PartCountSummary where
      constructor MkSummary
      pieceCount: Nat
      partNumberCount: Nat
      countForPart: String -> Nat

   export
   PartCountVisitor: Visitable Part -> PartCountSummary
   PartCountVisitor p = evalState (Z, SortedMap.empty {k=String,v=Nat}) $ do
      p.visit$ \case
         (Piece ** p) => case SortedMap.lookup p.partNumber (Builtin.snd !get) of
            Nothing => modify$ \(count, pm) => (count+1, insert p.partNumber 1 pm)
            Just carrier => modify$ \(count, pm) => (count+1, insert p.partNumber (carrier+1) pm)
         _ => pure ()
      (pieceCount, pm) <- get
      pure $MkSummary {
         pieceCount ,
         partNumberCount = length( keys pm ) ,
         countForPart = \p => 0 `fromMaybe` lookup p pm }

   namespace TestBOMReport

      private a:  Part Assembly ; a  = PartDetails   "5879" "MyAssembly" $MkAssembly []
      private p1: Part Piece    ; p1 = PartDetails "997624" "MyPart"     $MkPiece 3.20
      private p2: Part Piece    ; p2 = PartDetails   "7734" "Hell"       $MkPiece 666

      export
      testCreatePart: Bool
      testCreatePart =
         p1.partNumber  == "997624" &&
         p1.description == "MyPart" &&
         p1.cost        ==  3.20

      export
      testCreateAssembly: Bool
      testCreateAssembly =
         a.partNumber  == "5879" &&
         a.description == "MyAssembly"

      export
      testAssembly: Bool
      testAssembly = let
         b = (add p2 . add p1) a
         in case map unvisitable b.parts of
            [(Piece ** p1g), (Piece ** p2g)] => p1g == p1 && p2g == p2
            _ => False

      export
      testAssemblyOfAssemblies: Bool
      testAssemblyOfAssemblies = let
         subAssembly = add p1 $PartDetails "1324" "SubAssembly" $MkAssembly []
         b = add subAssembly a
         in case map unvisitable b.parts of
            [(Assembly ** i)] => i.partNumber == "1234" &&
                                 i.description == "SubAssembly" &&
                                 null i.parts
            _ => False

      export
      testVisitorCoverage: Bool
      testVisitorCoverage = runST$ do
         p1Found <- newSTRef False
         p2Found <- newSTRef False
         bFound  <- newSTRef False
         let b = (add p2 . add p1) a
         b.visit$ \case
            (Piece ** p) =>
               if      p == p1 then writeSTRef p1Found True
               else if p == p2 then writeSTRef p2Found True
               else pure ()
            (Assembly ** assy) => when( assy.partNumber == "5879" )$ writeSTRef bFound True
            _ => pure ()
         assertSequence [readSTRef p1Found, readSTRef p2Found, readSTRef bFound]

      private
      cellphone: Part Assembly
      cellphone = PartDetails "CP-7734" "Cell Phone" $MkAssembly [
         VisitablePart "DS-1428" "LCD Display" $MkPiece 14.37 ,
         VisitablePart "SP-92"   "Speaker"     $MkPiece  3.50 ,
         VisitablePart "MC-28"   "Microphone"  $MkPiece  5.30 ,
         VisitablePart "CR-56"   "Cell Radio"  $MkPiece 30.00 ,
         VisitablePart "FC-77"   "Front Cover" $MkPiece  1.40 ,
         VisitablePart "RC-77"   "RearCover"   $MkPiece  1.20 ,
         VisitablePart "KP-62"   "Keypad"      $MkAssembly (
            let button = visitable$ PartDetails "B52" "Button" $MkAssembly [
                           visitable$ PartDetails "CV-15" "Cover"   $MkPiece 0.5 ,
                           visitable$ PartDetails "CN-2"  "Contact" $MkPiece 1.2 ]
            in replicate 15 button ) ]

      export
      testExplodedCost: Bool
      testExplodedCost = ExplodedCostVisitor( visitable cellphone ) == 81.27

      export
      testPartCount: Bool
      testPartCount =
         let summary = PartCountVisitor( visitable cellphone )
         in and$
            ( summary.pieceCount == 36 ) ::
            ( summary.partNumberCount == 8 ) ::
            map (\(p,c) => summary.countForPart p == c) [
               ( "DS-1428" ,  1 ) ,
               ( "SP-92"   ,  1 ) ,
               ( "MC-28"   ,  1 ) ,
               ( "CR-56"   ,  1 ) ,
               ( "RC-77"   ,  1 ) ,
               ( "CV-15"   , 15 ) ,
               ( "CN-2"    , 15 ) ,
               ( "Bob"     ,  0 ) ]

namespace Decorator

   record Modem where
      constructor ModemCtl
      dial: String -> IO ()
      getPhoneNumber: IO (Maybe String)
      setSpeakerVolume: Int -> IO ()
      getSpeakerVolume: IO Int

   LoudDialModem: Modem -> Modem
   LoudDialModem m = { dial := \phoneNumber => m.setSpeakerVolume 10 >> m.dial phoneNumber } m

   HayesModem: IO Modem
   HayesModem = do
      volume <- newIORef 0
      connectedNumber <- newIORef Nothing
      pure$ ModemCtl {
         dial = \phoneNumber => do
            putStrLn$ "Dial " ++ phoneNumber ++ " (volume " ++ show !(readIORef volume) ++ ")"
            writeIORef connectedNumber (Just phoneNumber) ,
         getPhoneNumber   = readIORef  connectedNumber ,
         setSpeakerVolume = writeIORef volume ,
         getSpeakerVolume = readIORef  volume }

   namespace ModemDecoratorTest

      export
      testCreateHayes: IO Bool
      testCreateHayes = do
         m <- HayesModem
         assertSequence [
            m.getPhoneNumber <&> (== Nothing) ,
            m.dial "5551212" >> m.getPhoneNumber <&> (== Just "5551212") ,
            m.getSpeakerVolume <&> (== 0) ,
            m.setSpeakerVolume 10 >> m.getSpeakerVolume <&> (== 10) ]

      export
      testLoudDialModem: IO Bool
      testLoudDialModem = do
         m <- HayesModem
         let d = LoudDialModem m
         assertSequence [
            d.getPhoneNumber <&> (== Nothing) ,
            d.getSpeakerVolume <&> (== 0) ,
            d.dial "5551212" >> d.getPhoneNumber <&> (== Just "5551212") ,
            d.getSpeakerVolume <&> (== 10) ]

namespace Extension

   record XMLElement where
      constructor MkXMLElement
      getName:     String
      getTextTrim: String
      getChild:    String -> Maybe XMLElement
      getChildren: List XMLElement

   newElement: String -> String -> List XMLElement -> XMLElement
   newElement name text children = let
      childrenMap = SortedMap.fromList( children <&> \c => (c.getName, c) )
      in MkXMLElement {
            getName     = name ,
            getTextTrim = trim text ,
            getChild    = \n => lookup n childrenMap ,
            getChildren = children }

   interface ToXML (a: Type) where
      toXML: a -> XMLElement
   interface ToCSV (a: Type) where
      toCSV: a -> String

   AssemblyConstraints: Type -> Type
   AssemblyConstraints t = (ToXML t, ToCSV t)

   ||| As long as `Part t` can be converted to XML *and* to CSV, it can be added to this Assembly.
   data Assembly = MkAssembly( List (Part `ExtendWith` AssemblyConstraints) )

   (.parts): Part Extension.Assembly -> List (Part `ExtendWith` AssemblyConstraints)
   (.parts) details = let MkAssembly ps = details.part in reverse ps

   (.add): {p: Type} -> {auto extension: AssemblyConstraints(Part p)} ->
      Part Extension.Assembly -> List (Part p) -> Part Extension.Assembly
   (.add) assembly newParts = assembly <&> \(MkAssembly ps) => MkAssembly$
         reverseOnto assembly.parts (newParts <&> extendWith AssemblyConstraints)

   ToXML (Part Piece) where
      toXML piece = newElement "PiecePart" "" [
         newElement "PartNumber"  piece.partNumber  [] ,
         newElement "Description" piece.description [] ,
         newElement "Cost"        (show piece.cost) [] ]

   ToXML (Part Extension.Assembly) where
      toXML assembly = newElement "Assembly" "" [
         newElement "PartNumber"  assembly.partNumber  [] ,
         newElement "Description" assembly.description [] ,
         newElement "Parts" ""
            $ assembly.parts <&> \(_**(p,_)) => toXML p ]

   ToCSV (Part Piece) where
      toCSV piece = concat [
         "PiecePart,"           ,
         piece.partNumber,  "," ,
         piece.description, "," ,
         show piece.cost        ]

   ToCSV (Part Extension.Assembly) where
      toCSV assembly = concat$ intersperse "," $
         "Assembly" ::
         assembly.partNumber  ::
         assembly.description ::
         (assembly.parts <&> (\(_**(p,_)) => toCSV p) <&> \pcsv => "{\{pcsv}}")

   namespace TestBOMXML

      private p1: Part Piece; p1 = PartDetails "997624" "MyPart" $MkPiece 3.20
      private p2: Part Piece; p2 = PartDetails   "7734" "Hell"   $MkPiece 666
      private a:  Part Extension.Assembly; a = PartDetails "5879" "MyAssembly" $MkAssembly []

      export
      testPiecePart1XML: Bool
      testPiecePart1XML =
         let xe = toXML p1
         in and [
            xe.getName == "PiecePart" ,
            (xe.getChild "PartNumber"  <&> getTextTrim) == Just "997624" ,
            (xe.getChild "Description" <&> getTextTrim) == Just "MyPart" ,
            (xe.getChild "Cost" <&> cast . getTextTrim) == Just  3.20    ]

      export
      testPiecePart2XML: Bool
      testPiecePart2XML =
         let xe = toXML p2
         in and [
            xe.getName == "PiecePart" ,
            (xe.getChild "PartNumber"  <&> getTextTrim) == Just "7734" ,
            (xe.getChild "Description" <&> getTextTrim) == Just "Hell" ,
            (xe.getChild "Cost" <&> cast . getTextTrim) == Just  666   ]

      export
      testSimpleAssemblyXML: Bool
      testSimpleAssemblyXML =
         let xe = toXML a
         in and [
            xe.getName == "Assembly" ,
            (xe.getChild "PartNumber"  <&> getTextTrim) == Just "5879" ,
            (xe.getChild "Description" <&> getTextTrim) == Just "MyAssembly" ,
            (xe.getChild "Parts" <&> length . getChildren) == Just 0 ]

      export
      testAssemblyWithPartsXML: Bool
      testAssemblyWithPartsXML =
         let b = a.add [p1, p2]
             xe = toXML b
         in and [
            xe.getName == "Assembly" ,
            (xe.getChild "PartNumber"  <&> getTextTrim) == Just "5879"       ,
            (xe.getChild "Description" <&> getTextTrim) == Just "MyAssembly" ,
            fromMaybe False $xe.getChild "Parts" <&> getChildren <&> \case
               [xml1, xml2] => and [
                  xml1.getName == "PiecePart" ,
                  (xml1.getChild "PartNumber" <&> getTextTrim) == Just "997624" ,
                  xml2.getName == "PiecePart" ,
                  (xml2.getChild "PartNumber" <&> getTextTrim) == Just "7734" ]
               _ => False ]

      export
      testPiecePart1CSV: Bool
      testPiecePart1CSV = toCSV p1 == "PiecePart,997624,MyPart,3.2"

      export
      testPiecePart2CSV: Bool
      testPiecePart2CSV = toCSV p2 == "PiecePart,7734,Hell,666.0"

      export
      testSimpleAssemblyCSV: Bool
      testSimpleAssemblyCSV = toCSV a == "Assembly,5879,MyAssembly"

      export
      testAssemblyWithPartsCSV: Bool
      testAssemblyWithPartsCSV = toCSV( a.add [p1,p2] ) ==
         "Assembly,5879,MyAssembly," ++
         "{PiecePart,997624,MyPart,3.2}," ++
         "{PiecePart,7734,Hell,666.0}"

