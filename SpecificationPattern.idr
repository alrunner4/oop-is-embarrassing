||| https://en.wikipedia.org/wiki/Specification_pattern
module SpecificationPattern

public export
Predicate: Type -> Type
Predicate t = t -> Bool

public export
(.And), (.AndNot), (.Or), (.OrNot): Predicate t -> Predicate t -> Predicate t

(.And)    l r candidate = l candidate &&      r candidate
(.AndNot) l r candidate = l candidate && not( r candidate )
(.Or)     l r candidate = l candidate ||      r candidate
(.OrNot)  l r candidate = l candidate || not( r candidate )

public export
(.Not): Predicate t -> Predicate t
(.Not) other candidate = not( other candidate )

||| To quote Wikipedia for clarity here:
||| > [W]e are retrieving invoices and sending them to a collection agency if
||| > 1. they are overdue,
||| > 2. notices have been sent, and
||| > 3. they are not already with the collection agency.
namespace ExampleOfUse
   Invoice: Type
   GetInvoices: IO (List Invoice)
   SendToCollection: List Invoice -> IO ()
   example: IO ()
   example = let
      overDue:      Predicate Invoice
      noticeSent:   Predicate Invoice
      inCollection: Predicate Invoice
      shouldSendToCollection: Predicate Invoice
      shouldSendToCollection = (overDue.And noticeSent).And inCollection.Not
    in GetInvoices <&> filter shouldSendToCollection >>= SendToCollection
