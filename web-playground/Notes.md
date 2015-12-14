#### #1

[HVectElim Documentation](http://haddock.stackage.org/lts-3.12/hvect-0.2.0.0/Data-HVect.html#t:HVectElim)
This is brilliant :)

```haskell
-- todo: use a closed type family once GHC 7.6 compatibility is dropped
type family HVectElim (ts :: [*]) (a :: *) :: *
type instance HVectElim '[] a = a
type instance HVectElim (t ': ts) a = t -> HVectElim ts a
```

#### #2 Spock authentification

https://www.spock.li/2015/08/23/taking_authentication_to_the_next_level.html

#### #3 PG fromRow

http://janrain.com/blog/tutorial-building-a-sample-application-with-haskell-snap-postgresql-and-the-postgresql-simple-snaplet/