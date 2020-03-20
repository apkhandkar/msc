module LZ (
        LZ,
        lz,
        snag,
        lnext,
        lprev,
        lfirst,
        lins,
        lapp,
        (-:),
        (.>),
        compose 
    ) where

data LZ a = Zipper ([a], [a]) | Snag deriving (Show, Eq)

instance Semigroup (LZ a) where
    (Zipper (li, il))   <> z = Zipper ((li <> (getLi z)), il)
    _                   <> _ = Snag

instance Monoid (LZ a) where
    mempty = Zipper ([], []) 

instance Functor LZ where
    f   `fmap`  (Zipper (li, il))   = Zipper (f <$> li, f <$> il)
    _   `fmap`  _                   = Snag

instance Applicative LZ where
    pure    li                      = Zipper (li:[], [])
    Snag    <*> _                   = Snag
    fz      <*> (Zipper (n:[], [])) = Zipper (($ n):[], []) <*> fz
    fz      <*> lz@(Zipper (_, _))  = appLZ fz lz
    _       <*> _                   = Snag


-- UNSAFE; do not use by itself
getLi :: LZ a -> [a]
getLi z = (\(Zipper (l, _)) -> l) $ lfirst $ z

appLZ :: LZ (a -> b) -> LZ a -> LZ b
appLZ fz lz@(Zipper (li, il)) =
        let
            steps   = length il 
            sf 0 zi = zi
            sf n zi = sf (n-1) (lnext zi)
        in
            sf steps (Zipper ((getLi fz) <*> (getLi lz), []))

-- safe 
snag :: LZ a
snag = Snag

lz :: [a] -> LZ a
lz li = Zipper (li, [])

(-:) :: a -> (a -> b) -> b
a -: fs = fs a

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g.f

compose :: [b -> b] -> b -> b
compose [] = id
compose (f:fs) = f .> (compose fs)

lnext :: LZ a -> LZ a
lnext (Zipper ([], il))     = Snag
lnext (Zipper (l:li, il))   = Zipper (li, l:il)
lnext _                     = Snag

lprev :: LZ a -> LZ a
lprev (Zipper (li, []))     = Snag
lprev (Zipper (li, i:il))   = Zipper (i:li, il)
lprev _                     = Snag

lfirst :: LZ a -> LZ a
lfirst (Zipper (li, []))    = Zipper (li, [])
lfirst (Zipper (li, il))    = lfirst $ lprev $ Zipper (li, il)
lfirst _                    = Snag

lins :: a -> LZ a -> LZ a
lins e (Zipper (li, il))    = Zipper (e:li, il)
lins _ _                    = Snag

lapp :: [a] -> LZ a -> LZ a
lapp l (Zipper (li, il))    = Zipper (l++li, il)
lapp _ _                    = Snag
