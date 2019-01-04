data Vector a = Vector a a a deriving (Show)

v_plus :: (Num a) => Vector a -> Vector a -> Vector a
v_plus (Vector i j k) (Vector p q r) = Vector (i+p) (j+q) (k+r)

v_prod :: (Num a) => Vector a -> a -> Vector a 
v_prod (Vector i j k) m = Vector (i*m) (j*m) (k*m)

s_prod :: (Num a) => Vector a -> Vector a -> a
s_prod (Vector i j k) (Vector p q r) = (i*p) + (j*q) + (k*r)
