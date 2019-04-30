hoge :: forall a. (a -> a) -> a -> a
hoge f x = f $ x
