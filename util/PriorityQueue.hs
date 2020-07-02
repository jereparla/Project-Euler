module PriorityQueue where    
    infixr 5 :-
    data Queue a b = Queue
        { minKey :: !a
        , minVal :: b
        , rest   :: List a b
        }

    data List a b
        = Nil
        | (:-) {-# UNPACK #-} !(Queue a b)
            (List a b)


    (<+>) :: Ord a => Queue a b -> Queue a b -> Queue a b
    (<+>) q1@(Queue x1 y1 ts1) q2@(Queue x2 y2 ts2)
    | x1 <= x2 = Queue x1 y1 (q2 :- ts1)
    | otherwise = Queue x2 y2 (q1 :- ts2)

    mergeQs :: Ord a => List a b -> Queue a b
    mergeQs (t :- ts) = mergeQs1 t ts
    mergeQs Nil       = errorWithoutStackTrace "tried to merge empty list"

    mergeQs1 :: Ord a => Queue a b -> List a b -> Queue a b
    mergeQs1 t1 Nil              = t1
    mergeQs1 t1 (t2 :- Nil)      = t1 <+> t2
    mergeQs1 t1 (t2 :- t3 :- ts) = (t1 <+> t2) <+> mergeQs1 t3 ts

    insert :: Ord a => a -> b -> Queue a b -> Queue a b
    insert !k !v = (<+>) (singleton k v)

    singleton :: a -> b -> Queue a b
    singleton !k !v = Queue k v Nil