(data (Option t)
    (Some t)
    None)

(data (Result t e)
    (Ok t)
    (Err e))

(export car (x) (Pure (-> ('(t)) (Option t)))
    (match x
        ((Cons n _) (Some n))
        (_ None)))

(export cdr (x) (Pure (-> ('(t)) '(t)))
    (match x
        ((Cons _ l) l)
        (_ '())))

(export map (f x) (Pure (-> ((Pure (-> (a) b)) '(a)) '(b)))
    (match x
        ((Cons h l) (Cons (f h) (map f l)))
        (_ '())))

(export fold (f init x) (Pure (-> ((Pure (-> (a b) b)) b '(a)) b))
    (match x
        ((Cons h l) (fold f (f h init) l))
        (_ init)))
