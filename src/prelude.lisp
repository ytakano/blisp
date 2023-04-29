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

(export filter (f x)
    (Pure (->
        ((Pure (-> (t) Bool)) '(t))
        '(t)))
    (reverse (filter' f x '())))

(defun filter' (f x l)
    (Pure (-> (
        (Pure (-> (t) Bool)) '(t) '(t))
        '(t)))
    (match x
        ((Cons h a) (if (f h) (filter' f a (Cons h l)) (filter' f a l) ))
        (_ l)))

(export reverse (x) (Pure (-> ('(t)) '(t)))
    (reverse' x '()))

(defun reverse' (x l) (Pure (-> ('(t) '(t)) '(t)))
    (match x
        ((Cons h a) (reverse' a (Cons h l)))
        (_ l)))

(macro add
  (($e1 $e2) (+ $e1 $e2))
  (($e1 $e2 $e3 ...) (+ $e1 (add $e2 $e3 ...))))