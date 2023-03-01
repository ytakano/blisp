use blisp;

#[test]
fn test_transpile() {
    let expr = "
    (defun snoc (l y)
    (Pure (-> (
        '(t) t)
    '(t)))
    (match l
        (nil (Cons y nil))
        ((Cons h b) (Cons h (snoc b y)))))

    (defun rev (l)
    (Pure (-> (
        '(t))
    '(t)))
    (match l
        (nil nil)
        ((Cons h t) (snoc (rev t) h))))
        ";
    let exprs = blisp::init(expr, vec![]).unwrap();
    let ctx = blisp::typing(exprs).unwrap();

    println!("{}", blisp::transpile(&ctx));
}
