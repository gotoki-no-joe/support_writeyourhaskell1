---------- p.063 List 5.6

mult = \x y -> if0 y then 0 else add x (mult x (sub y 1));
div = \x y -> let p = sub x y; q = add 1 (signum p);
              in if0 q then 0 else add 1 (div p y);

data List a = Nil | Cons a (List a);
head = \l -> case l of { Cons x _ -> x; };
null = \l -> case l of { Nil -> True; _ -> False; };
