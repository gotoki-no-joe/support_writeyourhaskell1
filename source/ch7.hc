---------- p.112 List 7.4

mult = \x y -> case y of {
  0 -> 0 ;
  _ -> add x (mult x (sub y 1)) ; };
div = \x y -> let p = sub x y; q = add 1 (signum p); in
  case q of { 0 -> 0; _ -> add 1 (div p y); };
data List a = Nil | Cons a (List a);
head = \l -> case l of { Cons x _ -> x; };
null = \l -> case l of { Nil -> True; _ -> False; };
