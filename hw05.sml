
datatype term = Num of int
              | Var of string
              | Cnd of bool
              | If of term * term * term
              | Plus of term * term
              | Less of term * term
              | Let of  Var * term * term
              | Lam of Var * term
              | App of term * term

fun FV (Num i)           = []
  | FV (Cnd b)           = []
  | FV (Var x)           = [x]
  | FV (Plus t1 t2)      = FV (t1) @ FV (t2)
  | FV (If t1 t2 t3)     = FV (t1) @ FV (t2) @ FV(t3)
  | FV (Let x t1 t2)     = FV (t1) @ (delete x (FV (t2)))

and delete k nil = nil
  | delete k xs  = if (hd xs) = k
                   then (tl xs)
                   else (hd xs)::(delete k (tl xs))

and isValue (Num i) = true
  | isValue (Cnd b) = true
  | isValue (Var x) = false
  | isValue (Plus t1 t2) = if isValue t1 andalso isValue t2
                           then true
                           else false
  | isValue