let mapi f s : 'a IStream.t =
  let rec mapi_node f i = function
    | IStream.Nil -> IStream.Nil
    | IStream.Cons (x, s) -> Cons (f i x, mapi f (i + 1) s)
  and mapi f i s = IStream.thunk (fun () -> mapi_node f i (IStream.peek s))
  in mapi f 0 s

let rec to_list n s = match n, IStream.peek s with
  | _, Nil | 0, _ -> []
  | n, Cons (x, s) -> x :: (to_list (n - 1) s)

exception PredictionsEnd
exception DepthEnd

module type MonadNotations = sig
  include Monad.Def
  type 'a map = 'a -> 'a t
  val id : 'a map
  val (<*>) : ('a -> 'b) t -> 'a -> 'b t
  val (let+) : 'a t -> ('a -> 'b) -> 'b t
  val (and+) : 'a t -> 'b t -> ('a * 'b) t
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
end

module WithMonadNotations (M : Monad.Def) = struct
  include M
  let id = return
  let (<*>) f x     = f >>= fun f -> return (f x)
  let (let+) x f    = map f x
  let (and+) o1 o2  = o1 >>= fun o1 -> o2 >>= fun o2 -> return (o1, o2)
  let (let*) x f    = x >>= f
end

module IdentityMonad = struct
  type 'a t = 'a
  let return x = x
  let (>>=) x f = f x
  let (>>) () x = x
  let map f x = f x
end
