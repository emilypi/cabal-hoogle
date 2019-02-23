object foo {

type M = { type T[+A]; type Ev >: T[Any] <: T[Nothing] }
val M: M = ().asInstanceOf[M]
def ucast(m: M.T[Int]): M.T[Any] = m
def dcast(m: M.T[Any]): M.T[Int] = m: M.Ev
}
