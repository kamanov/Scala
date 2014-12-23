object HListExample {

  sealed trait HList{

    def foldl[R](f : (Any, R) => R, start : R) : R
    def foldr[R](f : (Any, R) => R, start : R) : R
    def append[B <: HList](b : B) : HList = foldr[HList]((x, ls) =>  HCons(x, ls), b)
  }

  final case class HCons[H, T <: HList](head: H, tail:T) extends HList {
    def ::[E](v : E) = HCons(v, this)
    override def toString = head + " :: " + tail

    def foldl[R](f : (Any, R) => R, start : R) = tail.foldl(f, f.apply(head, start))
    def foldr[R](f : (Any, R) => R, start : R) = f(head, tail.foldr(f, start))
  }

  final class HNil extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = "Nil"

    def foldl[R](f : (Any, R) => R, start : R) = start
    def foldr[R](f : (Any, R) => R, start : R) = start
  }

  object HList {
    type ::[H, T <: HList] = HCons[H,T]
    val :: = HCons
    val HNil = new HNil
  }

  def main(args: Array[String]) {
    import HList._

    def indexAt2ofT[A, B, T <: HList](x: (A :: B :: T)) = x match {
      case a :: b :: _ => b
    }

    def length(x : HList) : Int = {
      x.foldl((_, acc : Int) => acc + 1, 0)
    }

    val list1 = 1 :: false :: "Hi" :: HNil
    val list2 = true :: "Bye" :: 0 :: HNil
    println(length(list1))
    println(list1.append(list2))

  }
}
