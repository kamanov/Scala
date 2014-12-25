object HListExample {

  sealed trait HList{
    import HList._
    def fold[R](f : (Any, R) => R, start : R) : R

    type RevListAppend[T <: HList] <: HList
    def reverseSelfAndAppend[T <: HList](li : T) : RevListAppend[T]
    def reverse() = this.reverseSelfAndAppend(HNil)
    def append[A <: HList](ls : A) = reverse().reverseSelfAndAppend(ls)
  }

  final case class HCons[H, T <: HList](head: H, tail:T) extends HList {
    def ::[E](v : E) = HCons(v, this)
    override def toString = head + " :: " + tail

    def fold[R](f : (Any, R) => R, start : R) = tail.fold(f, f.apply(head, start))

    type RevListAppend[Q <: HList] = T#RevListAppend[HCons[H, Q]]
    def reverseSelfAndAppend[Q <: HList](ls : Q) = tail.reverseSelfAndAppend(HCons(head, ls))

  }

  final class HNil extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = "Nil"

    def fold[R](f : (Any, R) => R, start : R) = start

    type RevListAppend[Q <: HList] = Q
    def reverseSelfAndAppend[Q <: HList](ls : Q) = ls
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
      x.fold((_, acc : Int) => acc + 1, 0)
    }

    def f[T](v: T) = v match {
      case _: Int    => "Int"
      case _: String => "String"
      case _: Boolean => "Bool"
      case _         => "Unknown"
    }

    val list1 = 1 :: false :: "Hi" :: HNil
    val list2 = true :: "Bye" :: 0 :: true :: HNil
    println(f(indexAt2ofT(list2.append(list1))))
    println(length(list1.append(list2)))
    println(list1.append(list2))

  }
}
