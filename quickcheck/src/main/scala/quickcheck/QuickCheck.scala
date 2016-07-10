package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // hint 1
  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") =
    forAll { (n1: A, n2: A) =>
      findMin(insert(n2, insert(n1, empty))) == Math.min(n1, n2)
    }

  // hint 2
  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") =
    forAll { (n1: A) =>
      isEmpty(deleteMin(insert(n1, empty)))
    }

  // hint 3
  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") =
    forAll { (h: H) =>
      isSorted(h)
    }

  // hint 4
  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") =
    forAll { (n1: A, n2: A, n3: A, n4: A) =>
      findMin(meld(insert(n2, insert(n1, empty)), insert(n4, insert(n3, empty)))) == Math.min(n1, Math.min(n2, Math.min(n3, n4)))
    }

  // one heap create from merging two heap is equal to another heap created from merging such two heap that one is created by removing min
  // element which is added to the second one.
  property("Two heap should be equal when removing min element recursively until both heaps become empty.") = forAll { (h1: H, h2: H) =>

    def isEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else findMin(h1) == findMin(h2) && isEqual(deleteMin(h1), deleteMin(h2))

    isEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))

  }

  def isSorted(h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      val min = findMin(h)
      val rest = deleteMin(h)
      isEmpty(rest) || (min <= findMin(rest) && isSorted(rest))
    }
  }

}
