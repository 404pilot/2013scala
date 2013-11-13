package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val nonEmptyGenHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, value(empty)), (9, nonEmptyGenHeap))
  } yield insert(n, h)

  lazy val emptyGenheap: Gen[H] = value(empty)

  lazy val genHeap: Gen[H] = frequency((1, emptyGenheap), (19, nonEmptyGenHeap))

  //As long as the implicit arbTree function is in scope, you can now write properties like this:(data: H)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(nonEmptyGenHeap)

  // actions: empty insert meld isEmpty findMin deleteMin

  // 1. empty
  property("emptyHeap is empty") = propBoolean(isEmpty(empty))

  property("emptyHeap insert an element") = forAll { (elem: A) =>
    findMin(insert(elem, empty)) == elem
  }

  property("two emptyHeap meld return emptyHeap") = propBoolean(isEmpty(meld(empty, empty)))

  property("deleteMin Heap(a,empty) should return empty") = forAll { (elem: A) =>
    val dataSet = insert(elem, empty)
    isEmpty(deleteMin(dataSet))
  }

  /*property("emptyHeap deleteMin throw exception") = ???
  property("emptyHeap findMin throw exception") = ???*/

  // 2. empty & nonEmptyHeap
  property("emptyHeap & nonEmptyHeap meld") = forAll { nonEmptyHeap: H =>
    findMin(meld(empty, nonEmptyHeap)) == findMin(nonEmptyHeap)
  }

  // 3. nonEmptyHeap
  property("findMin nonEmptyGenHeap + new element returns correct min value") = forAll { (elem: A, genHeap: H) =>
    val dataSet = insert(elem, genHeap)
    val theMin = math.min(elem, findMin(genHeap))
    findMin(dataSet) == theMin
  }

  property("findMin and deleteMin returns a sorted sequence of elements") = forAll { (data: H) =>
    def _getExpectedSortedList(acc: List[A], testedHeap: H): List[A] = {
      if (isEmpty(testedHeap)) acc
      else _getExpectedSortedList(acc :+ findMin(testedHeap), deleteMin(testedHeap))
    }
    def _isSorted(sample: List[A]): Boolean = sample match {
      case first :: second :: stail => if (first > second) false else _isSorted(sample.tail)
      case _ => true
    }

    lazy val expectedSortedList: List[A] = _getExpectedSortedList(Nil, data)
    _isSorted(expectedSortedList)
  }

  property("meld of heap A and heap B should return the min") = forAll { (a: H, b: H) =>
    val dataSet = meld(a, b)
    val theMin = math.min(findMin(a), findMin(b))
    findMin(dataSet) == theMin
  }

  property("meld of heap A and heap B should have exact same element") = forAll { (a: H, b: H) =>
    def _meld(from: H, to: H): H = {
      if (isEmpty(from)) to
      else _meld(deleteMin(from), insert(findMin(from), to))
    }
    def _isEqual(m: H, n: H): Boolean = {
      lazy val b1 = isEmpty(m)
      lazy val b2 = isEmpty(n)
      if (b1 && b2) true
      else if (b1 != b2) false
      else findMin(m) == findMin(n) && _isEqual(deleteMin(m), deleteMin(n))
    }

    _isEqual(meld(a, b), _meld(a, b))
  }

  /*  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)*/
}