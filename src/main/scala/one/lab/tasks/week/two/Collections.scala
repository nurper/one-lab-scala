package one.lab.tasks.week.two
import java.util.NoSuchElementException

object Collections {
  // getLast(List(1 ,2, 3, 4)) -> 4
  // getLast(List())           -> java.util.NoSuchElementException
  def getLast[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException()
    case x :: tail if tail.isEmpty => x
    case _ :: tail => getLast(tail)
  }

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  def getLastOption[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: tail if tail.isEmpty => Some(x)
    case _ :: tail => getLastOption(tail)
  }

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A = list match {
    case  Nil => throw new NoSuchElementException()
    case _ :: tail if tail.isEmpty => throw new NoSuchElementException()
    case x :: _ :: tail if tail.isEmpty => x
    case _ :: tail => getPreLast(tail)
  }

  // getPreLastOption(List(1 ,2, 3, 4)) -> Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLastOption[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case _ :: tail if tail.isEmpty => None
    case x :: _ :: tail if tail.isEmpty => Some(x)
    case _ :: tail => getPreLastOption(tail)
  }

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](n: Int, list: List[A]): A = list match {
    case Nil => throw new IndexOutOfBoundsException
    case x :: tail if (n==1) => x
    case _ :: tail => getNthElement(n-1, tail)
  }

  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: tail if (n==1) => Some(x)
    case _ :: tail => getNthElementOption(n-1, tail)
  }

  // getLength(List(1,2,3)) -> 3
  // getLength(List())      -> 0
  def getLength[A](list: List[A]): Int = {
    def getLengthIter(acc: Int, list: List[A]): Int = {
      list match {
        case Nil => acc
        case _ :: tail => getLengthIter(acc+1, tail)
      }
    }

    getLengthIter(0, list)
  }

  // getReversedList(List(1,2,3)) -> List(3,2,1)
  def getReversedList[A](list: List[A]): List[A] = {
    def getReversedListIter(acc: List[A], list: List[A]): List [A] = {
      list match {
        case Nil => acc
        case x :: tail => getReversedListIter(x :: acc, tail)
      }
    }

    getReversedListIter(Nil, list)
  }

  // duplicateEveryElement(List(1,2,3)) -> List(1,1,2,2,3,3)
  def duplicateEveryElement[A](list: List[A]): List[A] = {
    def duplicateEveryElemIter(acc: List[A], list: List[A]): List[A] = {
      list match {
        case Nil => acc
        case x :: tail => duplicateEveryElemIter(acc :+ x :+ x, tail)
      }
    }

    duplicateEveryElemIter(Nil, list)
  }
}
