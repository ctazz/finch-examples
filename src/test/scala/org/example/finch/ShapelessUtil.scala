package org.example.finch

import shapeless.{Inl, Inr}

import scala.reflect.ClassTag

object ShapelessUtil {

  def evalCoproduct[A](o: Any)(implicit EV: ClassTag[A]): Option[A] = o match {
    case Inl(a) => evalCoproduct(a)
    case Inr(a) => evalCoproduct(a)
    case Some(a) => evalCoproduct(a) //This lets us handle the case where someone passes in an Option[A]
    case a: A => Some(a)
    case _ => None
  }

}
