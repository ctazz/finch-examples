package org.example.catseffect

object Data {

  case class Item(id: Long, desc: String)

  val db: Map[Long, Item] = Vector( Item(1, "x"), Item(2, "y"), Item(3, "w"), Item(4, "z") ).
    foldLeft(Map.empty[Long, Item])( (acc, next) => acc + (next.id -> next))

}
