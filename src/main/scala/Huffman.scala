import scala.collection.mutable

sealed abstract class Tree
case class Node(left: Tree, right: Tree) extends Tree
case class Leaf(c: Char) extends Tree

object Huffman {

  def treeOrdering(m: mutable.Map[Tree, Int]) = new Ordering[Tree] {
    def compare(x: Tree, y: Tree) = m(y).compare(m(x))
  }

  def frequencyByChar(text: String): Map[Tree, Int] = text groupBy (x => Leaf(x) : Tree) mapValues (_.length)

  def buildNode(queue: mutable.PriorityQueue[Tree], map: mutable.Map[Tree,Int]) {
    val right = queue.dequeue()
    val left = queue.dequeue()
    val node = Node(left, right)
    map(node) = map(left) + map(right)
    queue.enqueue(node)
  }

  def buildMappingTable(tree: Tree, map: mutable.Map[Tree, Int]): List[(Char, String)] = {
    def recurse(tree: Tree, prefix: String): List[(Char, String)] = tree match {
      case Node(left, right) => recurse(left, prefix+"0") ::: recurse(right, prefix+"1")
      case leaf @ Leaf(c) => (c, prefix) :: Nil
    }
    recurse(tree, "")
  }

  def buildMappingTable(text: String): Map[Char, String] = {
    val map = mutable.Map.empty[Tree,Int] ++= frequencyByChar(text)
    val queue = new mutable.PriorityQueue[Tree]()(treeOrdering(map)) ++= map.keysIterator
    while(queue.size > 1) {
      buildNode(queue, map)
    }
    buildMappingTable(queue.dequeue(), map).toMap
  }

  def main(args: Array[String]): Unit = {
    val initialValue = "initial value"

    val mappingTable = buildMappingTable(initialValue)
    println(s"Mapping table: ${mappingTable}")

    val encoded = initialValue.map(c => mappingTable(c)).mkString("")
    println(encoded)

  }

}
