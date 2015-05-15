import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

  def buildMappingTable(tree: Tree): List[(Char, String)] = {
    def recurse(tree: Tree, prefix: String): List[(Char, String)] = tree match {
      case Node(left, right) => recurse(left, prefix+"0") ::: recurse(right, prefix+"1")
      case leaf @ Leaf(c) => (c, prefix) :: Nil
    }
    recurse(tree, "")
  }

  def buildTree(text: String): Tree = {
    val map = mutable.Map.empty[Tree, Int] ++= frequencyByChar(text)
    val queue = new mutable.PriorityQueue[Tree]()(treeOrdering(map)) ++= map.keysIterator
    while (queue.size > 1) {
      buildNode(queue, map)
    }
    queue.dequeue()
  }

  def decode(encoded: String, tree: Tree): String = {
    def recurse(encoded: List[Char], decoded: mutable.ListBuffer[Char], fullTree: Tree, cursor: Tree) {
      cursor match {
        case Leaf(c) => {
          decoded += c
          if (encoded != Nil) {
            recurse(encoded, decoded, fullTree, fullTree)
          }
        }
        case Node(left, right) => {
          val nextNode = if (encoded.head == '0') { left } else { right }
          recurse(encoded.tail, decoded, fullTree, nextNode)
        }
      }
    }
    val decoded = new ListBuffer[Char]
    recurse(encoded.toList, decoded, tree, tree)
    decoded.mkString
  }

  def main(args: Array[String]): Unit = {
    val text = "I really like a pint of Guinness"

    val tree = buildTree(text)
    val mappingTable: Map[Char, String] = buildMappingTable(tree).toMap

    println(s"Mapping table: ${mappingTable}")

    val encoded = text.map(c => mappingTable(c)).mkString
    println(encoded)
    println(s"Encoded value length: ${encoded.length}")

    val decoded = decode(encoded, tree)
    println(s"$decoded")
  }

}
