import java.io.File
import java.nio.charset.StandardCharsets

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

sealed abstract class Tree {
  def nbOccurrences: Int
}
case class Node(left: Tree, right: Tree, nbOccurrences: Int) extends Tree
case class Leaf(symbol: String, nbOccurrences: Int) extends Tree

object Huffman {

  def nbOccurrencesBySymbol(text: String, nbChars: Int): Map[String, Int] = {
    (0 to text.length - nbChars)
      .map(i => text.substring(i, i+nbChars))
      .groupBy(identity)
      .mapValues(_.length)
  }

  def buildNode(queue: mutable.PriorityQueue[Tree]) {
    val right = queue.dequeue()
    val left = queue.dequeue()
    val node = Node(left, right, left.nbOccurrences + right.nbOccurrences)
    queue.enqueue(node)
  }

  def buildMappingTable(tree: Tree): List[(String, String)] = {
    def recurse(tree: Tree, prefix: String): List[(String, String)] = tree match {
      case Node(left, right, _) => recurse(left, prefix+"0") ::: recurse(right, prefix+"1")
      case leaf @ Leaf(symbol, _) => (symbol, prefix) :: Nil
    }
    recurse(tree, "")
  }

  def buildTree(symbolAndNbOccurences: Iterable[(String, Int)]): Tree = {
    val leafs: Iterable[Leaf] = symbolAndNbOccurences.map(tuple => Leaf(tuple._1, tuple._2))

    val treeOrdering = new Ordering[Tree] {
      def compare(x: Tree, y: Tree) = y.nbOccurrences.compareTo(x.nbOccurrences)
    }
    val queue = new mutable.PriorityQueue[Tree]()(treeOrdering) ++= leafs
    while (queue.size > 1) {
      buildNode(queue)
    }
    queue.dequeue()
  }

  def decode(encoded: String, tree: Tree): String = {
    def recurse(encoded: List[Char], decoded: mutable.ListBuffer[String], cursor: Tree) {
      cursor match {
        case Leaf(symbol, _) => {
          decoded += symbol
          if (encoded != Nil) {
            recurse(encoded.tail, decoded, tree)
          }
        }
        case Node(left, right, _) => {
          val nextNode = if (encoded.head == '0') { left } else { right }
          recurse(encoded.tail, decoded, nextNode)
        }
      }
    }
    val decoded = new ListBuffer[String]
    recurse(encoded.toList, decoded, tree)
    decoded.mkString
  }

  def main(args: Array[String]): Unit = {
    val text = Source.fromFile(new File("/home/francois/Desktop/melville-moby-106.txt")).mkString
    val textSizeInBits = text.getBytes(StandardCharsets.UTF_8).size * 8

    val symbolAndNbOccurences: Iterable[(String, Int)] = (1 to 4)
        .par
        .map(nbChars => {
          if (nbChars == 1) {
            nbOccurrencesBySymbol(text, nbChars)
          } else {
            nbOccurrencesBySymbol(text, nbChars).filter(_._2 > 2)
          }
        })
        .reduce(_ ++ _)

    val tree = buildTree(symbolAndNbOccurences)
    val mappingTable: Map[String, String] = buildMappingTable(tree).toMap

    val encoded = text.map(symbol => mappingTable(symbol.toString)).mkString("|")
    println(s"Encoded : ${encoded.substring(0, 500)}")
    println(s"Decoded value length in bits: $textSizeInBits")
    println(s"Encoded value length in bits: ${encoded.length}")
    println(s"Compression ratio: ${encoded.length / textSizeInBits.toDouble}")

    val decoded = decode(encoded, tree)
    println(s"${decoded.substring(0, 500)}")
  }

}
