import java.io.File
import local.sqlparser.SQLParser

/**
 * Created with IntelliJ IDEA.
 * User: r.tabulov
 * Date: 08.07.13
 * Time: 18:22
 * To change this template use File | Settings | File Templates.
 */
object TestSqlParser extends SQLParser with App {
  val file = new File("sqlTests\\Compound Expressions.txt");
  val source = scala.io.Source.fromFile(file)
  val lines = source.mkString.toUpperCase()
  source.close()
  println(parse(lines));
  println(lines);

//  val testDir = new File("C:\\Users\\path\\sqlTests");
//  for (file <- testDir.listFiles.filter(_.getName.endsWith(".txt"))) {
//    val source = scala.io.Source.fromFile(file)
//    val lines = source.mkString.toUpperCase()
//    source.close()
//    parse(lines) match {
//      case None => println("FAIL: " + file.getCanonicalPath)
//      case _ => println("OK  : " + file.getCanonicalPath)
//    }
//
//  }
}
