object TestSqlParser extends SQLParser with App {
//  val cmd = """select * from dual t"""
    val cmd = """select * from dual t, dual2 t2"""
  println(parse(cmd.toUpperCase()));
}
