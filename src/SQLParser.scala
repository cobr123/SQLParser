package local.sqlparser

import scala.util.parsing.combinator._

trait DebugJavaTokenParsers extends JavaTokenParsers {

  class Wrap[+T](name: String, parser: Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      val t = parser.apply(in)
      println(name + ".apply for token " + first + " at position " + pos + " offset " + offset + " returns " + t)
      t
    }
  }

  implicit def toWrapped(name: String) = new {
    def !!![T](p: Parser[T]) = new Wrap(name, p) //for debugging
    //def !!![T](p:Parser[T]) = p              //for production
  }
}

class SQLParser extends DebugJavaTokenParsers {

  //A main method for testing
  //  def main(args: Array[String]) = println(parse("select * from dual".toUpperCase()));

  //http://docs.oracle.com/cd/B28359_01/server.111/b28286/statements_10002.htm#i2065706
  def select: Parser[Any] = "select" !!! opt(subquery_factoring_clause) ~ subquery ~ opt(for_update_clause) ~ opt(";")

  def subquery: Parser[Any] = "subquery" !!!
    (query_block
      | (subquery ~ repsep((("UNION" ~ opt("ALL")) | "INTERSECT" | "MINUS") ~ subquery, ","))
      | ("(" ~ subquery ~ ")")
      ) ~ opt(order_by_clause)

  def query_block: Parser[Any] = "query_block" !!!
    "SELECT" ~
      opt(hint) ~
      opt(("DISTINCT" | "UNIQUE") | "ALL") ~
      select_list ~
      opt(into) ~
      "FROM" ~ (repsep(table_reference, ",") | join_clause | ("(" ~ join_clause ~ ")")) ~
      opt(where_clause) ~
      opt(hierarchical_query_clause) ~
      opt(group_by_clause) ~
      opt("HAVING" ~ condition)

  //  opt( model_clause )

  def into: Parser[Any] = "into" !!!
    "INTO" ~ rep1sep(variable_name, ",")

  def variable_name: Parser[Any] = "variable_name" !!! (":" ~ identifier) | identifier

  def for_update_clause: Parser[Any] = "for_update_clause" !!! "FOR" ~ "UPDATE" ~
    opt("OF" ~ repsep(opt(opt(schema ~ ".") ~
      table ~ ".") ~ column, ",")
    ) ~
    opt("NOWAIT" | ("WAIT" ~ integer))

  def integer: Parser[Any] = "integer" !!! wholeNumber

  def keywords: Parser[Any] = "BEGIN" | "END" | "FOR" | "IF" | "THEN" | "CASE" | "WHEN" | "WHERE" | "SELECT" | "UPDATE" | "DELETE" | "INSERT" | "FROM"

  def identifier = not(keywords) ~ ident

  def column: Parser[Any] = "column" !!! identifier

  def table: Parser[Any] = "table" !!! identifier

  def view: Parser[Any] = "view" !!! identifier

  def schema: Parser[Any] = "schema" !!! identifier

  def package_name: Parser[Any] = "package_name" !!! identifier

  def literal: Parser[Any] = "literal" !!! decimalNumber | wholeNumber | identifier

  def number: Parser[Any] = "number" !!! floatingPointNumber

  def otype: Parser[Any] = "otype" !!! identifier

  def c_alias: Parser[Any] = "c_alias" !!! ("\"" + """([^"\p{Cntrl}\\]|\\[\\'"bft]|\\u[a-fA-F0-9]{4})*""" + "\"").r

  def sequence: Parser[Any] = "sequence" !!! identifier

  def subquery_factoring_clause: Parser[Any] = "WITH" ~ repsep(query_name ~ "AS" ~ "(" ~ subquery ~ ")", ",")

  def query_name: Parser[Any] = "query_name" !!! identifier

  def hint: Parser[Any] = "hint" !!! comment

  def comment: Parser[Any] = "comment" !!! commentBlock | lineComment

  def commentBlock: Parser[Any] = """/\*[^(\*\/)]*\*/""".r

  def lineComment: Parser[Any] = """--[^(\r\n)|\n|\r]*""".r


  def select_list: Parser[Any] = "select_list" !!! (
    "*"
      | repsep(expr ~ opt(opt("AS") ~ c_alias), ",")
      | repsep((query_name ~ "." ~ "*"), ",")
    )

  //http://docs.oracle.com/cd/B28359_01/server.111/b28286/expressions.htm
  def expr: Parser[Any] = "expr" !!! (
    function_expression |
//      compound_expression |
      simple_expression
    )

  //      case_expression
  //      cursor_expression |
  //      | datetime_expression

  //      | interval_expression
  //      | object_access_expression
  //      | scalar_subquery_expression
  //      | model_expression
  //      | type_constructor_expression
  //      | variable_expression


  //http://docs.oracle.com/cd/B28359_01/server.111/b28286/expressions002.htm
  def simple_expression: Parser[Any] = "simple_expression" !!! (
    string
      | number
      | comment
      | (opt(opt(schema ~ ".") ~ table ~ ".") ~ column)
    )


  //http://docs.oracle.com/cd/B28359_01/server.111/b28286/expressions003.htm#sthref2739
//  def compound_expression: Parser[Any] = "compound_expression" !!! compound_expression1 | compound_expression2 | compound_expression3
//
//  def compound_expression1: Parser[Any] = "compound_expression1" !!! "(" ~ expr ~ ")"
//
//  def compound_expression2: Parser[Any] = "compound_expression2" !!! ("+" | "-" | "PRIOR") ~ expr
//
//  def compound_expression3: Parser[Any] = "compound_expression3" !!! (expr ~ ("*" | "/" | "+" | "-" | "||") ~ expr)

  //  def expr: Parser[Any] = simpleExpression ~ rep(("=" | "<>" | "<" | "<=" | ">=" | ">" | "in" | "||") ~ simpleExpression)
  //
  //  def simpleExpression: Parser[Any] = term ~ rep(("+" | "-" | "or") ~ term)
  //
  //  def term: Parser[Any] = signedFactor ~ rep(("*" | "/" | "div" | "mod" | "and") ~ signedFactor)
  //
  //  def signedFactor: Parser[Any] = opt("+" | "-" | "PRIOR") ~ factor
  //
  //  def factor: Parser[Any] = ("(" ~ expr ~ ")") | functionDesignator | unsignedConstant | ("NOT" ~ factor) | column
  //
  //  def unsignedConstant: Parser[Any] = string | "NULL"
  //
  //  def functionDesignator: Parser[Any] = identifier ~ "(" ~ rep("," ~ expr) ~ ")"

  def case_expression: Parser[Any] = "case_expression" !!!
    "CASE" ~ (searched_case_expression | simple_case_expression) ~ opt(else_clause) ~ "END"

  def simple_case_expression: Parser[Any] = "simple_case_expression" !!!
    expr ~ rep("WHEN" ~ comparison_expr ~ "THEN" ~ return_expr)

  def searched_case_expression: Parser[Any] = "searched_case_expression" !!!
    rep("WHEN" ~ condition ~ "THEN" ~ return_expr)

  def else_clause: Parser[Any] = "else_clause" !!!
    "ELSE" ~ else_expr

  def comparison_expr: Parser[Any] = "comparison_expr" !!! expr

  def return_expr: Parser[Any] = "return_expr" !!! expr

  def else_expr: Parser[Any] = "else_expr" !!! expr

  def cursor_expression: Parser[Any] = "cursor_expression" !!!
    "CURSOR" ~ "(" ~ subquery ~ ")"

  //  def datetime_expression: Parser[Any] =
  //    expr~ "AT"~
  //      ( "LOCAL"
  //        | ("TIME"~ "ZONE" ( ("'"~ opt( "+" | "-" ) ~hh~":"~mm~"'")
  //          | "DBTIMEZONE"
  //          | "SESSIONTIMEZONE"
  //            | ("'"~time_zone_name~"'")
  //          | expr)
  //        )
  //        )
  //  def hh: Parser[Any] = integer
  //
  //  def mm: Parser[Any] = integer
  //
  //  def time_zone_name: Parser[Any] = identifier

  //http://docs.oracle.com/cd/B28359_01/server.111/b28286/functions001.htm#i88893
  def function_expression: Parser[Any] = "function_expression" !!!
    (/*single_row_function
      | aggregate_function
      |*/ analytic_function
      //      | object_reference_function
      //      | model_function
      | user_defined_function
      )

  def analytic_function: Parser[Any] = "analytic_function" !!!
    analytic_function_name ~ "(" ~ opt(arguments) ~ ")" ~ "OVER" ~ "(" ~ analytic_clause ~ ")"

  def analytic_function_name: Parser[Any] = "analytic_function_name" !!! identifier

  def arguments: Parser[Any] = "arguments" !!! repsep(expr, ",")

  def analytic_clause: Parser[Any] = "analytic_clause" !!!
    opt(query_partition_clause) ~ opt(order_by_clause ~ opt(windowing_clause))

  def windowing_clause: Parser[Any] = "windowing_clause" !!!
    ("ROWS" | "RANGE") ~
      (("BETWEEN" ~
        (("UNBOUNDED" ~ "PRECEDING")
          | ("CURRENT" ~ "ROW")
          | (value_expr ~ ("PRECEDING" | "FOLLOWING"))
          ) ~
        "AND" ~
        (("UNBOUNDED" ~ "FOLLOWING")
          | ("CURRENT" ~ "ROW")
          | (value_expr ~ ("PRECEDING" | "FOLLOWING"))
          ))
        | (("UNBOUNDED" ~ "PRECEDING")
        | ("CURRENT" ~ "ROW")
        | (value_expr ~ "PRECEDING")
        )
        )

  def value_expr: Parser[Any] = "value_expr" !!! simple_expression

  //  def object_reference_function: Parser[Any] =

  //  def model_function: Parser[Any] =

  def user_defined_function: Parser[Any] = "user_defined_function" !!!
    opt(schema ~ ".") ~
      opt(package_name ~ ".") ~
      function ~
      opt("@" ~ dblink) ~
      opt("(" ~ opt(opt("DISTINCT" | "ALL") ~ repsep(expr, ",")) ~ ")")

  def function: Parser[Any] = "function" !!!
    function_name ~ opt("(" ~ opt(function_params) ~ ")")

  def function_name: Parser[Any] = "function_name" !!! identifier

  def function_params: Parser[Any] = "function_params" !!! repsep(expr, ",")


  //  def user_defined_operator: Parser[Any] =

  //  def interval_expression: Parser[Any] =
  //  def object_access_expression: Parser[Any] =
  //  def scalar_subquery_expression: Parser[Any] =
  //  def model_expression: Parser[Any] =
  //  def type_constructor_expression: Parser[Any] =
  //  def variable_expression: Parser[Any] =

  def table_reference: Parser[Any] =
    (("ONLY" ~ "(" ~ query_table_expression ~ ")")
      | query_table_expression
      //      ~opt( pivot_clause | unpivot_clause )
      ) ~
      //   opt( flashback_query_clause )~
      opt(t_alias)

  def t_alias: Parser[Any] = identifier

  //  def flashback_query_clause: Parser[Any] =
  //  opt( "VERSIONS"~ "BETWEEN"~
  //    ( "SCN" | "TIMESTAMP" )~
  //    ( expr | "MINVALUE" )~ "AND"~
  //    ( expr | "MAXVALUE" )~
  //  )~ "AS"~ "OF"~ ( "SCN" | "TIMESTAMP" )~ expr

  def query_table_expression: Parser[Any] = (query_name
    | (opt(schema ~ ".") ~
    (table ~ opt(("PARTITION" ~ "(" ~ partition ~ ")"
      | "SUBPARTITION" ~ "(" ~ subpartition ~ ")"
      )
      ~ opt(sample_clause)
      | opt(sample_clause)
      | ("@" ~ dblink)
    )
      | (view ~ opt("@" ~ dblink))
      ))
    | ("(" ~ subquery ~ opt(subquery_restriction_clause) ~ ")")
    | table_collection_expression
    )

  def partition: Parser[Any] = identifier

  def subpartition: Parser[Any] = identifier

  def sample_clause: Parser[Any] = "SAMPLE" ~ opt("BLOCK") ~
    "(" ~ sample_percent ~ ")" ~ opt("SEED" ~ "(" ~ seed_value ~ ")")

  def sample_percent: Parser[Any] = decimalNumber

  def seed_value: Parser[Any] = integer

  def dblink: Parser[Any] = identifier

  def subquery_restriction_clause: Parser[Any] =
    "WITH" ~ (("READ" ~ "ONLY")
      | ("CHECK" ~ "OPTION" ~ opt("CONSTRAINT" ~ constraint))
      )

  def constraint: Parser[Any] = identifier

  def table_collection_expression: Parser[Any] = "TABLE" ~ "(" ~ collection_expression ~ ")" ~ opt("(" ~ "+" ~ ")")

  /*collection_expression can be a subquery, a column, a function, or a collection constructor.*/
  def collection_expression: Parser[Any] = subquery | column

  def join_clause: Parser[Any] =
    table_reference ~ repsep(inner_cross_join_clause | outer_join_clause, ",")

  def inner_cross_join_clause: Parser[Any] = (
    (opt("INNER") ~ "JOIN" ~ table_reference ~
      (("ON" ~ condition)
        | ("USING" ~ "(" ~ repsep(column, ",") ~ ")")
        ))
      | (("CROSS"
      | "NATURAL" ~ opt("INNER")
      )
      ~ "JOIN" ~ table_reference)
    )

  def outer_join_clause: Parser[Any] = (
    opt(query_partition_clause)
      ~ ((outer_join_type ~ "JOIN")
      | ("NATURAL" ~ opt(outer_join_type) ~ "JOIN")
      )
      ~ table_reference ~ opt(query_partition_clause)
      ~ opt(("ON" ~ condition)
      | ("USING" ~ "(" ~ repsep(column, ",") ~ ")")
    )
    )

  def query_partition_clause: Parser[Any] = (
    "PARTITION" ~ "BY" ~ (repsep(expr, ",") | ("(" ~ repsep(expr, ",") ~ ")"))
    )

  def outer_join_type: Parser[Any] = (
    ("FULL" | "LEFT" | "RIGHT")
      ~ opt("OUTER")
    )

  def where_clause: Parser[Any] =
    "WHERE" ~ condition

  def hierarchical_query_clause: Parser[Any] =
    opt("START" ~ "WITH" ~ condition) ~
      "CONNECT" ~ "BY" ~ opt("NOCYCLE") ~ condition

  def group_by_clause: Parser[Any] = (
    "GROUP" ~ "BY" ~
      repsep(expr
        | rollup_cube_clause
        | grouping_sets_clause
        , ","
      )
      ~ opt("HAVING" ~ condition)
    )

  def condition: Parser[Any] = "condition" !!! (
    comparison_condition
      | floating_point_condition
    //      | logical_condition
    //      | model_condition
    //      | multiset_condition
    //      | pattern_matching_condition
    //      | range_condition
    //      | null_condition
    //      | XML_condition
    //      | compound_condition
    //      | exists_condition
    //      | in_condition
    //      | is_of_type_condition
    )

  def comparison_condition: Parser[Any] = "comparison_condition" !!! simple_comparison_condition | group_comparison_condition

  def simple_comparison_condition: Parser[Any] = "simple_comparison_condition" !!!
    ((expr ~ ("=" | "!=" | "^=" | "<>" | ">" | "<" | ">=" | "<=") ~ expr)
      |
      ("(" ~ repsep(expr, ",") ~ ")" ~ ("=" | "!=" | "^=" | "<>") ~ "(" ~ subquery ~ ")")
      )

  def group_comparison_condition: Parser[Any] = "group_comparison_condition" !!!
    ((expr ~ ("=" | "!=" | "^=" | "<>" | ">" | "<" | ">=" | "<=") ~ ("ANY" | "SOME" | "ALL") ~ "(" ~ (expression_list | subquery) ~ ")")
      |
      ("(" ~ repsep(expr, ",") ~ ")" ~ ("=" | "!=" | "^=" | "<>") ~ ("ANY" | "SOME" | "ALL") ~ "(" ~ (repsep(expression_list, ",") | subquery) ~ ")")
      )

  def floating_point_condition: Parser[Any] = "floating_point_condition" !!!
    expr ~ "IS" ~ opt("NOT") ~ ("NAN" | "INFINITE")

  //  def pattern_matching_condition: Parser[Any] =
  //    char1 ~opt( "NOT" )~ ( "LIKE" | "LIKEC" | "LIKE2" | "LIKE4" )~
  //    char2 ~opt( "ESCAPE"~ esc_char )
  //  def char1: Parser[Any] =
  //  def char2: Parser[Any] =
  //  def esc_char: Parser[Any] =
  /*
  In this syntax:
  char1 is a character expression, such as a character column, called the search value.
  char2 is a character expression, usually a literal, called the pattern.
  esc_char is a character expression, usually a literal, called the escape character.
  */

  //  def range_condition: Parser[Any] =
  //    expr ~ opt("NOT") ~ "BETWEEN" ~ expr ~ "AND" ~ expr
  //
  //  def null_condition: Parser[Any] =
  //    expr ~ "IS" ~ opt("NOT") ~ "NULL"
  //
  //  def XML_condition: Parser[Any] =
  //    "EQUALS_PATH" ~ "(" ~ column ~ "," ~ path_string ~ opt("," ~ correlation_integer) ~ ")"

  //  def path_string: Parser[Any] = string

  def string: Parser[String] = "string" !!!
    """'(('')|[^'])*'""".r

  //  def correlation_integer: Parser[Any] = integer

  //  def compound_condition: Parser[Any] = (
  //    ("(" ~ condition ~ ")")
  //      | ("NOT" ~ condition)
  //      | (condition ~ ("AND" | "OR") ~ condition))
  //
  //  def exists_condition: Parser[Any] =
  //    "EXISTS" ~ "(" ~ subquery ~ ")"
  //
  //  def in_condition: Parser[Any] = (
  //    (expr ~ opt("NOT") ~ "IN" ~ "(" ~ (expression_list | subquery) ~ ")")
  //      | ("(" ~ repsep(expr, ",") ~ ")" ~
  //      opt("NOT") ~ "IN" ~ "(" ~ (repsep(expression_list, ",") | subquery) ~ ")"
  //      ))
  //
  //  def is_of_type_condition: Parser[Any] =
  //    expr ~ "IS" ~ opt("NOT") ~ "OF" ~ opt("TYPE") ~
  //      "(" ~ repsep(opt("ONLY") ~ opt(schema ~ ".") ~ otype, ",") ~ ")"

  //  def model_clause: Parser[Any] =
  //    "MODEL"~
  //      opt( cell_reference_options )~
  //  opt( return_rows_clause )~
  //  opt( reference_model )~
  //  rep( reference_model )~
  //  main_model
  //  def cell_reference_options: Parser[Any] =
  //    opt(("IGNORE" | "KEEP") ~ "NAV") ~
  //      opt("UNIQUE" ~ ("DIMENSION" | ("SINGLE" ~ "REFERENCE")))
  //
  //  def return_rows_clause: Parser[Any] =
  //    "RETURN" ~ ("UPDATED" | "ALL") ~ "ROWS"

  //  def reference_model: Parser[Any] =
  //    "REFERENCE" ~reference_model_name~
  //      "ON" ~"("~subquery~")"~
  //      model_column_clauses~
  //    opt( cell_reference_options )
  //  def main_model: Parser[Any] =
  //  opt( "MAIN" ~main_model_name )~
  //  model_column_clauses~
  //    opt( cell_reference_options )~
  //  model_rules_clause
  def order_by_clause: Parser[Any] =
    "ORDER" ~ opt("SIBLINGS") ~ "BY" ~
      repsep((expr | position | c_alias) ~
        opt("ASC" | "DESC") ~
        opt(("NULLS" ~ "FIRST") | ("NULLS" ~ "LAST"))
        , ","
      )

  //  def reference_model_name: Parser[Any] = identifier

  def position: Parser[Any] = integer

  //  def main_model_name: Parser[Any] = identifier

  //  def model_column_clauses: Parser[Any] =
  //  opt( "PARTITION"~ "BY" ~repsep(expr ~opt(c_alias),",")~)~
  //  "DIMENSION"~ "BY" ~ "("~repsep(expr ~opt(c_alias),",")~")"~
  //  "MEASURES" ~ "("~repsep(expr ~opt(c_alias),",")~")"

  //  def model_column: Parser[Any] = expr ~ opt(opt("AS") ~ c_alias)

  //  def model_rules_clause: Parser[Any] =
  //    opt("RULES" ~
  //      opt(("UPDATE" | ("UPSERT" ~ opt("ALL")))) ~
  //      opt(("AUTOMATIC" | "SEQUENTIAL") ~ "ORDER") ~
  //      opt(model_iterate_clause)
  //    ) ~
  //      "(" ~ repsep(opt(("UPDATE" | ("UPSERT" ~ opt("ALL")))) ~ cell_assignment ~ opt(order_by_clause) ~ "=" ~ expr, ",") ~ ")"

  //  def model_iterate_clause = "ITERATE" ~ "(" ~ number ~ ")" ~ opt("UNTIL" ~ "(" ~ condition ~ ")")

  //  def cell_assignment: Parser[Any] = (
  //    measure_column ~ "[" ~ (repsep(condition
  //      | expr
  //      | single_column_for_loop
  //      , ",")
  //      | multi_column_for_loop
  //      )
  //      ~ "]")

  //  def measure_column: Parser[Any] = column

  //  def single_column_for_loop: Parser[Any] =
  //    "FOR" ~ dimension_column ~
  //      (("IN" ~ "(" ~ (repsep(literal, ",")
  //        | subquery
  //        ) ~
  //        ")")
  //        | opt("LIKE" ~ pattern) ~
  //        "FROM" ~ literal ~ "TO" ~ literal ~
  //        ("INCREMENT" | "DECREMENT") ~ literal
  //        )

  //  def multi_column_for_loop: Parser[Any] =
  //    "FOR" ~ "(" ~ repsep(dimension_column, ",") ~ ")" ~
  //      "IN" ~ "(" ~ (repsep("(" ~ repsep(literal, ",") ~ ")", ",")
  //      | subquery
  //      ) ~
  //      ")"

  //  def dimension_column: Parser[Any] = column

  //  def pattern: Parser[Any] = string

  def rollup_cube_clause: Parser[Any] =
    ("ROLLUP" | "CUBE") ~ "(" ~ grouping_expression_list ~ ")"

  def grouping_sets_clause: Parser[Any] =
    ("GROUPING" ~ "SETS"
      ~ "(" ~ (rollup_cube_clause | grouping_expression_list) ~ ")")

  def grouping_expression_list: Parser[Any] =
    repsep(expression_list, ",")

  def expression_list: Parser[Any] = (
    repsep(expr, ",")
      | ("(" ~ repsep(expr, ",") ~ ")"))

  def parse(p: Parser[Any], sql: String): Option[Any] = {
    parseAll(p, sql) match {
      case Success(r, q) => Option(r)
      case x => println(x); None
    }
  }

  def parse(sql: String): Option[Any] = {
    parseAll(select, sql) match {
      case Success(r, q) => Option(r)
      case x => println(x); None
    }
  }
}

