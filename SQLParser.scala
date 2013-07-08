import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._


class SQLParser extends JavaTokenParsers {

  def select: Parser[Any] = subquery ~ opt(for_update_clause)

  def subquery: Parser[Any] =
    opt(subquery_factoring_clause) ~
      "SELECT" ~
      opt(hint) ~
      opt(("DISTINCT" | "UNIQUE")
        | "ALL"

      ) ~
      select_list ~
      "FROM" ~ (repsep(table_reference, ",")
      | join_clause
      | ("(" ~ join_clause ~ ")")
      ) ~
      opt(where_clause) ~
      opt(hierarchical_query_clause) ~
      opt(group_by_clause) ~
      opt("HAVING" ~ condition) ~
      //  opt( model_clause )           ~
      opt(("UNION" ~ opt("ALL")
        | "INTERSECT"
        | "MINUS"
        ) ~
        ("(" ~ subquery ~ ")")
      ) ~
      opt(order_by_clause)


  def for_update_clause: Parser[Any] = "FOR" ~ "UPDATE" ~
    opt("OF" ~ repsep(opt(opt(schema ~ ".") ~
      (table | view) ~ ".") ~ column, ",")
    ) ~
    opt("NOWAIT" | ("WAIT" ~ integer))

  def integer: Parser[Any] = wholeNumber

  def column: Parser[Any] = ident

  def table: Parser[Any] = ident

  def view: Parser[Any] = ident

  def schema: Parser[Any] = ident

  def literal: Parser[Any] = decimalNumber | wholeNumber | ident

  def number: Parser[Any] = floatingPointNumber

  def otype: Parser[Any] = ident

  def c_alias: Parser[Any] = ident

  def sequence: Parser[Any] = ident

  def subquery_factoring_clause: Parser[Any] = "WITH" ~ repsep(query_name ~ "AS" ~ "(" ~ subquery ~ ")", ",")

  def query_name: Parser[Any] = ident

  def hint: Parser[Any] = comment

  def comment: Parser[Any] = mlComment | lineComment

  def mlComment: Parser[Any] = "/*" ~ ".*".r ~ "*/"

  def lineComment: Parser[Any] = "--" ~ ".*".r ~ not("\r\n" | "\n")

  def select_list: Parser[Any] = ("*"
    | (repsep((query_name ~ "." ~ "*")
    | (opt(schema ~ ".") ~
    (table | view) ~ "." ~ "*")
    | (expr ~ opt(opt("AS") ~ c_alias))
    , ","
  ))
    )

  def expr: Parser[Any] = (
    simple_expression
      | compound_expression
      //      | case_expression
      | cursor_expression
    //      | datetime_expression
    //      | function_expression
    //      | interval_expression
    //      | object_access_expression
    //      | scalar_subquery_expression
    //      | model_expression
    //      | type_constructor_expression
    //      | variable_expression
    )

  def simple_expression: Parser[Any] =
    ((opt((query_name ~ ".")
      | (opt(schema ~ ".") ~
      (table ~ "." | view ~ "."))
    ) ~ (column | "ROWID"))
      | "ROWNUM"
      | string
      | number
      | (sequence ~ "." ~ ("CURRVAL" | "NEXTVAL"))
      | "NULL"
      )

  def compound_expression: Parser[Any] =
    (("(" ~ expr ~ ")")
      | (("+" | "-" | "PRIOR") ~ expr)
      | (expr ~ ("*" | "/" | "+" | "-" | "||") ~ expr)
      )

  //  def case_expression: Parser[Any] =  (
  //    "CASE" ~( simple_case_expression
  //      | searched_case_expression
  //    )
  //      ~opt( else_clause )~
  //  "END"  )
  //  def simple_case_expression: Parser[Any] =
  //    expr~
  //    rep( "WHEN"~ comparison_expr ~"THEN"~ return_expr )
  //  def searched_case_expression: Parser[Any] =
  //  rep( "WHEN"~ condition ~"THEN"~ return_expr )
  //  def else_clause: Parser[Any] =
  //    "ELSE"~ else_expr
  //  def comparison_expr: Parser[Any] =
  //  def return_expr: Parser[Any] =
  //  def else_expr: Parser[Any] =
  def cursor_expression: Parser[Any] =
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
  def hh: Parser[Any] = integer

  def mm: Parser[Any] = integer

  def time_zone_name: Parser[Any] = ident

  //  def function_expression: Parser[Any] =
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

  def t_alias: Parser[Any] = ident

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

  def partition: Parser[Any] = ident

  def subpartition: Parser[Any] = ident

  def sample_clause: Parser[Any] = "SAMPLE" ~ opt("BLOCK") ~
    "(" ~ sample_percent ~ ")" ~ opt("SEED" ~ "(" ~ seed_value ~ ")")

  def sample_percent: Parser[Any] = decimalNumber

  def seed_value: Parser[Any] = integer

  def dblink: Parser[Any] = ident

  def subquery_restriction_clause: Parser[Any] =
    "WITH" ~ (("READ" ~ "ONLY")
      | ("CHECK" ~ "OPTION" ~ opt("CONSTRAINT" ~ constraint))
      )

  def constraint: Parser[Any] = ident

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

  def condition: Parser[Any] = (
    /*  comparison_condition
    | */ floating_point_condition
    //      | logical_condition
    //      | model_condition
    //      | multiset_condition
    //      | pattern_matching_condition
    | range_condition
    | null_condition
    | XML_condition
    | compound_condition
    | exists_condition
    | in_condition
    | is_of_type_condition)

  //  def comparison_condition: Parser[Any] =
  def floating_point_condition: Parser[Any] =
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

  def range_condition: Parser[Any] =
    expr ~ opt("NOT") ~ "BETWEEN" ~ expr ~ "AND" ~ expr

  def null_condition: Parser[Any] =
    expr ~ "IS" ~ opt("NOT") ~ "NULL"

  def XML_condition: Parser[Any] =
    "EQUALS_PATH" ~ "(" ~ column ~ "," ~ path_string ~ opt("," ~ correlation_integer) ~ ")"

  def path_string: Parser[Any] = string

  def string: Parser[String] =
    ("'" + """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""" + "'").r

  def correlation_integer: Parser[Any] = integer

  def compound_condition: Parser[Any] = (
    ("(" ~ condition ~ ")")
      | ("NOT" ~ condition)
      | (condition ~ ("AND" | "OR") ~ condition))

  def exists_condition: Parser[Any] =
    "EXISTS" ~ "(" ~ subquery ~ ")"

  def in_condition: Parser[Any] = (
    (expr ~ opt("NOT") ~ "IN" ~ "(" ~ (expression_list | subquery) ~ ")")
      | ("(" ~ repsep(expr, ",") ~
      opt("NOT") ~ "IN" ~ "(" ~ (repsep(expression_list, ",") | subquery) ~ ")"
      ~ ")"
      ))

  def is_of_type_condition: Parser[Any] =
    expr ~ "IS" ~ opt("NOT") ~ "OF" ~ opt("TYPE") ~
      "(" ~ repsep(opt("ONLY") ~ opt(schema ~ ".") ~ otype, ",") ~ ")"

  //  def model_clause: Parser[Any] =
  //    "MODEL"~
  //      opt( cell_reference_options )~
  //  opt( return_rows_clause )~
  //  opt( reference_model )~
  //  rep( reference_model )~
  //  main_model
  def cell_reference_options: Parser[Any] =
    opt(("IGNORE" | "KEEP") ~ "NAV") ~
      opt("UNIQUE" ~ ("DIMENSION" | ("SINGLE" ~ "REFERENCE")))

  def return_rows_clause: Parser[Any] =
    "RETURN" ~ ("UPDATED" | "ALL") ~ "ROWS"

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

  def reference_model_name: Parser[Any] = ident

  def position: Parser[Any] = integer

  def main_model_name: Parser[Any] = ident

  //  def model_column_clauses: Parser[Any] =
  //  opt( "PARTITION"~ "BY" ~repsep(expr ~opt(c_alias),",")~)~
  //  "DIMENSION"~ "BY" ~ "("~repsep(expr ~opt(c_alias),",")~")"~
  //  "MEASURES" ~ "("~repsep(expr ~opt(c_alias),",")~")"

  def model_column: Parser[Any] = expr ~ opt(opt("AS") ~ c_alias)

  def model_rules_clause: Parser[Any] =
    opt("RULES" ~
      opt(("UPDATE" | ("UPSERT" ~ opt("ALL")))) ~
      opt(("AUTOMATIC" | "SEQUENTIAL") ~ "ORDER") ~
      opt(model_iterate_clause)
    ) ~
      "(" ~ repsep(opt(("UPDATE" | ("UPSERT" ~ opt("ALL")))) ~ cell_assignment ~ opt(order_by_clause) ~ "=" ~ expr, ",") ~ ")"

  def model_iterate_clause = "ITERATE" ~ "(" ~ number ~ ")" ~ opt("UNTIL" ~ "(" ~ condition ~ ")")

  def cell_assignment: Parser[Any] = (
    measure_column ~ "[" ~ (repsep(condition
      | expr
      | single_column_for_loop
      , ",")
      | multi_column_for_loop
      )
      ~ "]")

  def measure_column: Parser[Any] = column

  def single_column_for_loop: Parser[Any] =
    "FOR" ~ dimension_column ~
      (("IN" ~ "(" ~ (repsep(literal, ",")
        | subquery
        ) ~
        ")")
        | opt("LIKE" ~ pattern) ~
        "FROM" ~ literal ~ "TO" ~ literal ~
        ("INCREMENT" | "DECREMENT") ~ literal
        )

  def multi_column_for_loop: Parser[Any] =
    "FOR" ~ "(" ~ repsep(dimension_column, ",") ~ ")" ~
      "IN" ~ "(" ~ (repsep("(" ~ repsep(literal, ",") ~ ")", ",")
      | subquery
      ) ~
      ")"

  def dimension_column: Parser[Any] = column

  def pattern: Parser[Any] = string

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


  def parse(sql: String): Option[Any] = {
    parseAll(select, sql) match {
      case Success(r, q) => Option(r)
      case x => println(x); None
    }
  }
}

