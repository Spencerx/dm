#' Create data model from database constraints
#'
#' @description If there are any permament tables on a DB, a new [`dm`] object can be created that contains those tables,
#' along with their primary and foreign key constraints.
#'
#' Currently this only works with MSSQL and Postgres/Redshift databases.
#'
#' The default database schema will be used; it is currently not possible to parametrize the funcion with a specific database schema.
#'
#' @param dest A `src`-object on a DB or a connection to a DB.
#'
#' @family DB interaction functions
#'
#' @return A [`dm`] object with the tables from the DB and the respective key relations.
#'
#' @noRd
#' @examples
#' if (FALSE) {
#'   src_sqlite <- dplyr::src_sqlite(":memory:", create = TRUE)
#'   iris_key <- mutate(iris, key = row_number())
#'
#'   # setting key constraints currently doesn't work on
#'   # SQLite but this would be the code to set the PK
#'   # constraint on the DB
#'   iris_dm <- copy_dm_to(
#'     src_sqlite,
#'     dm(iris = iris_key),
#'     set_key_constraints = TRUE
#'   )
#'
#'   # and this would be the code to learn
#'   # the `dm` from the SQLite DB
#'   iris_dm_learned <- dm_learn_from_db(src_sqlite)
#' }
#' @autoglobal
dm_learn_from_db <- function(dest, dbname = NA, schema = NULL, name_format = "{table}") {
  # assuming that we will not try to learn from (globally) temporary tables, which do not appear in sys.table
  con <- con_from_src_or_con(dest)
  src <- src_from_src_or_con(dest)

  if (is.null(con)) {
    return()
  }

  info <- dm_meta(con, catalog = dbname, schema = schema)

  df_info <-
    info %>%
    dm_select_tbl(-schemata) %>%
    collect()

  dm_name <-
    df_info$tables %>%
    select(catalog = table_catalog, schema = table_schema, table = table_name) %>%
    mutate(name = glue(!!name_format)) %>%
    pull() %>%
    unclass() %>%
    vec_as_names(repair = "unique")

  from <-
    df_info$tables %>%
    select(catalog = table_catalog, schema = table_schema, table = table_name) %>%
    pmap_chr(~ DBI::dbQuoteIdentifier(con, DBI::Id(...)))

  df_key_info <-
    df_info %>%
    dm_zoom_to(tables) %>%
    mutate(dm_name = !!dm_name, from = !!from) %>%
    dm_update_zoomed() %>%
    dm_zoom_to(columns) %>%
    arrange(ordinal_position) %>%
    select(-ordinal_position) %>%
    left_join(tables) %>%
    dm_update_zoomed() %>%
    dm_select_tbl(constraint_column_usage, key_column_usage, columns, table_constraints)

  table_info <-
    df_key_info %>%
    dm_zoom_to(columns) %>%
    group_by(dm_name, from) %>%
    summarize(vars = list(column_name)) %>%
    ungroup() %>%
    pull_tbl()

  table_info$from_id <- DBI::dbUnquoteIdentifier(con, DBI::SQL(table_info$from))
  tables <- map2(table_info$from_id, table_info$vars, ~ tbl(con, .x, vars = .y))
  names(tables) <- table_info$dm_name

  pks_df <-
    df_key_info %>%
    dm_zoom_to(table_constraints) %>%
    filter(constraint_type == "PRIMARY KEY") %>%
    dm_update_zoomed() %>%
    dm_zoom_to(key_column_usage) %>%
    semi_join(table_constraints) %>%
    anti_join(constraint_column_usage) %>%
    arrange(ordinal_position) %>%
    dm_update_zoomed() %>%
    dm_select_tbl(-table_constraints) %>%
    dm_flatten_to_tbl(key_column_usage, .recursive = TRUE) %>%
    select(constraint_catalog, constraint_schema, constraint_name, dm_name, column_name, is_autoincrement) %>%
    group_by(constraint_catalog, constraint_schema, constraint_name, dm_name) %>%
    summarize(pks = list(tibble(
      column = list(column_name),
      autoincrement = any(as.logical(is_autoincrement))
    ))) %>%
    ungroup() %>%
    select(table = dm_name, pks)

  fks_df <-
    df_key_info %>%
    dm_zoom_to(table_constraints) %>%
    filter(constraint_type == "FOREIGN KEY") %>%
    dm_update_zoomed() %>%
    dm_zoom_to(key_column_usage) %>%
    semi_join(table_constraints) %>%
    left_join(table_constraints, select = c(delete_rule)) %>%
    left_join(columns, select = c(column_name, dm_name, table_catalog, table_schema, table_name)) %>%
    dm_update_zoomed() %>%
    dm_select_tbl(-table_constraints) %>%
    dm_zoom_to(constraint_column_usage) %>%
    #
    # inner_join(): Matching column sometimes not found on Postgres
    inner_join(columns, select = c(column_name, dm_name, table_catalog, table_schema, table_name)) %>%
    #
    dm_update_zoomed() %>%
    dm_select_tbl(-columns) %>%
    dm_rename(constraint_column_usage, constraint_column_usage.table_catalog = table_catalog) %>%
    dm_rename(constraint_column_usage, constraint_column_usage.table_schema = table_schema) %>%
    dm_rename(constraint_column_usage, constraint_column_usage.table_name = table_name) %>%
    dm_rename(constraint_column_usage, constraint_column_usage.column_name = column_name) %>%
    dm_rename(constraint_column_usage, constraint_column_usage.dm_name = dm_name) %>%
    dm_rename(key_column_usage, key_column_usage.table_catalog = table_catalog) %>%
    dm_rename(key_column_usage, key_column_usage.table_schema = table_schema) %>%
    dm_rename(key_column_usage, key_column_usage.table_name = table_name) %>%
    dm_rename(key_column_usage, key_column_usage.column_name = column_name) %>%
    dm_rename(key_column_usage, key_column_usage.dm_name = dm_name) %>%
    # inner_join: Sometimes, constraint_schema is different, https://github.com/cynkra/dm/issues/2228
    dm_flatten_to_tbl(constraint_column_usage, .join = inner_join) %>%
    select(
      constraint_catalog,
      constraint_schema,
      constraint_name,
      ordinal_position,
      delete_rule,
      ref_table = constraint_column_usage.dm_name,
      ref_column = constraint_column_usage.column_name,
      table = key_column_usage.dm_name,
      column = key_column_usage.column_name,
    ) %>%
    arrange(
      constraint_catalog,
      constraint_schema,
      constraint_name,
      ordinal_position,
    ) %>%
    select(-ordinal_position) %>%
    # FIXME: Where to learn this in INFORMATION_SCHEMA?
    group_by(
      constraint_catalog,
      constraint_schema,
      constraint_name,
      ref_table,
    ) %>%
    summarize(fks = list(tibble(
      ref_column = list(ref_column),
      table = if (length(table) > 0) table[[1]] else NA_character_,
      column = list(column),
      on_delete = {
        x <- case_when(
          delete_rule == "CASCADE" ~ "cascade",
          .default = "no_action"
        ) %>% unique()
        if (!is_empty(x) & !is_scalar_character(x)) {
          abort("delete_rule for all fk_cols in one constraint_name should be the same")
        }
        x
      }
    ))) %>%
    ungroup() %>%
    select(-(1:3)) %>%
    group_by(table = ref_table) %>%
    summarize(fks = list(bind_rows(fks))) %>%
    ungroup()

  # FIXME: add uks_df
  def <- new_dm_def(tables, pks_df, fks_df = fks_df)
  dm_from_def(def)
}

schema_if <- function(schema, table, con, dbname = NULL) {
  if (is_null(dbname) || is.na(dbname) || dbname == "") {
    purrr::map2(
      schema, table,
      ~ {
        if (is.na(.x)) {
          DBI::Id(table = .y)
        } else {
          DBI::Id(schema = .x, table = .y)
        }
      }
    )
  } else {
    # 'schema_if()' only used internally (can e.g. be set to default schema beforehand)
    # so IMHO we don't need a formal 'dm_error' here
    if (anyNA(schema)) abort("`schema` must be given if `dbname` is not NULL`.")
    purrr::map2(
      table, schema,
      ~ DBI::Id(catalog = dbname, schema = .y, table = .x)
    )
  }
}
