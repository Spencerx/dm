#' Load a dm from a remote data source
#'
#' @description
#' `dm_from_con()` creates a [dm] from some or all tables in a [src]
#' (a database or an environment) or which are accessible via a DBI-Connection.
#' For Postgres/Redshift and SQL Server databases, primary and foreign keys
#' are imported from the database.
#'
#' @param con A [`DBI::DBIConnection-class`] or a `Pool` object.
#' @param table_names
#'   A character vector of the names of the tables to include.
#' @param learn_keys
#'   `r lifecycle::badge("experimental")`
#'
#'   Set to `TRUE` to query the definition of primary and
#'   foreign keys from the database.
#'   Currently works only for Postgres/Redshift and SQL Server databases.
#'   The default attempts to query and issues an informative message.
#' @param .names
#'   `r lifecycle::badge("experimental")`
#'
#'   A glue specification that describes how to name the tables
#'   within the output, currently only for MSSQL, Postgres/Redshift and MySQL/MariaDB.
#'   This can use `{.table}` to stand for the table name, and
#'   `{.schema}` to stand for the name of the schema which the table lives
#'   within. The default (`NULL`) is equivalent to `"{.table}"` when a single
#'   schema is specified in `schema`, and `"{.schema}.{.table}"` for the case
#'   where multiple schemas are given, and may change in future versions.
#' @param ... `r lifecycle::badge("experimental")`
#'
#'   Additional parameters for the schema learning query.
#'
#'   - `schema`: supported for MSSQL (default: `"dbo"`), Postgres/Redshift (default: `"public"`), and MariaDB/MySQL
#'     (default: current database). Learn the tables in a specific schema (or database for MariaDB/MySQL).
#'   - `dbname`: supported for MSSQL. Access different databases on the connected MSSQL-server;
#'     default: active database.
#'   - `table_type`: supported for Postgres/Redshift (default: `"BASE TABLE"`). Specify the table type. Options are:
#'     1. `"BASE TABLE"` for a persistent table (normal table type)
#'     2. `"VIEW"` for a view
#'     3. `"FOREIGN TABLE"` for a foreign table
#'     4. `"LOCAL TEMPORARY"` for a temporary table
#'
#' @return A `dm` object.
#'
#' @export
#' @examplesIf dm:::dm_has_financial()
#' con <- dm_get_con(dm_financial())
#'
#' # Avoid DBI::dbDisconnect() here, because we don't own the connection
dm_from_con <- function(
  con = NULL,
  table_names = NULL,
  learn_keys = NULL,
  .names = NULL,
  ...
) {
  stopifnot(is(con, "DBIConnection") || inherits(con, "Pool"))

  check_suggested("dbplyr", "dm_from_con")

  if (inherits(con, "Pool")) {
    con <- pool_con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(pool_con))
  }

  src <- src_from_src_or_con(con)

  if (is.null(learn_keys) || isTRUE(learn_keys)) {
    # FIXME: Try to make it work everywhere
    tryCatch(
      {
        dm_learned <- dm_learn_from_db(con, ...)
        if (is_null(learn_keys)) {
          inform(c(
            "Keys queried successfully.",
            i = "Use `learn_keys = TRUE` to mute this message."
          ))
        }

        if (is_null(table_names)) {
          return(dm_learned)
        }

        tbls_in_dm <- src_tbls_impl(dm_learned)

        if (!all(table_names %in% tbls_in_dm)) {
          abort_tbl_access(setdiff(table_names, tbls_in_dm))
        }
        tbls_req <- intersect(tbls_in_dm, table_names)

        return(dm_learned %>% dm_select_tbl(!!!tbls_req))
      },
      error = function(e) {
        if (isTRUE(learn_keys)) {
          abort_learn_keys(e)
        }
        inform(
          "Keys could not be queried.",
          x = conditionMessage(e),
          i = "Use `learn_keys = FALSE` to mute this message."
        )
        NULL
      }
    )
  }

  if (is_null(table_names)) {
    src_tbl_names <- get_src_tbl_names(src, ..., names = .names)
  } else {
    src_tbl_names <- table_names
    if (is.null(names(src_tbl_names))) {
      names(src_tbl_names) <- src_tbl_names
    }
  }

  tbls <-
    src_tbl_names %>%
    quote_ids(con) %>%
    map(possibly(tbl, NULL), src = src)

  bad <- map_lgl(tbls, is_null)
  if (any(bad)) {
    if (is_null(table_names)) {
      warn_tbl_access(names(tbls)[bad])
      tbls <- tbls[!bad]
    } else {
      abort_tbl_access(names(tbls)[bad])
    }
  }

  new_dm(tbls)
}

#' Load a dm from a remote data source
#'
#' Deprecated  in dm 0.3.0 in favor of [dm_from_con()].
#'
#' @inheritParams dm_from_con
#' @param src A dbplyr source, DBI connection object or a Pool object.
#'
#' @export
#' @keywords internal
dm_from_src <- function(src = NULL, table_names = NULL, learn_keys = NULL,
                        ...) {
  if (is_null(src)) {
    return(empty_dm())
  }

  deprecate_soft("0.3.0", "dm::dm_from_src()", "dm::dm_from_con()")
  dm_from_con(con = con_from_src_or_con(src), table_names, learn_keys, ...)
}

quote_ids <- function(x, con, schema = NULL) {
  if (is.null(con)) {
    return(x)
  }

  if (is_null(schema)) {
    map_if(x, ~ !inherits(.x, "Id"), ~ DBI::Id(table = .x))
  } else {
    map_if(x, ~ !inherits(.x, "Id"), ~ schema_if(rep(schema, length(.x)), .x, con)[[1]])
  }
}

# Errors ------------------------------------------------------------------

abort_learn_keys <- function(parent) {
  abort(error_txt_learn_keys(), class = dm_error_full("learn_keys"), parent = parent)
}

error_txt_learn_keys <- function() {
  c(
    "Failed to learn keys from database.",
    i = "Use `learn_keys = FALSE` to work around, or `dm:::dm_meta()` to debug."
  )
}

abort_tbl_access <- function(bad) {
  dm_abort(
    error_txt_tbl_access(bad),
    "tbl_access"
  )
}

warn_tbl_access <- function(bad) {
  dm_warn(
    c(
      error_txt_tbl_access(bad),
      i = "Set the `table_name` argument to avoid this warning."
    ),
    "tbl_access"
  )
}

error_txt_tbl_access <- function(bad) {
  c(
    glue("Table(s) {commas(tick(bad))} cannot be accessed."),
    i = "Use `tbl(src, ...)` to troubleshoot."
  )
}
