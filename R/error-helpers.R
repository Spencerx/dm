# Cf. https://github.com/cynkra/dm/issues/144 (Review error messages)

# error class generator ---------------------------------------------------

format_msg_and_bullets <- function(bullets) {
  if (length(bullets) <= 1) {
    bullets
  } else {
    paste0(bullets[[1]], "\n", format_error_bullets(bullets[-1]))
  }
}

dm_error <- function(x) {
  paste0("dm_error_", x)
}

dm_error_full <- function(x) {
  c(dm_error(x), "dm_error")
}

dm_abort <- function(bullets, class) {
  abort(
    format_msg_and_bullets(bullets),
    class = dm_error_full(class)
  )
}

dm_warning <- function(x) {
  paste0("dm_warning_", x)
}

dm_warning_full <- function(x) {
  c(dm_warning(x), "dm_warning")
}

dm_warn <- function(bullets, class) {
  warn(
    format_msg_and_bullets(bullets),
    class = dm_warning_full(class)
  )
}

# abort and text for key-helper functions ---------------------------------

abort_not_unique_key <- function(table_name, column_names) {
  abort(error_txt_not_unique_key(table_name, column_names), class = dm_error_full("not_unique_key"))
}

error_txt_not_unique_key <- function(table_name, column_names) {
  glue("({commas(tick(column_names))}) not a unique key of {tick(table_name)}.")
}

# error: is not subset of -------------------------------------------------

abort_not_subset_of <- function(table_name_1, colname_1,
                                table_name_2, colname_2) {
  abort(error_txt_not_subset_of(table_name_1, colname_1, table_name_2, colname_2),
    class = dm_error_full("not_subset_of")
  )
}

error_txt_not_subset_of <- function(table_name_1, colname_1,
                                    table_name_2, colname_2) {
  # taking care of singular/plural of the word "columns" and the corresponding ending of the verb
  plural <- s_if_plural(colname_1)
  glue(
    "Column{plural['n']} ({commas(tick(colname_1))}) of table ",
    "{tick(table_name_1)} contain{plural['v']} values (see examples above) that are ",
    "not present in column{plural['n']} ",
    "({commas(tick(colname_2))}) of table {tick(table_name_2)}."
  )
}

# error sets not equal ----------------------------------------------------

abort_sets_not_equal <- function(error_msgs) {
  abort(error_txt_sets_not_equal(error_msgs), class = dm_error_full("sets_not_equal"))
}

error_txt_sets_not_equal <- function(error_msgs) {
  paste0(error_msgs, collapse = "\n  ")
}

# cardinality check errors ------------------------------------------------

abort_not_bijective <- function(child_table_name, fk_col_name) {
  abort(error_txt_not_bijective(child_table_name, fk_col_name),
    class = dm_error_full("not_bijective")
  )
}

error_txt_not_bijective <- function(child_table_name, fk_col_name) {
  plural <- s_if_plural(fk_col_name)
  glue(
    "1..1 cardinality (bijectivity) is not given: Column{plural['n']} ",
    "({commas(tick(fk_col_name))}) in table ",
    "{tick(child_table_name)} contain{plural['v']} duplicate values."
  )
}

abort_not_injective <- function(child_table_name, fk_col_name) {
  abort(error_txt_not_injective(child_table_name, fk_col_name),
    class = dm_error_full("not_injective")
  )
}

error_txt_not_injective <- function(child_table_name, fk_col_name) {
  plural <- s_if_plural(fk_col_name)
  glue(
    "0..1 cardinality (injectivity from child table to parent table) is not given: ",
    "Column{plural['n']} ({commas(tick(fk_col_name))})",
    " in table {tick(child_table_name)} contain{plural['v']} duplicate values."
  )
}

# errors in fk handling --------------------------------------------------

abort_ref_tbl_has_no_pk <- function(ref_table_name) {
  abort(error_txt_ref_tbl_has_no_pk(ref_table_name),
    class = dm_error_full("ref_tbl_has_no_pk")
  )
}

error_txt_ref_tbl_has_no_pk <- function(ref_table_name) {
  glue(
    "ref_table {tick(ref_table_name)} needs a primary key first. ",
    "Use `dm_enum_pk_candidates()` to find appropriate columns and `dm_add_pk()` to define a primary key."
  )
}

# error helpers for draw_dm -----------------------------------------------

abort_last_col_missing <- function() {
  abort(error_txt_last_col_missing(), class = dm_error_full("last_col_missing"))
}

error_txt_last_col_missing <- function() {
  "The last color can't be missing."
}

# errors in graph-functions -----------------------------------------------

abort_no_cycles <- function(g) {
  shortest_cycle <-
    igraph::girth(g) %>%
    pluck("circle") %>%
    names()
  # add the first element after the last element, so it's more clear that it's a cycle
  shortest_cycle <- paste(c(shortest_cycle, shortest_cycle[1]), collapse = " -> ")
  # FIXME: extract, also identify parallel edges as circles
  abort(error_txt_no_cycles(shortest_cycle), class = dm_error_full("no_cycles"))
}

error_txt_no_cycles <- function(shortest_cycle) {
  c("Cycles in the relationship graph not yet supported.", i = glue::glue("Shortest cycle: {shortest_cycle}"))
}


# error in dm_flatten_to_tbl() ----------------------------------------------

abort_tables_not_reachable_from_start <- function() {
  abort(error_txt_tables_not_reachable_from_start(), class = dm_error_full("tables_not_reachable_from_start"))
}

error_txt_tables_not_reachable_from_start <- function() {
  glue("All selected tables must be reachable from `.start`.")
}


# errors in table surgery -------------------------------------------------

abort_dupl_new_id_col_name <- function(table_name) {
  abort(error_txt_dupl_new_id_col_name(table_name), class = dm_error_full("dupl_new_id_col_name"))
}

error_txt_dupl_new_id_col_name <- function(table_name) {
  glue("`new_id_column` can't have an identical name as one of the columns of {tick(table_name)}.")
}

abort_no_overwrite <- function() {
  fun_name <- as_string(sys.call(-1)[[1]])
  abort(error_txt_no_overwrite(fun_name), class = dm_error_full("no_overwrite"))
}

error_txt_no_overwrite <- function(fun_name) {
  glue("`{fun_name}()` does not support the `overwrite` argument.")
}

abort_update_not_supported <- function() {
  abort(error_txt_update_not_supported(), class = dm_error_full("update_not_supported"))
}

error_txt_update_not_supported <- function() {
  "Updating `dm` objects not supported."
}

# errors when filters are set but they shouldn't be ------------------------------

abort_only_possible_wo_filters <- function(fun_name) {
  abort(error_txt_only_possible_wo_filters(fun_name), class = dm_error_full("only_possible_wo_filters"))
}

error_txt_only_possible_wo_filters <- function(fun_name) {
  glue("You can't call `{fun_name}()` on a `dm` with filter conditions. Consider using `dm_apply_filters()` first.")
}

# no foreign key relation -------------------------------------------------

abort_tables_not_neighbors <- function(t1_name, t2_name) {
  abort(error_txt_tables_not_neighbors(t1_name, t2_name), class = dm_error_full("tables_not_neighbors"))
}

error_txt_tables_not_neighbors <- function(t1_name, t2_name) {
  glue("Tables `{t1_name}` and `{t2_name}` are not directly linked by a foreign key relation.")
}

# `dm_flatten_to_tbl()` and `dm_join_to_tbl()` only supported for parents

abort_only_parents <- function() {
  abort(error_txt_only_parents(), class = dm_error_full("only_parents"))
}

error_txt_only_parents <- function() {
  paste0(
    "When using `dm_join_to_tbl()` or `dm_flatten_to_tbl()` all join partners of table `.start` ",
    "have to be its direct neighbors. For 'flattening' with `left_join()`, `inner_join()` or `full_join()` ",
    "use `dm_flatten_to_tbl(.recursive = TRUE)` as an alternative."
  )
}

# not all tables have the same src ----------------------------------------


abort_not_same_src <- function(dm_bind = FALSE) {
  abort(error_txt_not_same_src(dm_bind), class = dm_error_full("not_same_src"))
}

error_txt_not_same_src <- function(dm_bind = FALSE) {
  if (!dm_bind) {
    "Not all tables in the object share the same `src`."
  } else {
    "All `dm` objects need to share the same `src`."
  }
}

# Something other than tables are put in a `dm` ------------------

abort_what_a_weird_object <- function(class) {
  abort(error_txt_what_a_weird_object(class), class = dm_error_full("what_a_weird_object"))
}

error_txt_what_a_weird_object <- function(class) {
  glue("Don't know how to determine table source for object of class {commas(tick(class))}.")
}

abort_squash_limited <- function() {
  abort(error_txt_squash_limited(), class = dm_error_full("squash_limited"))
}

error_txt_squash_limited <- function() {
  "`dm_flatten_to_tbl(.recursive = TRUE)` only supports join methods `left_join`, `inner_join`, `full_join`."
}

abort_apply_filters_first <- function(join_name) {
  abort(error_txt_apply_filters_first(join_name), class = dm_error_txt_apply_filters_first(join_name))
}

dm_error_txt_apply_filters_first <- function(join_name) {
  dm_error(c(paste0("apply_filters_first_", join_name), "apply_filters_first"))
}

error_txt_apply_filters_first <- function(join_name) {
  glue(
    "`dm_..._to_tbl()` with join method `{join_name}` generally wouldn't ",
    "produce the correct result when filters are set. ",
    "Please consider calling `dm_apply_filters()` first."
  )
}

abort_no_flatten_with_nest_join <- function() {
  abort(error_txt_no_flatten_with_nest_join(), class = dm_error_full("no_flatten_with_nest_join"))
}

error_txt_no_flatten_with_nest_join <- function() {
  paste0(
    "`dm_..._to_tbl()` can't be called with `join = nest_join`, ",
    "see the help pages for these functions. Consider `join = left_join`."
  )
}


# object is not a `dm` (but should be one) --------------------------------
abort_is_not_dm <- function(obj_class) {
  abort(error_txt_is_not_dm(obj_class), class = dm_error_full("is_not_dm"))
}

error_txt_is_not_dm <- function(obj_class) {
  glue("Required class `dm` but instead is {format_classes(obj_class)}.")
}


# local `dm` has no con ---------------------------------------------------
abort_con_only_for_dbi <- function() {
  abort(error_txt_con_only_for_dbi(), class = dm_error_full("con_only_for_dbi"))
}

error_txt_con_only_for_dbi <- function() {
  "A local `dm` doesn't have a DB connection."
}

# when zoomed and it shouldn't be ------------------------------

abort_only_possible_wo_zoom <- function(fun_name) {
  abort(error_txt_only_possible_wo_zoom(fun_name), class = dm_error_full("only_possible_wo_zoom"))
}

error_txt_only_possible_wo_zoom <- function(fun_name) {
  glue(
    "You can't call `{fun_name}()` on a `dm_zoomed`. Consider using one of `dm_update_zoomed()`, ",
    "`dm_insert_zoomed()` or `dm_discard_zoomed()` first."
  )
}

# when not zoomed and it should be ------------------------------

abort_only_possible_w_zoom <- function(fun_name) {
  abort(error_txt_only_possible_w_zoom(fun_name), class = dm_error_full("only_possible_w_zoom"))
}

error_txt_only_possible_w_zoom <- function(fun_name) {
  glue("You can't call `{fun_name}()` on an unzoomed `dm`. Consider using `dm_zoom_to()` first.")
}

# errors for `copy_to.dm()` ----------------------------------------------

abort_only_data_frames_supported <- function() {
  abort("`copy_to.dm()` only supports class `data.frame` for argument `df`", class = dm_error_full("only_data_frames_supported"))
}

abort_one_name_for_copy_to <- function(name) {
  abort(glue("Argument `name` in `copy_to.dm()` needs to have length 1, but has length {length(name)} ({commas(tick(name))})"),
    class = dm_error_full("one_name_for_copy_to")
  )
}

# new table name needs to be unique ---------------------------------------

abort_need_unique_names <- function(duplicate_names) {
  abort(error_txt_need_unique_names(unique(duplicate_names)), class = dm_error_full("need_unique_names"))
}

error_txt_need_unique_names <- function(duplicate_names) {
  glue(
    "Each new table needs to have a unique name. Duplicate new name(s): ",
    "{commas(tick(duplicate_names))}."
  )
}

# lost track of by-column (FK-relation) -----------------------------------

abort_fk_not_tracked <- function(x_orig_name, y_name) {
  abort(error_txt_fk_not_tracked(x_orig_name, y_name), class = dm_error_full("fk_not_tracked"))
}

error_txt_fk_not_tracked <- function(x_orig_name, y_name) {
  glue(
    "The foreign key that existed between the originally zoomed table {tick(x_orig_name)} ",
    "and {tick(y_name)} got lost in transformations. Please explicitly provide the `by` argument."
  )
}

# lost track of PK-column(s) -----------------------------------

abort_pk_not_tracked <- function(orig_table, orig_pk) {
  abort(error_txt_pk_not_tracked(orig_table, orig_pk), class = dm_error_full("pk_not_tracked"))
}

error_txt_pk_not_tracked <- function(orig_table, orig_pk) {
  glue(
    "The primary key column(s) {commas(tick(orig_pk))} of the originally zoomed table {tick(orig_table)} got lost ",
    "in transformations. Therefore it is not possible to use `nest.dm_zoomed()`."
  )
}


# only for local src ------------------------------------------------------

abort_only_for_local_src <- function(src_dm) {
  abort(error_txt_only_for_local_src(format_classes(class(src_dm))), class = dm_error_full("only_for_local_src"))
}

error_txt_only_for_local_src <- function(src_class) {
  glue("`nest_join.dm_zoomed()` works only for a local `src`, not on a database with `src`-class: {src_class}.")
}

# Errors for `pull_tbl.dm()` -----------------------------

abort_no_table_provided <- function() {
  abort(error_txt_no_table_provided(), class = dm_error_full("no_table_provided"))
}

error_txt_no_table_provided <- function() {
  "Argument `table` for `pull_tbl.dm()` missing."
}

abort_table_not_zoomed <- function(table_name, zoomed_tables) {
  abort(error_txt_table_not_zoomed(table_name, zoomed_tables), class = dm_error_full("table_not_zoomed"))
}

error_txt_table_not_zoomed <- function(table_name, zoomed_tables) {
  glue(
    "In `pull_tbl.dm_zoomed`: Table {tick(table_name)} not zoomed, ",
    "zoomed tables: {commas(tick(zoomed_tables))}."
  )
}

abort_not_pulling_multiple_zoomed <- function() {
  abort(error_txt_not_pulling_multiple_zoomed(), class = dm_error_full("not_pulling_multiple_zoomed"))
}

error_txt_not_pulling_multiple_zoomed <- function() {
  "If more than 1 zoomed table is available you need to specify argument `table` in `pull_tbl.dm_zoomed()`."
}

abort_cols_not_avail <- function(wrong_col) {
  abort(error_txt_cols_not_avail(wrong_col), class = dm_error_full("cols_not_avail"))
}

error_txt_cols_not_avail <- function(wrong_col) {
  glue(
    "The color(s) {commas(tick(wrong_col))} are not ",
    "available. Call `dm_get_available_colors()` for possible color names or use hex color codes."
  )
}

abort_only_named_args <- function(fun_name, name_meaning) {
  abort(error_txt_only_named_args(fun_name, name_meaning), class = dm_error_full("only_named_args"))
}

error_txt_only_named_args <- function(fun_name, name_meaning) {
  glue(
    "All `...` arguments to function {tick(paste0(fun_name, '()'))} must be named. ",
    "The names represent {name_meaning}."
  )
}

abort_wrong_syntax_set_cols <- function() {
  abort(error_txt_wrong_syntax_set_cols(), class = dm_error_full("wrong_syntax_set_cols"))
}

error_txt_wrong_syntax_set_cols <- function() {
  "You seem to be using outdated syntax for `dm_set_colors()`, type `?dm_set_colors()` for examples."
}

abort_parameter_not_correct_class <- function(parameter, correct_class, class) {
  abort(
    error_txt_parameter_not_correct_class(
      parameter,
      correct_class,
      class
    ),
    class = dm_error_full("parameter_not_correct_class")
  )
}

error_txt_parameter_not_correct_class <- function(parameter, correct_class, class) {
  glue(
    "Parameter {tick(parameter)} needs to be of class {tick(correct_class)} but is of class {format_classes(class)}."
  )
}

abort_parameter_not_correct_length <- function(parameter, correct_length, parameter_value) {
  abort(
    error_txt_parameter_not_correct_length(
      parameter,
      correct_length,
      parameter_value
    ),
    class = dm_error_full("parameter_not_correct_length")
  )
}

error_txt_parameter_not_correct_length <- function(parameter, correct_length, parameter_value) {
  glue(
    "Parameter {tick(parameter)} needs to be of length {tick(correct_length)} but is ",
    "of length {as.character(length(parameter_value))} ({commas(tick(parameter_value))})."
  )
}

warn_if_arg_not <- function(arg,
                            only_on,
                            arg_name = deparse(substitute(arg)),
                            correct = NULL,
                            additional_msg = "") {
  if (!identical(arg, correct)) {
    only_on_string <- glue::glue_collapse(only_on, sep = ", ", last = " and ")
    dm_warn(
      glue::glue(
        "Argument {tick(arg_name)} ignored: currently only supported for {only_on_string}.",
        if (!is.null(additional_msg)) {
          paste0("\n", additional_msg)
        }
      ),
      class = "arg_not"
    )
  }
  NULL
}

# Errors for schema handling functions ------------------------------------

abort_no_schemas_supported <- function(dbms = NULL, con = NULL) {
  abort(
    error_txt_no_schemas_supported(dbms, con),
    class = dm_error_full("no_schemas_supported")
  )
}

error_txt_no_schemas_supported <- function(dbms, con) {
  if (!is.null(dbms)) {
    glue::glue("The concept of schemas is not supported for DBMS {tick(dbms)}.")
  } else if (!is.null(con)) {
    glue::glue(
      "Currently schemas are not supported for a connection ",
      "of class {tick(class(con))}."
    )
  } else {
    # if local src, `con = NULL`
    "Schemas are not available locally."
  }
}

abort_temporary_not_in_schema <- function() {
  abort(
    "If argument `temporary = TRUE`, argument `schema` has to be `NULL`.",
    class = dm_error_full("temporary_not_in_schema")
  )
}

abort_one_of_schema_table_names <- function() {
  abort(
    "Only one of the arguments `schema` and `table_names` can be different from `NULL`.",
    class = dm_error_full("one_of_schema_table_names")
  )
}

s_if_plural <- function(vec) {
  if (length(vec) > 1) {
    # n = noun, v = verb
    c(n = "s", v = "")
  } else {
    c(n = "", v = "s")
  }
}
