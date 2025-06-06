# Demo for presentation at BaselR, December 2019

# {dm} facilitates working with multiple tables

options(tibble.print_min = 6)
options(tibble.print_max = 6)
options(rlang_backtrace_on_error = "none")

##
##
##
## Greeting
## --------------------------------------------------------------------
##
##
##

# Teaser
dm::dm_nycflights13(cycle = TRUE) %>%
  dm::dm_draw()

# Poll: Who is familiar with the {dplyr} package
#       (grammar of data manipulation)?

# Poll: Who is familiar with the {dbplyr} package
#       (using {dplyr} with databases)?

# Poll: Who has worked with databases?

# Poll: Who has worked with a software that has
#       a concept of "THE DATASET"?

# Poll: Who uses more than one table/data frame
#       at the same time?

##
##
##
## Why?
## --------------------------------------------------------------------
##
##
##

library(nycflights13)

# Example dataset: tables linked with each other
?flights

flights

##
##
##
## Data model
## --------------------------------------------------------------------
##
##
##

library(tidyverse)
flights_base <-
  flights %>%
  select(year, month, day, carrier, tailnum, origin, dest, time_hour)
flights_base

# `carrier` column also present in `airlines`, this table contains
# additional information
airlines

flights_base %>%
  left_join(airlines)

##
##
##
## Keys
## --------------------------------------------------------------------
##
##
##

# `carrier` is a "primary key" in `airlines`
any(duplicated(airlines$carrier))

# `carrier` is a "foreign key" in `flights` into `airlines`
all(flights$carrier %in% airlines$carrier)

##
##
##
## Single source of truth
## --------------------------------------------------------------------
##
##
##

# Update in one single location
airlines[airlines$carrier == "UA", "name"] <-
  "United broke my guitar"

airlines %>%
  filter(carrier == "UA")

# ...propagates to all related records
flights_base %>%
  left_join(airlines)

##
##
##
## Goal: work with one table
## --------------------------------------------------------------------
##
##
##

flights_base %>%
  left_join(airlines, by = "carrier") %>%
  left_join(planes, by = "tailnum") %>%
  left_join(weather, by = c("origin", "time_hour")) %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  left_join(airports, by = c("dest" = "faa"))

##
##
##
## PROBLEMS (see appendix)
## --------------------------------------------------------------------
##
## - wrong keys
## - data mismatches
## - relationship unclear
## - combinatorial explosion
##
##
##

##
##
##
## Data model object
## --------------------------------------------------------------------
##
##
##

library(dm)

# Compound object: tables, relationships, data
dm_flights <- dm_nycflights13(cycle = TRUE)
dm_flights

dm_flights %>%
  dm_draw()

# Selection of tables
dm_flights %>%
  dm_select_tbl(flights, airlines) %>%
  dm_draw()

dm_flights %>%
  dm_select_tbl(airports, airlines) %>%
  dm_draw()

try(
  dm_flights %>%
    dm_select_tbl(bogus)
)

# Accessing tables
dm_flights %>%
  pull_tbl(airlines)

dm_flights$airlines

# Table names
dm_flights %>%
  src_tbls()

# NB: [, $, [[ and names() also work


##
##
##
##
##
##
## THREE USE CASES:
## --------------------------------------------------------------------
##
## 1. Work with a prepared dm object
## 2. Build a data model for your own data
## 3. Publish a dm to a relational database (and load from it)
##
##
##
##


##
##
##
##
##
##
##
##
##
##
##
##
## USE CASE 1: Work with a prepared dm object
## --------------------------------------------------------------------
##
##
##


##
##
##
##
##
## Joining two tables
## --------------------------------------------------------------------
##
##
##

dm_flights <- dm_nycflights13()
dm_flights %>%
  dm_draw()

dm_flights %>%
  dm_join_to_tbl(airlines, flights)

try(
  dm_flights %>%
    dm_join_to_tbl(airports, airlines)
)


##
##
##
## Joining many tables
## --------------------------------------------------------------------
##
##
##

dm_flights %>%
  dm_flatten_to_tbl(flights)

##
##
##
## Filtering for data models
## --------------------------------------------------------------------
##
##
##

# Filtering on a table returns a dm object
# with
# 1. the filter applied to the table in question
# 2. the filter condition(s) stored
dm_flights %>%
  dm_filter(airlines, name == "Delta Air Lines Inc.")

# the resulting `dm` can then be filtered on another table
dm_flights %>%
  dm_filter(airlines, name == "Delta Air Lines Inc.") %>%
  dm_filter(airports, name != "John F Kennedy Intl")

# ... and stored in another dm variable
delta_non_jfk_january <-
  dm_flights %>%
  dm_filter(airlines, name == "Delta Air Lines Inc.") %>%
  dm_filter(airports, name != "John F Kennedy Intl") %>%
  dm_filter(planes, year < 2000) %>%
  dm_filter(flights, month == 1)
delta_non_jfk_january

# Querying a table applies the filters via semi-joins
# along the FK constraints to the requested table
delta_non_jfk_january %>%
  tbl("planes")

# FIXME: Can this work without applying all filters?

delta_non_jfk_january %>%
  dm_apply_filters() %>%
  dm_join_to_tbl(flights, airlines)

delta_non_jfk_january %>%
  dm_flatten_to_tbl(flights)

##
##
##
## NEW NEW NEW: Data manipulation in a dm
## --------------------------------------------------------------------
##
##
##


# A single table of a `dm` can be activated (or zoomed to),
# and subsequently be manipulated by many {dplyr}-verbs.
# Eventually, either the original table can be updated
# or the manipulated table can be inserted as a new table.


# The print output for a `dm_zoomed` looks very much like that from a normal `tibble`.
dm_flights %>%
  dm_zoom_to(flights)

# Many {dplyr} verbs work on zoomed tables:
dm_flights %>%
  dm_zoom_to(flights) %>%
  mutate(am_pm_dep = if_else(dep_time < 1200, "am", "pm")) %>%
  select(year:dep_time, am_pm_dep, everything())

# Put back into the dm:
dm_flights %>%
  dm_zoom_to(flights) %>%
  mutate(am_pm_dep = if_else(dep_time < 1200, "am", "pm")) %>%
  select(year:dep_time, am_pm_dep, everything()) %>%
  dm_update_zoomed()

# Immutable objects, like in {dplyr}
dm_flights

# Creation of a summary table:
dm_with_summary <-
  dm_flights %>%
  dm_zoom_to(flights) %>%
  count(origin) %>%
  dm_insert_zoomed("origin_count")

dm_with_summary$origin_count

dm_with_summary %>%
  dm_draw()

# All relationships still available in the summary are retained:
dm_flights %>%
  dm_zoom_to(flights) %>%
  count(carrier, origin) %>%
  dm_insert_zoomed("origin_carrier_count") %>%
  dm_draw()


##
##
##
## USE CASE 2: Build a data model for your own data
## --------------------------------------------------------------------
##
##
##


##
##
##
##
##
##
##
## Build up data model from scratch
## --------------------------------------------------------------------
##
##
##


# Use `dm()` with a syntax similar to `tibble()`:
nycflights13_tbl <- dm(airlines, airports, flights, planes, weather)
nycflights13_tbl

nycflights13_tbl %>%
  dm_draw()

# Alternatively, start from an empty `dm`
# and add tables via `dm_add_tbl()`:
dm() %>%
  dm_add_tbl(airlines, airports, flights, planes, weather)

# Tables are not connected yet:
nycflights13_tbl %>%
  dm_draw()

# Adding primary keys:
nycflights13_pk <-
  nycflights13_tbl %>%
  dm_add_pk(planes, tailnum) %>%
  dm_add_pk(airports, faa) %>%
  dm_add_pk(airlines, carrier)

nycflights13_pk %>%
  dm_draw()

# FIXME: Model weak constraints, show differently in diagram (#4)

# Adding foreign keys
nycflights13_fk <-
  nycflights13_pk %>%
  dm_add_fk(flights, tailnum, planes) %>%
  dm_add_fk(flights, origin, airports) %>%
  dm_add_fk(flights, dest, airports) %>%
  dm_add_fk(flights, carrier, airlines, check = TRUE)

nycflights13_fk %>%
  dm_draw()

# Color it!
dm_get_available_colors()

nycflights13_base <-
  nycflights13_fk %>%
  cdm_set_colors(
    airlines = , planes = , weather = , airports = "blue"
  )

nycflights13_base %>%
  dm_draw()


##
##
##
## Use data manipulation to understand and establish relationships
## --------------------------------------------------------------------
##
##
##


# Determine key candidates
zoomed_weather <- dm_zoom_to(nycflights13_base, weather)
zoomed_weather

# `enum_pk_candidates()` works for both `tibbles` and `dm_zoomed`
enum_pk_candidates(zoomed_weather)

enum_pk_candidates(zoomed_weather) %>%
  count(candidate)

# It's tricky:
zoomed_weather %>%
  unite("slot_id", origin, year, month, day, hour, remove = FALSE) %>%
  count(slot_id) %>%
  filter(n > 1)

zoomed_weather %>%
  count(origin, time_hour) %>%
  filter(n > 1)

zoomed_weather %>%
  count(origin, format(time_hour)) %>%
  filter(n > 1)

# This looks like a good candidate:
zoomed_weather %>%
  count(origin, format(time_hour, tz = "UTC")) %>%
  filter(n > 1)

# FIXME: Support compound keys (#3)

# Currently, we need to create surrogate keys:
nycflights13_weather_link <-
  zoomed_weather %>%
  mutate(time_hour_fmt = format(time_hour, tz = "UTC")) %>%
  unite("origin_slot_id", origin, time_hour_fmt) %>%
  # here the original 'weather' table is updated with the manipulated one
  dm_update_zoomed() %>%
  # here we are adding a PK for the "enhanced" weather table
  dm_add_pk(weather, origin_slot_id)

nycflights13_weather_link$weather

nycflights13_weather_link %>%
  dm_draw()

# FIXME: zoom to multiple tables

nycflights13_weather_flights_link <-
  dm_zoom_to(nycflights13_weather_link, flights) %>%
  # same procedure with `flights` table
  mutate(time_hour_fmt = format(time_hour, tz = "UTC")) %>%
  # for flights we need to keep the column `origin`,
  # since it is a FK pointing to `airports`
  unite("origin_slot_id", origin, time_hour_fmt, remove = FALSE) %>%
  select(origin_slot_id, everything(), -time_hour_fmt) %>%
  dm_update_zoomed()

# `dm_enum_fk_candidates()` of a `dm` gives info
# about potential FK columns from one table to another
dm_enum_fk_candidates(nycflights13_weather_flights_link, flights, weather)

# well, it's almost perfect, let's add the FK anyway...

nycflights13_perfect <-
  nycflights13_weather_flights_link %>%
  dm_add_fk(flights, origin_slot_id, weather)

nycflights13_perfect %>%
  dm_draw()

# What are the missings?
nycflights13_perfect %>%
  dm_zoom_to(flights) %>%
  anti_join(weather) %>%
  count(origin_slot_id)

# Create table of aggregates, and insert it into the dm
nycflights13_perfect %>%
  dm_zoom_to(flights) %>%
  count(origin) %>%
  dm_insert_zoomed("flights_agg") %>%
  dm_draw()


##
##
##
## USE CASE 3: Publish a dm to a relational database (and load from it)
## --------------------------------------------------------------------
##
##
##


##
##
##
##
##
## Copy to database
## --------------------------------------------------------------------
##
##
##

# All operations are designed to work locally and on the database
dm_flights_sqlite <-
  dm_flights %>%
  copy_dm_to(
    dbplyr::src_memdb(), .,
    unique_table_names = TRUE, set_key_constraints = FALSE
  )

dm_flights_sqlite

dm_flights_sqlite %>%
  dm_draw()

dm_flights_sqlite %>%
  dm_get_tables() %>%
  map(dbplyr::sql_render)

dm_flights_sqlite %>%
  dm_join_to_tbl(airlines, flights) %>%
  dbplyr::sql_render()

dm_flights_sqlite %>%
  dm_flatten_to_tbl(flights) %>%
  dbplyr::sql_render()

# Filtering on the database
dm_flights_sqlite %>%
  dm_filter(airlines, name == "Delta Air Lines Inc.") %>%
  dm_filter(airports, name != "John F Kennedy Intl") %>%
  dm_filter(flights, day == 1) %>%
  dm_apply_filters_to_tbl(flights)

# ... and the corresponding SQL statement and query plan
dm_flights_sqlite %>%
  dm_filter(airlines, name == "Delta Air Lines Inc.") %>%
  dm_filter(airports, name != "John F Kennedy Intl") %>%
  dm_filter(flights, day == 1) %>%
  dm_apply_filters_to_tbl(flights) %>%
  dbplyr::sql_render()


##
##
##
## Import a dm from a database, including key constraints
## --------------------------------------------------------------------
##
##
##

try({
  con_pq <- DBI::dbConnect(RPostgres::Postgres())

  # FIXME: Schema support

  # Off by default, to ensure that no tables are accidentally deleted
  if (FALSE) {
    walk(
      names(dm_flights),
      ~ DBI::dbExecute(
        con_pq,
        paste0("DROP TABLE IF EXISTS ", ., " CASCADE")
      )
    )
  }

  # Import
  dm_flights_pq <-
    dm_flights %>%
    dm_filter(planes, TRUE) %>%
    dm_filter(flights, month == 1, day == 1) %>%
    copy_dm_to(con_pq, ., temporary = FALSE)

  dm_flights_from_pq <-
    dm_learn_from_db(con_pq)

  dm_flights_from_pq %>%
    dm_draw()
})


##
##
##
##
## THREE USE CASES:
## --------------------------------------------------------------------
##
## 1. Work with a prepared dm object
## 2. Build a data model for your own data
## 3. Publish a dm to a relational database (and load from it)
##
##
##
##


##
##
##
##
##
##
##
##
##
##
##
##
## Appendix
## ====================================================================
##
##
##

##
##
##
## Analogy?
## --------------------------------------------------------------------
##
##
##

# Analogy: parallel vectors
# Multiple parallel vectors can be combined into a data frame.
# Multiple related tables can be combined into a dm.
x <- 1:5
x
y <- x + 1
y
z <- diff(y)
z

try(
  tibble(x = 1:5) %>%
    mutate(y = x + 1) %>%
    mutate(z = diff(y))
)

##
##
##
## Pitfall: wrong keys
## --------------------------------------------------------------------
##
##
##

# Same for airplanes?
planes

flights_base %>%
  left_join(planes)

flights_base %>%
  left_join(planes) %>%
  count(is.na(type))

# Take a closer look at the join
flights_base %>%
  left_join(planes, by = "tailnum")

flights_base %>%
  left_join(planes, by = "tailnum") %>%
  count(is.na(type))

##
##
##
## Pitfall: data mismatches
## --------------------------------------------------------------------
##
##
##

flights_base %>%
  left_join(planes, by = "tailnum") %>%
  group_by(carrier) %>%
  summarize(mismatch_rate = mean(is.na(type))) %>%
  filter(mismatch_rate > 0) %>%
  ggplot(aes(x = carrier, y = mismatch_rate)) +
  geom_col()

##
##
##
## Pitfall: relationship unclear
## --------------------------------------------------------------------
##
##
##

# Same for airports?
airports

try(
  flights_base %>%
    left_join(airports)
)

# Need to specify join variables!
flights_base %>%
  left_join(airports, by = c("origin" = "faa"))

# cleanup
rm(airlines)

##
##
##
## Pitfall: combinatorial explosion
## --------------------------------------------------------------------
##
##
##

# Row identifiers
t1 <- tibble(a = 1, b = letters[1:3])
t1
t2 <- tibble(a = 1, c = 1:2)
t2

# What happens here?
left_join(t1, t2)

# When joining, the column(s) must be unique in at least one
# participating table!

# Ensure uniqueness:
airlines %>%
  count(carrier)

airlines %>%
  count(carrier) %>%
  count(n)

planes %>%
  count(tailnum) %>%
  count(n)

# dm shortcut:
planes %>%
  check_key(tailnum)

try(
  planes %>%
    check_key(engines)
)

airports %>%
  check_key(faa)

# Why is name not a key candidate for airports?
try(
  airports %>%
    check_key(name)
)

# Friendly description
airports %>%
  enum_pk_candidates()

# Cleanup
rm(t1, t2)
