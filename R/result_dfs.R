result_dfs <- function(rgs, con) {
  lapply(
    rgs$layers,
    function(layer) DBI::dbGetQuery(con, layer$source_sql_query)
  )
}
