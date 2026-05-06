#'Internal function : Connect R with the sandbox.
#'
#' @importFrom DBI dbConnect
#'
#' @export

get_connection_db <- function()
{
  con <- dbConnect(
    Postgres(),
    dbname = Sys.getenv("STATSDB_DATABASE"),
    host = Sys.getenv("STATSDB_HOST"),
    port = Sys.getenv("STATSDB_PORT"),
    user = Sys.getenv("STATSDB_USER"),
    password = Sys.getenv("STATSDB_PASSWORD")
  )

  return(con)
}
