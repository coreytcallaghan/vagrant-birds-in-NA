library(RSQLite)
library(lubridate)
library(dplyr)
library(readr)

txt_to_sqlite <- function(txt_file, sqlite_file, table_name, 
                          pre_process_size = 1000, chunk_size = 50000) {
  
  con <- dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
  
  # read an extract of the data to extract the colnames and types
  # to figure out the date and datetime columns
  df <- read_delim(txt_file, "\t", escape_double = FALSE, trim_ws = TRUE, n_max = pre_process_size)
  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("/", "_", colnames(df))
  date_cols <- df %>% 
    select_if(lubridate::is.Date) %>% 
    colnames()
  datetime_cols <- df %>% 
    select_if(lubridate::is.POSIXt) %>% 
    colnames()    
  # write this first batch of lines to SQLITE table, converting dates to string representation
  df[ , date_cols] <- as.character.Date(df[ , date_cols])
  df[ , datetime_cols] <- as.character.POSIXt(df[ , datetime_cols])
  dbWriteTable(con, table_name, as.data.frame(df), overwrite=TRUE)

  # subfunction that appends new sections to the table
  append_to_sqlite <- function(x, pos) {
    x <- as.data.frame(x)
    x[ , date_cols] <- as.character.Date(x[ , date_cols])
    x[ , datetime_cols] <- as.character.POSIXt(x[ , datetime_cols])
    dbWriteTable(con, table_name, x, append = TRUE)
  }
  
  # readr chunk functionality
  read_delim_chunked(txt_file, append_to_sqlite, delim = "\t",
                     skip = pre_process_size+1, col_names = colnames(df), escape_double=FALSE, trim_ws=TRUE,
                     col_types = spec(df), chunk_size = chunk_size,
                     progress = FALSE)
  dbDisconnect(con)
}


sqlite_file <- "canada-2016.sqlite"
table_name <- "canada-2016"

txt_to_sqlite(paste("Data/Canada_2016_relMay-2017.txt", sep="\t"), 
              sqlite_file, table_name, pre_process_size = 1000, 
              chunk_size = 50000)

sqlite_file <- "Data/usa-2016.sqlite"
table_name <- "usa-2016"

txt_to_sqlite(paste("Data/USA_2016_relMay-2017.txt", sep="\t"), 
              sqlite_file, table_name, pre_process_size = 1000, 
              chunk_size = 50000)





