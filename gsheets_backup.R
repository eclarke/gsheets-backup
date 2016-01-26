
# Libraries ---------------------------------------------------------------

library(googlesheets)
library(plyr)

# Variables ---------------------------------------------------------------

backup.fp <- file.path("~/gsheets_backup")

# Create backup directory if it doesn't exist
if (!dir.exists(backup.fp)) {
  dir.create(backup.fp)
}

# Load sheets. This may prompt for authentication
sheets <- gs_ls()

# Check if the destination file exists, and if it does, compare the mod time the
# last updated time of the google sheet
do_backup <- function(gsheet) {
  fname <- gsheet[['filename']]
  ifelse(file.exists(fname), file.mtime(fname) < as.POSIXct(gsheet[['updated']]), 
         TRUE)
}

# Download a copy of the google sheet if it's been updated since the last backup
register_and_download <- function(gsheet) {
  if (do_backup(gsheet)) {
    try({
      sheet <- gs_key(gsheet[['sheet_key']])
      gs_download(sheet, to=gsheet[['filename']], overwrite=TRUE)
    }, silent = FALSE)
  }
}

# Replaces non-alphanumerics with underscores.
# Stolen from knitr:::sanitize_fn
sanitize_fname <- function(path) 
{
  if (grepl("[^~:_./\\[:alnum:]-]", path)) {
    warning("replaced special characters in filename \"", 
            path, "\" -> \"", path <- gsub("[^~:_./\\[:alnum:]-]", 
                                           "_", path), "\"")
  }
  s = strsplit(path, "[/\\\\]")[[1L]]
  i = (s != ".") & (s != "..") & grepl("\\.", s)
  if (any(i)) {
    s[i] = gsub("\\.", "_", s[i])
    path = paste(s, collapse = "/")
    warning("dots in paths replaced with _ (\"", path, 
            "\")")
  }
  path
}

d_ply(sheets, .(sheet_title), function(ws) {
  
  # Choose a destination filename. If there are duplicate sheet names, append
  # the sheet key to make it unique.
  if (nrow(ws) > 1) {
    ws$filename = paste(ws$sheet_title, ws$sheet_key, "xlsx", sep='.')
  } else {
    ws$filename = paste(ws$sheet_title, "xlsx", sep='.')
  }
  ws$filename <- sapply(ws$filename, sanitize_fname)
  ws$filename <- file.path(backup.fp, ws$filename)
  
  apply(ws, 1, register_and_download)

})



