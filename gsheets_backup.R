
library(googlesheets)
library(plyr)

# Replaces non-alphanumerics with underscores.
# Stolen from knitr:::sanitize_fn
sanitize_fname <- function(path) 
{
  if (grepl("[^~:_./\\[:alnum:]-]", path)) {
    warning("replaced special characters in sheet name \"", 
            path, "\" -> \"", path <- gsub("[^~:_./\\[:alnum:]-]", 
                                           "_", path), "\"")
  }
  path
}

backup.fp <- file.path("~/gsheets_backup")

# Create backup directory if it doesn't exist
if (!dir.exists(backup.fp)) {
  dir.create(backup.fp)
}

# Load sheets. This may prompt for authentication
sheets <- gs_ls()
# Sanitize sheet names
sheets$sanitized_titles <- sanitize_fname(sheets$sheet_title)

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


d_ply(sheets, .(sheet_title), function(ws) {
  
  # Choose a destination filename. If there are duplicate sheet names, append
  # the sheet key to make it unique.
  if (nrow(ws) > 1) {
    ws$sanitized_title = paste(ws$sanitized_title, ws$sheet_key, sep='.')
  }
  
  ws$filename <- file.path(backup.fp, paste0(ws$sanitized_title, '.xlsx'))
  
  apply(ws, 1, register_and_download)

})



