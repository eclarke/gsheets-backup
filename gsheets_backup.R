
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

d_ply(sheets, .(sheet_title), function(ws) {
  
  # Choose a destination filename. If there are duplicate sheet names, append
  # the sheet key to make it unique.
  if (nrow(ws) > 1) {
    ws$filename = paste(ws$sheet_title, ws$sheet_key, "xlsx", sep='.')
  } else {
    ws$filename = paste(ws$sheet_title, "xlsx", sep='.')
  }
  
  ws$filename <- file.path(backup.fp, ws$filename)
  
  apply(ws, 1, register_and_download)

})



