# Define the file path and parameters
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny", repos = "https://cloud.r-project.org")
}
library(shiny)

file_path <- "OncoGenAI/running_jobs.csv"
# Read command-line arguments
args <- commandArgs(trailingOnly = TRUE)
# print(length(args))
# print(args)
# Check if all required arguments are provided
if (length(args) != 2) {
  stop("Usage: Rscript script.R <job_id> <new_status>", call. = FALSE)
}


job_id <- args[1]
new_status <- args[2]

# Read the CSV file
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Find the column indices
job_id_col <- which(names(df) == "Job_ID")
status_col <- which(names(df) == "Status")

# Update the status for the specified Job.ID
df[df[, job_id_col] == job_id, status_col] <- new_status

# print(df)
# print(df[, job_id_col] == as.integer(job_id_col))
# print(job_id_col)
# print(status_col)
# print(job_id)

# Write the updated data back to the CSV file
write.csv(df, file_path, row.names = FALSE)

cat("Status updated successfully.\n")


