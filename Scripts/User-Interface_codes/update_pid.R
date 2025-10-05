# Define the file path and parameters
library(shiny)

file_path <- "OncoGenAI/running_jobs.csv"
# Read command-line arguments
args <- commandArgs(trailingOnly = TRUE)
print(length(args))
print(args)
# Check if all required arguments are provided
if (length(args) != 2) {
  stop("Usage: Rscript script.R <job_id> <pid>", call. = FALSE)
}


job_id <- args[1]
pid <- args[2]

# Read the CSV file
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Find the column indices
job_id_col <- which(names(df) == "Job_ID")
pid_col <- which(names(df) == "PID")

# Update the status for the specified Job.ID
df[df[, job_id_col] == job_id, pid_col] <- pid

print(df)
print(job_id_col)
print(pid_col)
print(job_id)

# Write the updated data back to the CSV file
write.csv(df, file_path, row.names = FALSE)

cat("Process ID updated successfully.\n")


