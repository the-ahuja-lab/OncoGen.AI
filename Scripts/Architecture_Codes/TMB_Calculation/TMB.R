# Install required packages if not already installed
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#if (!require("tidyverse", quietly = TRUE))3
#  install.packages("tidyverse")

# Install biomaRt, VariantAnnotation, and genome-specific packages
#BiocManager::install("biomaRt")
#BiocManager::install("VariantAnnotation")
#BiocManager::install("BSgenome.Hsapiens.UCSC.hg19")
#BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
#BiocManager::install("TxDb.Hsapiens.UCSC.hg19.knownGene")
# Load necessary libraries
# Load necessary libraries
library(tidyverse)

# Set a debugging variable
debug_env <- new.env()
debug_env$debug = FALSE

# Get command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if the required number of arguments (4) is provided
if (length(args) != 4) {
  stop("Please provide exactly 4 arguments: VCF file path, gene panel file path, genome assembly version (hg19 or hg38), and VAF cutoff value.")
}

vcf_file_path <- args[1]   # VCF file path
gene_panel_file <- args[2] # Gene panel file path
genome_assembly <- args[3] # Genome assembly version (hg19/hg38)
vaf_cutoff <- as.numeric(args[4]) # VAF cutoff value

# Print the input files, genome assembly, and VAF cutoff for debugging
cat("VCF File Path: ", vcf_file_path, "\n")
cat("Gene Panel File Path: ", gene_panel_file, "\n")
cat("Genome Assembly Version: ", genome_assembly, "\n")
cat("VAF Cutoff: ", vaf_cutoff, "\n")

# Directory path for sourcing other script files (if needed)
directory_path <- "TmbR_scripts"
files.sources <- list.files(directory_path, full.names = TRUE)
# Source each file
sapply(files.sources, function(file) source(file, chdir = TRUE))

# Read VCF file from the provided path
vcf_files <- list(UserVCF = vcf_file_path)

# Read VCF files
vcfs <- readVcfFiles(vcfFiles = vcf_files, assembly = genome_assembly)

# Read the gene panel design file from the provided path
design_file_path <- gene_panel_file

# Read bed file with the panel sequencing design
design <- readDesign(
  filename = design_file_path, 
  assembly = genome_assembly,
  ids = "entrezgene_id"
)

# Apply filters using the provided VAF cutoff to remove synonymous variants and keep others
vcfs_NoSynonymous <- applyFilters(vcfs = vcfs, 
                                  assembly = genome_assembly, 
                                  design = design,
                                  vaf.cutoff = vaf_cutoff,  # Use user-provided VAF cutoff
                                  variantType = c("synonymous"))

# Calculate Tumor Mutation Burden (TMB)
TMB_res <- applyTMB(inputForTMB = vcfs_NoSynonymous, 
                    assembly = genome_assembly)

# Print TMB results
print(TMB_res)
TMB_res$Sample <- NULL
# Save TMB results as a CSV file
#FOR ION TORRENT
output_file <- str_replace(vcf_file_path,".vcf","_TMB.csv")
write.csv(TMB_res, output_file, row.names = FALSE)
cat("TMB results saved to:", output_file, "\n")
#To add for illumina vcf