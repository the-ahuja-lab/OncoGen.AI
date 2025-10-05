print("merging started")
required_packages <- c(
  "data.table", "tidyr", "stringr", "dplyr",
  "cli", "glue", "Rcpp", "tibble", "pkgconfig",
  "rlang", "lifecycle", "magrittr", "ellipsis"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(data.table)
library(data.table)
library(tidyr)
library(stringr)
library(dplyr)



# Loop over each ID
# for (job_id in ids) {
# OncoGenAI/results/Oncogen202410130001/Oncogen202410130001
# job_id= "Oncogen202409020001"
# Perform any operation with the current job_id
#print(paste("Processing job:", job_id))
# genome="hg19"
args <- commandArgs(trailingOnly = TRUE)
job_id= args[1]
# Perform any operation with the current job_id
#print(paste("Processing job:", job_id))
genome=args[2]
# Perform any operation with the current job_id
#print(paste("Processing job:", job_id))




folder_path <- paste0("OncoGenAI/results/", job_id, "/")
file_path <- paste0( folder_path,job_id,"_bed_filtered.merged2.AFadded.csv")
vcf <- read.csv(file_path,sep="\t")

vcf <- vcf %>%
  separate(RSID, into = c("rsID_1", "rsID_2"), sep = ";", fill = "right")

vcf_data_full <- vcf
# Function to determine if a variant is an SNP (both ref and ALt are single bases)
is_snp <- function(ref, ALt) {
  nchar(ref) == 1 & nchar(ALt) == 1
}

# Split into SNP and Indel dataframes
vcf_data_snp <- vcf_data_full[is_snp(vcf_data_full$Ref, vcf_data_full$ALt), ]
head(vcf_data_snp)
indel_vcf_data <- vcf_data_full[!is_snp(vcf_data_full$Ref, vcf_data_full$ALt), ]
head(indel_vcf_data)


vcf_data_snp <- vcf_data_snp %>%
  mutate(
    # Format ID column
    formatted_ID = paste0( CHRM, "_", LOC, "_", Ref, "/", ALt),
    # Replace ':' with '_' in the RSID column
  )


# indel 
head(indel_vcf_data)
indel_vcf_data <- indel_vcf_data %>%
  mutate(
    # Increment the position by 1
    LOC = ifelse(nchar(Ref) > nchar(ALt), LOC + 1, LOC),
    
    larger_allele = ifelse(nchar(Ref) > nchar(ALt), Ref, ALt),
    smaller_allele = ifelse(nchar(Ref) > nchar(ALt), ALt, Ref),
    
    # Modify alleles
    ref_allele_modified = ifelse(larger_allele == Ref, substr(larger_allele, 2, nchar(larger_allele)), if_else(smaller_allele != "*" ,"-","*")),
    ALt_allele_modified = ifelse(larger_allele == ALt, substr(larger_allele, 2, nchar(larger_allele)), if_else(smaller_allele != "*" ,"-","*")),
    # ref_allele_modified = ifelse(larger_allele == Ref & , substr(larger_allele, 2, nchar(larger_allele)), Ref),
    # ALt_allele_modified = ifelse(larger_allele == ALt & ALt != "*", substr(larger_allele, 2, nchar(larger_allele)), ALt),

    
    # Create the ID column in the desired format
    formatted_ID = paste0(CHRM, "_", LOC, "_", ref_allele_modified, "/", ALt_allele_modified),
  ) 



# colnames(vcf_data_snp)

# Get the common columns between the two dataframes
common_columns <- intersect(colnames(indel_vcf_data), colnames(vcf_data_snp))

# Subset both dataframes to keep only the common columns
indel_vcf_data_common <- indel_vcf_data[, common_columns]
vcf_data_snp_common <- vcf_data_snp[, common_columns]

# Combine the two dataframes using rbind
all_var <- rbind(indel_vcf_data_common, vcf_data_snp_common)



file_path <- paste0(folder_path, job_id,  "_bed_filtered.merged-vep.vcf")
# library(data.table)
vep_data  <- fread(cmd = paste("grep -v '^##' ", file_path), sep = "\t", header = T, fill = TRUE)
dim(vep_data)




file_path <- paste0(folder_path, job_id, "_merged.", genome, "_multianno.txt.intervar")
anotated_acmg <- read.csv(file_path, sep = "\t")
head(anotated_acmg)
dim(anotated_acmg)



file_path <- paste0(folder_path, job_id, "_merged.", genome, "_multianno.txt")
anotated_acmg2 <- read.csv(file_path, sep = "\t")
head(anotated_acmg2)
dim(anotated_acmg)






anotated_acmg$ID <- paste(anotated_acmg$X.Chr, anotated_acmg$Start, anotated_acmg$Ref, anotated_acmg$Alt, sep = "_")
anotated_acmg$ID <- paste("chr",anotated_acmg$ID,sep = "")


anotated_acmg2$ID <- paste(anotated_acmg2$Chr, anotated_acmg2$Start, anotated_acmg2$Ref, anotated_acmg2$Alt, sep = "_")
# Create an ID column in anotated_acmg2 by merging the relevant columns with '_'
# anotated_acmg2$ID <- paste(sub("^chr", "", anotated_acmg2$Chr), anotated_acmg2$Start, anotated_acmg2$End, anotated_acmg2$Ref, anotated_acmg2$Alt, sep = "_")
anotated_acmg <- anotated_acmg %>% distinct(ID, .keep_all = TRUE)
anotated_acmg2 <- anotated_acmg2 %>% distinct(ID, .keep_all = TRUE)

merged_intervar_data <- merge(anotated_acmg, anotated_acmg2, by = "ID", all = TRUE)

nrow(merged_intervar_data)

# Merge the two data frames by ID
merged_intervar_data2 <-  merge(anotated_acmg, anotated_acmg2, by = "ID")


write.csv(merged_intervar_data2,"test.csv")
nrow(anotated_acmg)
nrow(anotated_acmg2)
nrow(merged_intervar_data2)

# dim(merged_df_vcf_vep)
# head(merged_df_vcf_vep)

# head(merged_intervar_data)
# colnames(merged_df_vcf_vep)

# anotated_acmg2$ID[anotated_acmg2$ID  ==  "chr1_103005901_CAT_-"]
# anotated_acmg$ID[anotated_acmg$ID  ==  "chr1_103005901_CAT_-"]

# merged_intervar_data[merged_intervar_data$ID == "chr1_103005901_CAT_-",,drop=F ]$ID

# merged_df_vcf_vep$CHRM[ is.na(merged_df_vcf_vep$formatted_ID)]
# nrow(merged_df_vcf_vep)
# head(merged_df_vcf_vep)
# colnames(merged_df_vcf_vep_intervar)
# merged_df_vcf_vep_intervar$avsnp147.y[is.na(merged_df_vcf_vep_intervar$avsnp147.y)]

merged_df_vcf_vep<- merge(all_var, vep_data , by.x = "rsID_1", by.y = "#Uploaded_variation", all.y = TRUE)
colnames(merged_df_vcf_vep)[which(names(merged_df_vcf_vep) == "rsID_1")] <- "Upload_variation"
merged_df_vcf_vep$formatted_ID <- gsub("/", "_", merged_df_vcf_vep$formatted_ID)


merged_intervar_data <- merged_intervar_data %>%
  mutate(location = paste0("chr", X.Chr, ":", Start.x, "-", End.x))

find_position <- function(element, vec) {
  match(element, vec)
}

positions_in_vector2 <- find_position(merged_df_vcf_vep$formatted_ID, merged_intervar_data$ID)
position_in_vector2_or_3_or4 <- positions_in_vector2
`%notin%` <- Negate(`%in%`)
if (any(is.na(positions_in_vector2))) {
  # Identify elements not found in vector2
  not_in_vector2_uv <- merged_df_vcf_vep$Upload_variation[is.na(positions_in_vector2)]
  not_in_vector2_loc <- merged_df_vcf_vep$Location[is.na(positions_in_vector2)]
  # Find positions of elements not in vector2 in vector3
  positions_in_vector3 <- find_position(not_in_vector2_uv, merged_intervar_data$avsnp147.x)

  position_in_vector2_or_3_or4[is.na(positions_in_vector2)] <- positions_in_vector3


  not_in_vector3_loc <- not_in_vector2_loc[is.na(positions_in_vector3)]
  not_in_vector3_uv <- not_in_vector2_uv[is.na(positions_in_vector3)]
  if (any(is.na(positions_in_vector3))) {
    
    # Identify elements not found in vector3

    # Find positions of elements not in vector3 in vector4
    positions_in_vector4 <- find_position(not_in_vector3_loc, merged_intervar_data$location)

    positions_in_vector2_or_3 <-positions_in_vector2
    positions_in_vector2_or_3[is.na(positions_in_vector2_or_3)] <- positions_in_vector3
    position_in_vector2_or_3_or4[is.na(positions_in_vector2_or_3)] <- positions_in_vector4

    not_in_vector4_loc <- not_in_vector3_loc[is.na(positions_in_vector4)]
    not_in_vector4_uv <- not_in_vector3_uv[is.na(positions_in_vector4)]
    print("still remaining")
    print(not_in_vector4_uv)
    merged_df_vcf_vep2 = merged_df_vcf_vep[which(merged_df_vcf_vep$Upload_variation %notin% not_in_vector4_uv),]

    merged_intervar_data2 <- merged_intervar_data[c(position_in_vector2_or_3_or4),]
    merged_intervar_data2<- merged_intervar_data2[!is.na(merged_intervar_data2$ID),]
  }else{

  # print(not_in_vector4_uv)
  merged_df_vcf_vep2 = merged_df_vcf_vep[which(merged_df_vcf_vep$Upload_variation %notin% not_in_vector3_uv),]

  merged_intervar_data2 <- merged_intervar_data[c(position_in_vector2_or_3_or4),]
  merged_intervar_data2<- merged_intervar_data2[!is.na(merged_intervar_data2$ID),]
  }


}else{
  merged_df_vcf_vep2 = merged_df_vcf_vep

  merged_intervar_data2 <- merged_intervar_data[c(position_in_vector2_or_3_or4),]
  merged_intervar_data2<- merged_intervar_data2[!is.na(merged_intervar_data2$ID),]

}


# result$ID<- pathogenic_df2$ID
# fwrite(result,paste0("all_vcf_updated/",job_id,"_Check.csv"))

merged_df_vcf_vep_intervar <- cbind(merged_df_vcf_vep2,merged_intervar_data2)




#merged_df_vcf_vep_intervar<- merge(merged_df_vcf_vep, merged_intervar_data , by.x = "formatted_ID", by.y = "ID", all.y = TRUE)

if(length(merged_df_vcf_vep_intervar$formatted_ID[which(is.na(merged_df_vcf_vep_intervar$formatted_ID)) ]) > 0){
  print("Merging problem formated id")
}

if(length(merged_df_vcf_vep_intervar$formatted_ID[which(is.na(merged_df_vcf_vep_intervar$Feature)) ]) > 0){
  print("Merging problem formated id")
}


fwrite(merged_df_vcf_vep_intervar, paste0(folder_path, job_id, "_merged_with_AF.csv"))

# merged_df_vcf_vep[merged_df_vcf_vep$rsID_1 == "rs71752747",]$formatted_ID

# df = merged_df_vcf_vep_intervar[which(merged_df_vcf_vep_intervar$rsID_1 ==    "rs71752747"),]






