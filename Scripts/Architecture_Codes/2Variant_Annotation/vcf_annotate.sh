  #!/bin/bash

# Start the timer
start=$(date +%s)

PATH=$PATH:$PWD/gatk-4.5.0.0/
PATH=$PATH:$PWD/samtools-1.20/
PATH=$PATH:$PWD/bwa-0.7.17/
PATH=$PATH:$PWD/bcftools-1.20/

# Function to display usage
usage() {
  echo "Usage: $0 -f1 <file_path1> -f2 <file_path2> -g <genome> -s <output_prefix> -a <analysis_type> -b <bed_file> -t <technology> -p <progress> -trim <trim_adapters> -cnv <cnv_analysis>"
  exit 1
}

# Initialize variables
file_path1=""
file_path2=""
genome=""
output_prefix=""
analysis_type=""
bed_file=""
technology=""
progress=""
trim_adapters=""
cnv_analysis="Yes"

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -f1)
      file_path1="$2"
      shift 2
      ;;
    -f2)
      file_path2="$2"
      shift 2
      ;;
    -g)
      genome="$2"
      shift 2
      ;;
    -s)
      output_prefix="$2"
      shift 2
      ;;
    -a)
      analysis_type="$2"
      shift 2
      ;;
    -b)
      bed_file="$2"
      shift 2
      ;;
    -t)
      technology="$2"
      shift 2
      ;;
    -p)
      progress="$2"
      shift 2
      ;;
    -trim)
      trim_adapters="$2"
      shift 2
      ;;
    -cnv)
      cnv_analysis="$2"
      shift 2
      ;;
    *)
      usage
      ;;
  esac
done

# Check if all required arguments are provided
if [[ -z "$file_path1" || -z "$file_path2" || -z "$genome" || -z "$output_prefix" || -z "$analysis_type" || -z "$bed_file" || -z "$technology" || -z "$progress" ]]; then
  usage
fi

# Check if input files exist
if [[ ! -f "$file_path1" ]]; then
    echo "Error: Input file $file_path1 not found."
    exit 1
fi

if [[ ! -f "$file_path2" ]]; then
    echo "Error: Input file $file_path2 not found."
    exit 1
fi

# Check if progress is a valid number
if ! [[ "$progress" =~ ^[0-9]+$ ]]; then
  echo "Error: Progress must be a valid number."
  echo p$rogress
  exit 1
fi

# Print the arguments
echo "file_path1: ${file_path1}"
echo "file_path2: ${file_path2}"
echo "genome: ${genome}"
echo "output_prefix: ${output_prefix}"
echo "analysis_type: ${analysis_type}"
echo "bed_file: ${bed_file}"
echo "technology: ${technology}"
echo "progress: ${progress}"
echo "trim_adapters: ${trim_adapters}"
echo "cnv_analysis: ${cnv_analysis}"

if [ "$genome" == "hg19" ]; then
    annotate_file="hg19-refFlat.txt"
    access_file="data/access-5k-mappable.hg19.bed"
    reference_file="my_flat_reference_hg19.cnn"
else
    annotate_file="hg38-refFlat.txt"
    access_file="data/access-10kb.hg38.bed"
    reference_file="my_flat_reference_hg38.cnn"
fi


# Determine the genome name based on the genome variable
if [ "$genome" = "hg19" ]; then
    genome_name="GRCh37"
elif [ "$genome" = "hg38" ]; then
    genome_name="GRCh38"
else
    genome_name="Unknown"
fi
# #!/bin/bash
# 
# # Start the timer
# start=$(date +%s)
# 
# PATH=$PATH:$PWD/gatk-4.5.0.0/
# PATH=$PATH:$PWD/samtools-1.20/
# PATH=$PATH:$PWD/bwa-0.7.17/
# PATH=$PATH:$PWD/bcftools-1.20/
# 
# # Function to display usage
# usage() {
#   echo "Usage: $0 -f1 <file_path1> -f2 <file_path2> -g <genome> -s <output_prefix> -a <analysis_type> -b <bed_file> -t <technology> -p <progress>"
#   exit 1
# }
# 
# # Initialize variables
# file_path1=""
# file_path2=""
# genome=""
# output_prefix=""
# analysis_type=""
# bed_file=""
# technology=""
# progress=""
# 
# # Parse command-line arguments
# while [[ $# -gt 0 ]]; do
#   case "$1" in
#     -f1)
#       file_path1="$2"
#       shift 2
#       ;;
#     -f2)
#       file_path2="$2"
#       shift 2
#       ;;
#     -g)
#       genome="$2"
#       shift 2
#       ;;
#     -s)
#       output_prefix="$2"
#       shift 2
#       ;;
#     -a)
#       analysis_type="$2"
#       shift 2
#       ;;
#     -b)
#       bed_file="$2"
#       shift 2
#       ;;
#     -t)
#       technology="$2"
#       shift 2
#       ;;
#     -p)
#       progress="$2"
#       shift 2
#       ;;
#     *)
#       usage
#       ;;
#   esac
# done
# 
# # Check if all required arguments are provided
# if [[ -z "$file_path1" || -z "$file_path2" || -z "$genome" || -z "$output_prefix" || -z "$analysis_type" || -z "$bed_file" || -z "$technology" || -z "$progress" ]]; then
#   usage
# fi
# 
# # Check if input files exist
# if [[ ! -f "$file_path1" ]]; then
#     echo "Error: Input file $file_path1 not found."
#     exit 1
# fi
# 
# if [[ ! -f "$file_path2" ]]; then
#     echo "Error: Input file $file_path2 not found."
#     exit 1
# fi
# 
# # Check if progress is a valid number
# if ! [[ "$progress" =~ ^[0-9]+$ ]]; then
#   echo "Error: Progress must be a valid number."
#   exit 1
# fi
# 
# # Print the arguments
# echo "file_path1: ${file_path1}"
# echo "file_path2: ${file_path2}"
# echo "genome: ${genome}"
# echo "output_prefix: ${output_prefix}"
# echo "analysis_type: ${analysis_type}"
# echo "bed_file: ${bed_file}"
# echo "technology: ${technology}"
# echo "progress: ${progress}"

job_id=${output_prefix}




Rscript update_pid.R "${job_id}" "$BASHPID"
echo "$BASHPID"

# Function to check if a file is less than 1MB
check_file_size() {
  local file_name="$1"
  local file_size_predicted="$2"
  local error_message="$3"

  # Check if file exists
  if [ ! -f "$file_name" ]; then
    echo "'$file_name' not found!"
    echo $error_message
    # Rscript update_status.R $job_id "$error_message"
    Rscript update_status.R $job_id "Aborted"
    exit 0
  fi

  # Get the file size in bytes
  local file_size
  file_size=$(stat -c%s "$file_name")

  # Check if file size is less than 1MB (1MB = 1048576 bytes)
  if [ "$file_size" -lt $file_size_predicted ]; then
    echo $error_message
    exit 0
  else
    echo "The file '$file_name' created ."
  fi
}

output_prefix="OncoGenAI/results/${output_prefix}/${output_prefix}"
echo "run vcf annotation again"
if [[ $progress -lt 7 ]]; then
  gatk SelectVariants --exclude-filtered -V ${output_prefix}_bed_filt.indels.vcf -O ${output_prefix}_bed_filtered.indels.vcf
  gatk SelectVariants --exclude-filtered -V ${output_prefix}_bed_filt.snps.vcf.gz -O ${output_prefix}_bed_filtered.snps.vcf
  #Using VEP to annotate variants in transcript level

  source /opt/conda/etc/profile.d/conda.sh
  #source /home/oncogen/miniconda3/etc/profile.d/conda.sh
  conda activate vep
  vep --input_file ${output_prefix}_bed_filtered.snps.vcf --output_file ${output_prefix}_bed_filtered.snps-vep.vcf --everything --cache --assembly ${genome_name} --dir_cache $PWD/vep-${genome} --tab --offline --force_overwrite
  vep --input_file ${output_prefix}_bed_filtered.indels.vcf --output_file ${output_prefix}_bed_filtered.indels-vep.vcf --everything --cache --assembly ${genome_name} --dir_cache $PWD/vep-${genome} --tab --offline --force_overwrite
  #check_file_size ${output_prefix}_bed_filtered.indels-vep.vcf 1000 "###? VEP problem 1"
  #check_file_size ${output_prefix}_bed_filtered.snps-vep.vcf 1000 "###? VEP problem 2"

  gatk MergeVcfs -I ${output_prefix}_bed_filtered.snps.vcf -I ${output_prefix}_bed_filtered.indels.vcf  -O ${output_prefix}_bed_filtered.merged.vcf
  ./vcf_id_add.sh ${output_prefix}_bed_filtered.merged.vcf ${output_prefix}_bed_filtered.merged2.vcf
  vep --input_file  ${output_prefix}_bed_filtered.merged2.vcf --output_file ${output_prefix}_bed_filtered.merged-vep.vcf --everything --cache --assembly ${genome_name} --dir_cache $PWD/vep-${genome} --tab --offline --force_overwrite
  #check_file_size ${output_prefix}_bed_filtered.merged-vep.tsv 1000 "###? VEP problem 3"


  #ANNOVAR and InterVar to annotate and interpret variants clinical significance based on ACMG guidelines
  cd InterVar-master/
  python Intervar-${genome}.py  -i ../${output_prefix}_bed_filtered.merged2.vcf  -o ../${output_prefix}_merged --input_type VCF
  python Intervar-${genome}.py  -i ../${output_prefix}_bed_filtered.snps.vcf  -o ../${output_prefix}_snps --input_type VCF
  python Intervar-${genome}.py  -i ../${output_prefix}_bed_filtered.indels.vcf  -o ../${output_prefix}_indels --input_type VCF
  cd ..
  # check_file_size ${output_prefix}_indels.${genome}_multianno.txt.grl_p  1000 "###? InterVar problem 1"
  # check_file_size ${output_prefix}_indels.${genome}_multianno.txt.intervar  1000 "###? InterVar problem 2"
  # check_file_size ${output_prefix}_snps.${genome}_multianno.txt.grl_p  1000 "###? InterVar problem 3"
  # check_file_size ${output_prefix}_snps.${genome}_multianno.txt.intervar  1000 "###? InterVar problem 4"

  source /opt/conda/etc/profile.d/conda.sh
  #source /home/oncogen/miniconda3/etc/profile.d/conda.sh
  conda activate nf-env
  pip install pandas
  python vephtml_parse.py ${output_prefix}


  if [ "$cnv_analysis" == "Yes" ]; then
      # Do something if cnv_analysis is "yes"
      echo "Performing CNV analysis..."
      # Your code to perform CNV analysis goes here
      # CNV analysis
      source /opt/conda/etc/profile.d/conda.sh
      #source /home/oncogen/miniconda3/etc/profile.d/conda.sh
      conda activate cnv-env
      cd cnvkit

      
      cnvkit.py batch ../${output_prefix}_sorted_dedup_reads.bam -n -t ../exome/${bed_file} -f ../${genome}.fa --annotate ${genome}-refFlat.txt --access ${access_file} --output-reference ${reference_file} --output-dir ../OncoGenAI/results/${job_id}/
      cnvkit.py export bed batch ../${output_prefix}_sorted_dedup_reads.cns -o ${output_prefix}.bed
      cd ..
      python cnv-csv.py ${output_prefix}_sorted_dedup_reads.call.cns ${output_prefix}call-cns.csv
      python cnv-csv.py ${output_prefix}_sorted_dedup_reads.cnr ${output_prefix}cnr.csv
      python cnv_del_dup.py ${output_prefix}call-cns.csv ${output_prefix}-call-cns-del-dup.csv
      python cnv_del_dup.py ${output_prefix}cnr.csv ${output_prefix}-cnr-del-dup.csv
      python cnv-bed.py ${output_prefix}-call-cns-del-dup.csv ${output_prefix}-call-cns-del-dup.bed
      python cnv-bed.py ${output_prefix}-cnr-del-dup.csv ${output_prefix}-cnr-del-dup.bed


      cd ClassifyCNV
      mkdir -p ../OncoGenAI/results/${job_id}/CNVclassify/
      python3 ClassifyCNV.py --infile ../${output_prefix}-call-cns-del-dup.bed --GenomeBuild ${genome} --precise --outdir ../OncoGenAI/results/${job_id}/CNVclassify/
      python txt-csv.py ../OncoGenAI/results/${job_id}/CNVclassify/Scoresheet.txt ../${output_prefix}-Scoresheet.csv
      cd ..
  else
      # Optional: Do something else if cnv_analysis is not "yes"
      echo "Skipping CNV analysis."
  fi

  

fi
if [[ $progress -lt 8 ]]; then
  # python cnv-csv.py "${base_name}.cnr" "${oncogen_id}.cnr.csv"
  # python cnv_del_dup.py "${oncogen_id}.call-cns.csv" "${oncogen_id}-call-cns-del-dup.csv"
  # python cnv_del_dup.py "${oncogen_id}.cnr.csv" "${oncogen_id}-cnr-del-dup.csv"
  # python cnv-bed.py "${oncogen_id}-call-cns-del-dup.csv" "${oncogen_id}-call-cns-del-dup.bed"
  # python cnv-bed.py "${oncogen_id}-cnr-del-dup.csv" "${oncogen_id}-cnr-del-dup.bed"
  python3 holy_AFr3.py ${output_prefix}
  Rscript data_merger.r ${job_id} ${genome}
  python3 transcript-id.py ${output_prefix}_merged_with_AF.csv GRCH38-apris-ensembl-with-NM.csv ${output_prefix}_transcript-id.csv

  Rscript update_progress.R "${job_id}" 7
fi

Rscript update_status.R $job_id "Completed"

echo "The script took $elapsed seconds to run."

echo "ALL_DONE_Successfully $(date +'%Y-%m-%d %H:%M:%S')"