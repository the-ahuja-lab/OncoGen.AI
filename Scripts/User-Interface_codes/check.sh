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
trim_adapters="No"
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



# Progress updates based on the provided progress value
if [[ $progress -lt 1 ]]; then
  Rscript update_progress.R "${job_id}" 1
  mkdir -p "OncoGenAI/results/${output_prefix}"
  echo "Started from progess 1"
fi

output_prefix="OncoGenAI/results/${output_prefix}/${output_prefix}"








if [[ $progress -lt 2 ]]; then
  if [[ "$trim_adapters" == "Yes" ]]; then 
      echo "Performing adapter trimming using fastp..."

      trimmed_file1="${output_prefix}_R1_fastp.fastq.gz"
      trimmed_file2="${output_prefix}_R2_fastp.fastq.gz"
      source /opt/conda/etc/profile.d/conda.sh
      source /home/oncogen/miniconda3/etc/profile.d/conda.sh
      conda activate fastp

      if [[ -z "$file_path2" || "$file_path2" == "NA" ]]; then
          # Run fastp for single-end trimming
          fastp -i "$file_path1" -o "$trimmed_file1"
          
          # Update input file variable to point to the trimmed file
          file_path1="$trimmed_file1"

          echo "Adapter trimming completed. Updated input file:"
          echo "Trimmed file 1: ${file_path1}"
      else
          # Run fastp for paired-end trimming
          fastp -i "$file_path1" -I "$file_path2" -o "$trimmed_file1" -O "$trimmed_file2"
          
          # Update input file variables to point to the trimmed files
          file_path1="$trimmed_file1"
          file_path2="$trimmed_file2"

          echo "Adapter trimming completed. Updated input files:"
          echo "Trimmed file 1: ${file_path1}"
          echo "Trimmed file 2: ${file_path2}"
      fi
  fi

  echo "## aligning reads to reference genome ..."

  if [[ -z "$file_path2" || "$file_path2" == "NA" ]]; then
      # Run BWA for single-end alignment
      bwa mem -K 100000000 -v 3 -t 4 -Y -R "@RG\tID:${output_prefix}\tLB:${output_prefix}\tPL:${technology}\tPM:HISEQ\tSM:${output_prefix}" ${genome}.fa ${file_path1} | \
      samtools sort --threads 8 - > ${output_prefix}_aligned_reads.bam
  else
      # Run BWA for paired-end alignment
      bwa mem -K 100000000 -v 3 -t 4 -Y -R "@RG\tID:${output_prefix}\tLB:${output_prefix}\tPL:${technology}\tPM:HISEQ\tSM:${output_prefix}" ${genome}.fa ${file_path1} ${file_path2} | \
      samtools sort --threads 8 - > ${output_prefix}_aligned_reads.bam
  fi

  # Check file size after alignment
  #check_file_size ${output_prefix}_aligned_reads.bam 1048576 "Alignment problem"

  # Update progress using R script
  Rscript update_progress.R "${job_id}" 2
fi




if [[ $progress -lt 3 ]]; then
  echo "#Mark Duplicates ..."
  gatk MarkDuplicatesSpark -I ${output_prefix}_aligned_reads.bam -M ${output_prefix}_dedup_metrics.txt -O ${output_prefix}_sorted_dedup_reads.bam
  #check_file_size  ${output_prefix}_sorted_dedup_reads.bam 1048576 "###? Mark Duplicates problem"
  
  
  java -jar picard.jar CollectAlignmentSummaryMetrics R=${genome}.fa I=${output_prefix}_sorted_dedup_reads.bam O=${output_prefix}_alignment_metrics.txt
  #check_file_size  ${output_prefix}_alignment_metrics.txt 1024 "###? Collect Alignment Summary Metrics problem"
  
  java -jar picard.jar CollectInsertSizeMetrics INPUT=${output_prefix}_sorted_dedup_reads.bam OUTPUT=${output_prefix}_insert_metrics.txt HISTOGRAM_FILE=${output_prefix}_insert_size_histogram.pdf 
  #check_file_size  ${output_prefix}_insert_metrics.txt 1024 "###? Collect Alignment Summary Metrics 2 problem"
  
  
  samtools depth -a ${output_prefix}_sorted_dedup_reads.bam > ${output_prefix}_depth_out.txt
  #check_file_size  ${output_prefix}_depth_out.txt 1024 "###? Collect Alignment Summary Metrics 3 problem"
  Rscript update_progress.R "${job_id}" 3
fi

if [[ $progress -lt 4 ]]; then
  echo "#Base Quality Score Recalibration ..."
  if [[ "${bed_file}" == "NA" ]]; then
    gatk BaseRecalibrator -I ${output_prefix}_sorted_dedup_reads.bam -O ${output_prefix}_bed.recal.table --tmp-dir ./ -R ${genome}.fa --known-sites dbsnp/Mills_and_1000G_gold_standard.indels.${genome}.vcf.gz --known-sites dbsnp/Homo_sapiens_${genome}.dbsnp138.vcf
  else
    gatk BaseRecalibrator -I ${output_prefix}_sorted_dedup_reads.bam -O ${output_prefix}_bed.recal.table -L exome/${bed_file} --tmp-dir ./ -R ${genome}.fa --known-sites dbsnp/Mills_and_1000G_gold_standard.indels.${genome}.vcf.gz --known-sites dbsnp/Homo_sapiens_${genome}.dbsnp138.vcf
  fi
  
  #gatk BaseRecalibrator -I ${output_prefix}_sorted_dedup_reads.bam -O ${output_prefix}_bed.recal.table -L exome/${bed_file} --tmp-dir ./ -R ${genome}.fa --known-sites dbsnp/Mills_and_1000G_gold_standard.indels.${genome}.vcf.gz --known-sites dbsnp/Homo_sapiens_${genome}.dbsnp138.vcf
  #check_file_size  ${output_prefix}_bed.recal.table 1024 "###? Base Quality Score Recalibration problem"
  
  if [[ "${bed_file}" == "NA" ]]; then
    gatk ApplyBQSR -I ${output_prefix}_sorted_dedup_reads.bam -O ${output_prefix}_bed.recal.bam -R ${genome}.fa --bqsr-recal-file ${output_prefix}_bed.recal.table
  else
    gatk ApplyBQSR -I ${output_prefix}_sorted_dedup_reads.bam -O ${output_prefix}_bed.recal.bam -L exome/${bed_file} -R ${genome}.fa --bqsr-recal-file ${output_prefix}_bed.recal.table
  fi
  #gatk ApplyBQSR -I ${output_prefix}_sorted_dedup_reads.bam -O ${output_prefix}_bed.recal.bam -L exome/${bed_file} -R ${genome}.fa --bqsr-recal-file ${output_prefix}_bed.recal.table
  #check_file_size  ${output_prefix}_bed.recal.bam 1048576 "###? Base Quality Score Recalibration 2 problem"
  
  mv ${output_prefix}_bed.recal.bai ${output_prefix}_bed.recal.bam.bai
  Rscript update_progress.R "${job_id}" 4
fi

if [[ "$analysis_type" == "Germline" || "$analysis_type" == "both" ]]; then
  echo "Performing germline analysis."
  if [[ $progress -lt 5 ]]; then
    if [[ "${bed_file}" == "NA" ]]; then
      gatk HaplotypeCaller -R ${genome}.fa -I ${output_prefix}_bed.recal.bam -D dbsnp/Homo_sapiens_${genome}.dbsnp138.vcf -O ${output_prefix}_bed.vcf -ERC GVCF
      #check_file_size  ${output_prefix}_bed.recal.bam 1048576 "###? HaplotypeCaller problem"
      gatk GenotypeGVCFs -R ${genome}.fa -D dbsnp/Homo_sapiens_${genome}.dbsnp138.vcf -V ${output_prefix}_bed.vcf -O ${output_prefix}_bed_g.vcf
      #check_file_size  ${output_prefix}_bed_g.vcf 1000 "###? GenotypeGVCFs problem"
    else
      gatk HaplotypeCaller -R ${genome}.fa -I ${output_prefix}_bed.recal.bam -L exome/${bed_file} -D dbsnp/Homo_sapiens_${genome}.dbsnp138.vcf -O ${output_prefix}_bed.vcf -ERC GVCF
      #check_file_size  ${output_prefix}_bed.recal.bam 1048576 "###? HaplotypeCaller problem"
      gatk GenotypeGVCFs -R ${genome}.fa -L exome/${bed_file} -D dbsnp/Homo_sapiens_${genome}.dbsnp138.vcf -V ${output_prefix}_bed.vcf -O ${output_prefix}_bed_g.vcf
      #check_file_size  ${output_prefix}_bed_g.vcf 1000 "###? GenotypeGVCFs problem"
    fi

    gatk SelectVariants -R ${genome}.fa -V ${output_prefix}_bed_g.vcf -O ${output_prefix}_bed.snps.vcf.gz -select-type SNP
    #check_file_size  ${output_prefix}_bed.snps.vcf.gz 1000 "###? SelectVariants problem"
  
  
    gatk SelectVariants -R ${genome}.fa -V ${output_prefix}_bed_g.vcf -O ${output_prefix}_bed.indels.vcf.gz -select-type INDEL
    #check_file_size  ${output_prefix}_bed.indels.vcf.gz 1000 "###? SelectVariants 2 problem"

    Rscript update_progress.R "${job_id}" 5
  fi

  # if [[ $progress -lt 6 ]]; then
  #   gatk SelectVariants -R ${genome}.fa -V ${output_prefix}_bed_g.vcf -O ${output_prefix}_bed.snps.vcf.gz -select-type SNP
  #   check_file_size  ${output_prefix}_bed.snps.vcf.gz 1000 "###? SelectVariants problem"
  
  
  #   gatk SelectVariants -R ${genome}.fa -V ${output_prefix}_bed_g.vcf -O ${output_prefix}_bed.indels.vcf.gz -select-type INDEL
  #   check_file_size  ${output_prefix}_bed.indels.vcf.gz 1000 "###? SelectVariants 2 problem"
  
  
  #   #Filter Variants
  #   gatk VariantFiltration -R ${genome}.fa -V ${output_prefix}_bed.snps.vcf.gz -O ${output_prefix}_bed_filt.snps.vcf.gz --filter-expression "QD < 2.0" --filter-name "filterQD_lt2.0" --filter-expression "MQ < 25.0" --filter-name "filterMQ_lt25.0" --filter-expression "SOR > 3.0" --filter-name "filterSOR_gt3.0" --filter-expression "MQRankSum < -12.5" --filter-name "filterMQRankSum_lt-12.5" --filter-expression "ReadPosRankSum < -8.0" --filter-name "filterReadPosRankSum_lt-8.0"
  #   gatk VariantFiltration -R ${genome}.fa -V ${output_prefix}_bed.indels.vcf.gz -O ${output_prefix}_bed_filt.indels.vcf --filter-expression "QD < 2.0" --filter-name "filterQD" --filter-expression "SOR > 10.0" --filter-name "filterSOR_gt10.0" --filter-expression "ReadPosRankSum < -20.0" --filter-name "filterReadPosRankSum"
  #   gatk SelectVariants --exclude-filtered -V ${output_prefix}_bed_filt.indels.vcf -O ${output_prefix}_bed_filtered.indels.vcf
  #   gatk SelectVariants --exclude-filtered -V ${output_prefix}_bed_filt.snps.vcf.gz -O ${output_prefix}_bed_filtered.snps.vcf
  
  #   check_file_size  ${output_prefix}_bed_filtered.snps.vcf 1000 "###? SelectVariants 2 problem"
  #   Rscript update_progress.R "${job_id}" 6
fi


if [[ "$analysis_type" == "Somatic" || "$analysis_type" == "both" ]]; then
  echo "Performing somatic analysis."
  
  if [[ $progress -lt 5 ]]; then
    gatk Mutect2 -R ${genome}.fa -I ${output_prefix}_bed.recal.bam --panel-of-normals PON/Mutect2-exome-panel_${genome}.vcf.gz --germline-resource dbsnp/af-only-gnomad.raw.sites_${genome}.vcf.gz -O  ${output_prefix}_somatic.vcf
    #check_file_size ${output_prefix}_somatic.vcf 100000 "###? Mutect2 analysis problem"
    Rscript update_progress.R "${job_id}" 5
  fi
  
  if [[ $progress -lt 6 ]]; then
    gatk SelectVariants -R ${genome}.fa -V ${output_prefix}_somatic.vcf -O ${output_prefix}_somatic.snps.vcf.gz -select-type SNP
    #check_file_size  ${output_prefix}_somatic.snps.vcf.gz 100000 "###? SelectVariants problem"
  
  
    gatk SelectVariants -R ${genome}.fa -V ${output_prefix}_somatic.vcf -O ${output_prefix}_somatic.indels.vcf.gz -select-type INDEL
    #check_file_size  ${output_prefix}_somatic.indels.vcf.gz 100000 "###? SelectVariants 2 problem"k

    Rscript update_progress.R "${job_id}" 6
  fi

  if [[ $progress -lt 7 ]]; then
    #Using VEP to annotate variants in transcript level
    source /opt/conda/etc/profile.d/conda.sh
    source /home/oncogen/miniconda3/etc/profile.d/conda.sh
    conda activate vep_env
    vep --input_file ${output_prefix}_bed_filtered.snps.vcf --output_file ${output_prefix}_bed_filtered.snps-vep.vcf --everything --cache --dir_cache $PWD/vep-${genome} --offline --force_overwrite 
    #check_file_size ${output_prefix}_bed_filtered.snps-vep.vcf 100000 "###? VEP problem"
  
  
    vep --input_file ${output_prefix}_bed_filtered.indels.vcf --output_file ${output_prefix}_bed_filtered.indels-vep.vcf --everything --cache --dir_cache $PWD/vep-${genome} --offline --force_overwrite
    #check_file_size ${output_prefix}_bed_filtered.indels-vep.vcf 100000 "###? VEP problem"
  
    #ANNOVAR and InterVar to annotate and interpret variants clinical significance based on ACMG guidelines
    cd InterVar-master/
    python Intervar-${genome}.py  -i ../${output_prefix}_bed_filtered.snps.vcf  -o ../${output_prefix}/${output_prefix}_snps --input_type VCF
    python Intervar-${genome}.py  -i ../${output_prefix}_bed_filtered.indels.vcf  -o ../${output_prefix}/${output_prefix}_indels --input_type VCF
    cd ..
    #check_file_size ${output_prefix}_indels.${genome}_multianno.txt.grl_p  100000 "###? InterVar problem 1"
    #check_file_size ${output_prefix}_indels.${genome}_multianno.txt.intervar  100000 "###? InterVar problem 2"
    #check_file_size ${output_prefix}_snps.${genome}_multianno.txt.grl_p  100000 "###? InterVar problem 3"
    #check_file_size ${output_prefix}_snps.${genome}_multianno.txt.intervar  100000 "###? InterVar problem 4"
    
    
    Rscript tab_view_backend.R ${job_id} ${genome}
  
    Rscript update_progress.R "${job_id}" 7
  fi
fi




# End the timer
end=$(date +%s)

# Calculate the elapsed time
elapsed=$((end - start))

Rscript update_status.R $job_id "Completed"

echo "The script took $elapsed seconds to run."

echo "ALL_DONE_Successfully $(date +'%Y-%m-%d %H:%M:%S')"



