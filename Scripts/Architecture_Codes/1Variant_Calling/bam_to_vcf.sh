#!/bin/bash

# Start the timer
start=$(date +%s)
export PATH=$PATH:IonTorrent-VariantCaller-install/usr/local/bin
# Set the path to tools
#PATH=$PATH:$PWD/gatk-4.5.0.0/
#PATH=$PATH:$PWD/samtools-1.21/
#PATH=$PATH:$PWD/bwa-0.7.17/
#PATH=$PATH:$PWD/bcftools-1.20/

# Function to display usage
usage() {
  echo "Usage: $0 -f <bam_file_path> -g <genome> -s <output_prefix> -a <analysis_type> -b <bed_file> -t <technology> -p <progress> -trim <trim_adapters> -cnv <cnv_analysis> -tumor <tumor_percentage>"
  exit 1
}

# Function to convert tumor percentage to fraction
convert_to_fraction() {
  local input="$1"
  if [[ "$input" =~ ^[0-9]+$ ]] && (( input >= 0 && input <= 100 )); then
    echo "scale=2; $input / 100" | bc
  else
    echo "Error: Tumor percentage must be a valid number between 0 and 100." >&2
    exit 1
  fi
}

# Initialize variables
file_path=""
genome=""
output_prefix=""
analysis_type=""
bed_file=""
technology=""
progress=""
trim_adapters="false"
cnv_analysis="Yes"
tumor_percentage=""

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -f)
      file_path="$2"
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
    -tumor)
      tumor_percentage="$2"
      shift 2
      ;;
    *)
      usage
      ;;
  esac
done

# Check if all required arguments are provided
if [[ -z "$file_path" || -z "$genome" || -z "$output_prefix" || -z "$analysis_type" || -z "$bed_file" || -z "$technology" || -z "$progress" || -z "$tumor_percentage" ]]; then
  usage
fi

# Check if input BAM file exists
if [[ ! -f "$file_path" ]]; then
  echo "Error: Input BAM file $file_path not found."
  exit 1
fi

# Check if progress is a valid number
if ! [[ "$progress" =~ ^[0-9]+$ ]]; then
  echo "Error: Progress must be a valid number."
  exit 1
fi

# Validate and convert tumor percentage to fraction
tumor_percentage=$(convert_to_fraction "$tumor_percentage")

# Print the arguments
echo "file_path: ${file_path}"
echo "genome: ${genome}"
echo "output_prefix: ${output_prefix}"
echo "analysis_type: ${analysis_type}"
echo "bed_file: ${bed_file}"
echo "technology: ${technology}"
echo "progress: ${progress}"
echo "trim_adapters: ${trim_adapters}"
echo "cnv_analysis: ${cnv_analysis}"

# Set genome-specific files
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

# Modify this line to include the path where your BAM file is located
#file_path=/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/InterVar-master/Sample1Output/indel_assembly.vcf



# End timer and display duration
end=$(date +%s)
runtime=$((end-start))
echo "Script runtime: $runtime seconds"



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
    Rscript update_status.R $job_id "Aborted"
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


#  docker run --rm -v ${bam_directory}:/data1 -v ${bam_directory}:/data2 -v ${PWD}/exome:/data3 -v ${bam_directory}:/output sanjayk741/tvc:variantcaller variant_caller_pipeline.py --input-bam "/data1/${bam}" --reference-fasta "/data2/hg19.fasta" --parameters-file "/data3/ccp_somatic_lowstringency_p1_parameters.json" --output-dir "/output/${output_prefix}" --sample-name "sample1" --region-bed "/data3/OCAPlus.20210824.designed.bed" --num-threads 1 --generate-gvcf=on

#sudo docker run --rm -v /home/suvenduk/itor_example:/data1 -v /home/suvenduk/itor_example:/data2 -v /home/suvenduk/itor_example:/output iontorrent/variantcaller variant_caller_pipeline.py --input-bam "/data1/Sample1_IonXpress_026_rawlib.bam" --reference-fasta "/data2/hg19.fasta" --parameters-file "/data1/ccp_somatic_lowstringency_p1_parameters.json" --output-dir "/output/Sample1Output" --sample-name "sample1" --region-bed "/data1/OCAPlus.20210824.designed.bed" --num-threads 12 --generate-gvcf=on

if [[ $progress -lt 2 ]]; then

  echo "## Running Iontorrent variant caller ..."


  cp $file_path itor_example/
  cp exome/${bed_file} itor_example/
  filename=$(basename "$file_path")
  bedname=$(basename "$bed_file")
  echo $filename
  echo $bedname
  ./samtools-1.20/samtools index itor_example/$filename

  

  
  #docker run --rm -v /itor_example:/data1 -v /itor_example:/data2 -v /itor_example:/output iontorrent/variantcaller variant_caller_pipeline.py --input-bam "/data1/${filename}" --reference-fasta "/data2/hg19.fasta" --parameters-file "/data1/ccp_somatic_lowstringency_p1_parameters.json" --output-dir "/output/${job_id}" --sample-name ${job_id} --region-bed "/data1/${bedname}" --num-threads 12 --generate-gvcf=on
  if [[ "$analysis_type" == "Somatic" ]]; then
    python2 IonTorrent-VariantCaller/bin/variant_caller_pipeline.py --input-bam itor_example/${filename} --reference-fasta itor_example/hg19.fasta --parameters-file itor_example/ccp_somatic_lowstringency_p1_parameters.json --output-dir itor_example/${job_id} --region-bed itor_example/${bedname} --num-threads 12 --generate-gvcf=on
  else
    python2 IonTorrent-VariantCaller/bin/variant_caller_pipeline.py --input-bam itor_example/${filename} --reference-fasta itor_example/hg19.fasta --parameters-file itor_example/ampliseqexome_germline_lowstringency_p1_parameters.json --output-dir itor_example/${job_id} --region-bed itor_example/${bedname} --num-threads 12 --generate-gvcf=on
  fi
  mv itor_example/${job_id} "OncoGenAI/results/${job_id}"
  # Check file size after alignment
  #check_file_size "OncoGenAI/results/${job_id}/${job_id}/TSVC_variants.vcf" 1048576 "Variant calling problem"


  # Update progress using R script
  Rscript update_progress.R "${job_id}" 2
fi
if [[ $progress -lt 5 ]]; then
  # cp OncoGenAI/results/${job_id}/${job_id}/TSVC_variants.vcf 
  gatk SelectVariants -R itor_example/hg19.fasta -V OncoGenAI/results/${job_id}/${job_id}/TSVC_variants.vcf -O ${output_prefix}_bed.snps.vcf.gz -select-type SNP
  #check_file_size  ${output_prefix}_bed.snps.vcf.gz 1000 "###? SelectVariants problem"


  gatk SelectVariants -R itor_example/hg19.fasta -V OncoGenAI/results/${job_id}/${job_id}/TSVC_variants.vcf -O ${output_prefix}_bed.indels.vcf.gz -select-type INDEL
  #check_file_size  ${output_prefix}_bed.indels.vcf.gz 1000 "###? SelectVariants 2 problem"

  Rscript update_progress.R "${job_id}" 5

fi

if [[ $progress -lt 6 ]]; then
    if [ "$cnv_analysis" == "Yes" ]; then
      # Do something if cnv_analysis is "yes"
      echo "Performing CNV analysis..."
      # Your code to perform CNV analysis goes here
      # CNV analysis
      cp $file_path ${output_prefix}_sorted_dedup_reads.bam
      source /opt/conda/etc/profile.d/conda.sh
      #source /home/oncogen/miniconda3/etc/profile.d/conda.sh
      conda activate cnv-env
      cd cnvkit

      
      cnvkit.py batch ../${output_prefix}_sorted_dedup_reads.bam -n -t ../exome/${bed_file} -f ../${genome}/${genome}.fa --annotate ${genome}-refFlat.txt --access ${access_file} --output-reference ${reference_file} --output-dir ../OncoGenAI/results/${job_id}/
      cnvkit.py call -m none ../${output_prefix}_sorted_dedup_reads.call.cns --purity $tumor_percentage -y -o ../${output_prefix}_sorted_dedup_reads.call.cns
      cd ..
      python cnv-csv.py ${output_prefix}_sorted_dedup_reads.call.cns ${output_prefix}call-cns.csv
      python cnv-csv.py ${output_prefix}_sorted_dedup_reads.cnr ${output_prefix}cnr.csv
      python cnv_del_dup.py ${output_prefix}call-cns.csv ${output_prefix}-call-cns-del-dup.csv
      python cnv_del_dup.py ${output_prefix}cnr.csv ${output_prefix}-cnr-del-dup.csv
      python cnv-bed.py ${output_prefix}-call-cns-del-dup.csv ${output_prefix}-call-cns-del-dup.bed
      python cnv-bed.py ${output_prefix}-cnr-del-dup.csv ${output_prefix}-cnr-del-dup.bed


      cd ClassifyCNV
      rm -r ../OncoGenAI/results/${job_id}/CNVclassify/
      mkdir -p ../OncoGenAI/results/${job_id}/CNVclassify/
      python3 ClassifyCNV.py --infile ../${output_prefix}-call-cns-del-dup.bed --GenomeBuild ${genome} --precise --outdir ../OncoGenAI/results/${job_id}/CNVclassify/
      python ../txt_to_csv.py ../OncoGenAI/results/${job_id}/CNVclassify/Scoresheet.txt ../${output_prefix}-Scoresheet.csv
      cd ..
    else
      # Optional: Do something else if cnv_analysis is not "yes"
      echo "Skipping CNV analysis."
    fi
fi
echo $cnv_analysis


if [[ $progress -lt 7 ]]; then
  gatk SelectVariants --exclude-filtered -V ${output_prefix}_bed.indels.vcf.gz -O ${output_prefix}_bed_filtered.indels.vcf
  gatk SelectVariants --exclude-filtered -V ${output_prefix}_bed.snps.vcf.gz -O ${output_prefix}_bed_filtered.snps.vcf
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
  python3 holy_AFr2.py ${output_prefix}
  Rscript data_merger.r ${job_id} ${genome}
  python3 transcript-id.py ${output_prefix}_merged_with_AF.csv GRCH38-apris-ensembl-with-NM.csv ${output_prefix}_transcript-id.csv
  python3 vephtml_parse.py ${output_prefix}

  Rscript update_progress.R "${job_id}" 7


fi


# End the timer
end=$(date +%s)

# Calculate the elapsed time
elapsed=$((end - start))

Rscript update_status.R $job_id "Completed"

echo "The script took $elapsed seconds to run."

echo "ALL_DONE_Successfully $(date +'%Y-%m-%d %H:%M:%S')"



