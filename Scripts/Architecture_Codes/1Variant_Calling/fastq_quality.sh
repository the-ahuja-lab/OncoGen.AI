#fastq_quality.sh
source /opt/conda/etc/profile.d/conda.sh
conda activate nf-env
if [[ $# -eq 1 ]]; then
    fastqc -o fastq_results $1
fi



