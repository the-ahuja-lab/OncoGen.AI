# 🧬 OncoGen.ai: An Integrated Knowledge-Graph-Driven Platform for Automated Genomic Analysis and Clinical Reporting in Precision Oncology

OncoGen.ai is a **containerized bioinformatics platform** designed for streamlined **variant analysis and interpretation** in clinical cancer genomics. It supports **Illumina (FASTQ)** and **Ion Torrent (BAM)** workflows and delivers a full analytical pipeline—**from raw sequence data to interactive visualizations and clinical insights**—via an integrated **R Shiny interface**.

---

## 🚀 Features

- 🔄 **Dual Workflow Support**: Illumina & Ion Torrent
- 🔍 Quality Control, Variant Calling, CNV & TMB Analysis
- 🧠 Integrated Clinical Annotation (ANNOVAR, VEP, InterVar)
- 📊 Real-time Visualizations with Shiny Dashboard
- 🧬 Supports **hg19 (GRCh37)** and **hg38 (GRCh38)** genomes
- 🔗 Google Gemini-powered report generation (Generative-AI-enhanced)

---

## 📦 Docker Installation

### Step 1: Install Docker

**Ubuntu/Linux:**
```bash
sudo apt update
sudo apt install docker.io -y
sudo systemctl start docker
sudo systemctl enable docker
```

**Mac/Windows:**  
Download Docker Desktop: [https://www.docker.com/products/docker-desktop](https://www.docker.com/products/docker-desktop)

---

## 📥 Pull the Docker Image

Pull from DockerHub:
```bash
docker pull ahujalab/oncogenai
```

---

## 🖥️ Running the App

To launch the OncoGen.ai web application with your input and output directories mounted:

```bash
docker run --rm -p 1235:1235 \
  -v /path/to/input-directory:/app-input \
  -v /path/to/output-directory:/OncoGenAI/results \
  ahujalab/oncogenai
```

- Replace `/path/to/input-directory` with the folder containing FASTQ/BAM files
- Replace `/path/to/output-directory` with the folder to store results
- The `--rm` flag ensures that the container is cleaned up after stopping

After execution, access the app at:  
👉 [http://localhost:1235](http://localhost:1235)

---

## 🔬 Workflow Overview

| Platform        | Input Type | Process Triggered        | Notes                                |
|------------------|------------|---------------------------|---------------------------------------|
| **Ion Torrent**  | `.bam`     | `bam_to_vcf.sh`           | Internal—variant calling, CNV, annotation |
| **Illumina**     | `.fastq.gz`| `fastq_to_vcf.sh`         | Internal—QC, alignment, variant calling |
| **Common Output**| `.vcf`     | `vcf_annotate.sh`, `TMB.R`| Functional annotation and TMB scoring |

> ⚠️ You do **not** need to manually run any script.  
> All steps are automatically executed after job submission via the Shiny interface.

---

## 🛠️ Toolchain & Versions

| Tool Name               | Version         | Purpose                                                  |
|-------------------------|-----------------|----------------------------------------------------------|
| **FastQC**              | v0.12.1         | Quality control for sequencing reads                     |
| **Burrows-Wheeler Aligner (BWA)** | v0.7.17 | Read alignment                                           |
| **Samtools**            | v1.20           | Manipulation and indexing of BAM files                   |
| **GATK**                | v4.5.0.0        | Variant calling for Illumina data                        |
| **Ion Torrent Variant Caller** | v5.0-3   | Variant calling for Ion Torrent data                     |
| **VEP (Variant Effect Predictor)** | v112 | Variant annotation using Ensembl databases               |
| **ANNOVAR**             | 2019Oct24       | Functional annotation of variants                        |
| **InterVar**            | Updated-2021-08 | Clinical interpretation of variants                      |
| **CNVKit**              | v0.9.11         | CNV detection and visualization                          |
| **ClassifyCNV**         | v1.1.1          | CNV classification and pathogenicity scoring             |
| **TMBler**              | R package       | Tumor Mutation Burden scoring                            |
| **igvShiny**            | v1.0.5          | Embedded IGV-style genome browser                        |
| **VariantAnnotation**   | v1.50.0         | Parsing and filtering VCFs in R                          |
| **GenomicAlignments**   | v1.40.0         | Genomic alignment analysis                               |
| **Shiny**               | v1.9.1          | Web application framework in R                           |
| **Google Gemini API**   | gemini-1.5-flash| AI-generated summaries and clinical reports              |

---

## ⚙️ System & R Dependencies Pre-installed

- `build-essential`, `gfortran`, `libreadline-dev`, `libpng-dev`, `libcurl4-openssl-dev`, `libssl-dev`, `libbz2-dev`, `liblzma-dev`, `zlib1g-dev`, `default-jdk`
- **R 4.4.1** with Bioconductor and CRAN packages:
  - `shiny`, `shinydashboard`, `DT`, `plotly`, `ggplot2`, `ggpubr`, `pheatmap`, `vcfR`, `circlize`, `tidyverse`, `lubridate`, `visNetwork`, `GenomicRanges`, `TxDb.Hsapiens.UCSC.hg19.knownGene`, `TxDb.Hsapiens.UCSC.hg38.knownGene`, `org.Hs.eg.db`, `AnnotationDbi`, `musicatk`, `BioCircos`, `VariantAnnotation`, `GenomicAlignments`, `ggbio`, `StructuralVariantAnnotation`, `shiny.gosling`, `igvShiny`, and more.
- **Python** and required packages via `conda`:
  - `fastp`, `bedtools`, `cnvkit`, `google-generativeai`, `pandas`
- Additional software: `docker.io`, `git`, `curl`, `wget`, `unzip`, `perl`, `cmake`, `java-17`, `picard`, `InterVar`, `ANNOVAR`, `ClassifyCNV`

---

## 📁 Directory Structure (Inside Container)

| Container Path         | Description                          |
|------------------------|--------------------------------------|
| `/app-input`           | User input files (FASTQ/BAM)         |
| `/OncoGenAI/results`   | Result files (by job ID)             |
| `/hg19`, `/hg38`       | Reference FASTA files                | 
| `/exome`, `/panel_file`, `/PON` | Reference & panel files   |
| `/TmbR_scripts`        | Scripts for TMB scoring              |
| `/InterVar-master`, `/annovar` | Clinical interpretation |
| `/vep-hg19`, `/vep-hg38` | Transcript-based annotation |

---

## 👤 Developer & Contact

Developed by: **Ahuja Lab**  
📧 Email: `gaurav.ahuja@iiitd.ac.in`

---

## 🧠 License

This project is intended for **academic and clinical research use only**. Please cite appropriately and contact us for collaboration or commercial use.

---
