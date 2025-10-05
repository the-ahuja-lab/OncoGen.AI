import os
import pandas as pd
import sys

print(os.getcwd())

snp_inpath= sys.argv[1]+'_bed_filtered.snps-vep.vcf_summary.html'
indel_inpath=sys.argv[1]+'_bed_filtered.indels-vep.vcf_summary.html'
merged_inpath=sys.argv[1]+"_bed_filtered.merged-vep.vcf_summary.html"

print(snp_inpath)
print(indel_inpath)
print(merged_inpath)

out_path=sys.argv[1]

# snp_inpath='/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/OncoGenAI/results/Oncogen202408030004/Oncogen202408030004_bed_filtered.snps-vep.vcf_summary.html'
# indel_inpath='/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/OncoGenAI/results/Oncogen202408030004Oncogen202408030004_bed_filtered.indels-vep.vcf_summary.html'
# merged_inpath='/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/OncoGenAI/results/Oncogen202408030004/Oncogen202408030004_bed_filtered.merged-vep.vcf_summary.html'
# out_path='/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/OncoGenAI/results/Oncogen202408030004/'


snp={'var coding_table':out_path+'snp_coding_consequences.csv','var chr_table':out_path+'snp_chr_count.csv','var polyphen_table':out_path+'snp_polyphen_count.csv','var sift_table':out_path+'snp_sift_count.csv'}
for qt in snp.keys():
  Data_cat=[]
  Data_val=[]
  with open(snp_inpath)as fin:
    for line in fin:
      if qt in line: 
        for val in line.split(']'):
          if val.startswith(','):
            Data_cat.append(val.split(',')[-2].split('\'')[-2])
            Data_val.append(int(val.split(',')[-1]))
        outdf=pd.DataFrame()
        outdf['category']=Data_cat
        outdf['value']=Data_val
        outdf.to_csv(snp[qt])



indel={'var coding_table':out_path+'indel_coding_consequences.csv','var chr_table':out_path+'indel_chr_count.csv'}
for qt in indel.keys():
  Data_cat=[]
  Data_val=[]
  with open(indel_inpath)as fin:
    for line in fin:
      if qt in line:
        for val in line.split(']'):
          if val.startswith(','):
            Data_cat.append(val.split(',')[-2].split('\'')[-2])
            Data_val.append(int(val.split(',')[-1]))
        outdf=pd.DataFrame()
        outdf['category']=Data_cat
        outdf['value']=Data_val
        outdf.to_csv(indel[qt])

merged={'var var_class_table':out_path+'merged_all_varClasses.csv','var consequences_table':out_path+'merged_all_consequences.csv'}
for qt in merged.keys():
  Data_cat=[]
  Data_val=[]
  with open(merged_inpath)as fin:
    for line in fin:
      if qt in line:
        for val in line.split(']'):
          if val.startswith(','):
            Data_cat.append(val.split(',')[-2].split('\'')[-2])
            Data_val.append(int(val.split(',')[-1]))
        outdf=pd.DataFrame()
        outdf['category']=Data_cat
        outdf['value']=Data_val
        outdf.to_csv(merged[qt])

