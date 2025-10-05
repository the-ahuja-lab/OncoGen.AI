import os
import sys

fi=sys.argv[1]+'_bed_filtered.merged2'
with open(fi+'.AFadded.csv','w')as fout:
        fout.write('CHRM\tLOC\tRSID\tRef\tALt\tRefAF\tAltAF\tINFO\n')
        with open(fi+'.vcf')as fin:
                for line in fin:
                        if not line.startswith('#'):
                                #fout.write(line.split('\t')[0]+'\t'+line.split('\t')[1]+'\t'+line.split('\t')[2]+'\t'+line.split('\t')[3]+'\t'+line.split('\t')[4]+'\t'+str(int(line.split('\t')[-1].split(':')[1].split(',')[0])/int(line.split('\t')[-1].split(':')[2]))+'\t'+str(int(line.split('\t')[-1].split(':')[1].split(',')[1])/int(line.split('\t')[-1].split(':')[2]))+'\n')
                                fout.write(line.split('\t')[0]+'\t'+line.split('\t')[1]+'\t'+line.split('\t')[2]+'\t'+line.split('\t')[3]+'\t'+line.split('\t')[4]+'\t'+str(int(line.split('\t')[-1].split(':')[1].split(',')[0])/int(line.split('\t')[-1].split(':')[2]))+'\t'+str(int(line.split('\t')[-1].split(':')[1].split(',')[1])/int(line.split('\t')[-1].split(':')[2]))+'\t'+line.split('\t')[7]+'\n')