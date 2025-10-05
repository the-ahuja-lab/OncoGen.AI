# promt_fil= "/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/report.txt"

import google.generativeai as genai
#import PIL.Image
import os
import sys

promt_fil=sys.argv[1]

def format_paragraph(input_string, characters_per_line):
    words = input_string.split()
    lines = []
    current_line = ""

    for word in words:
        if len(current_line) + len(word) + 1 <= characters_per_line:
            if current_line:
                current_line += " " + word
            else:
                current_line = word
        else:
            lines.append(current_line)
            current_line = word

    if current_line:
        lines.append(current_line)
    with open('OncoGenAI/generated_report.txt','w')as fout:
        # for line in lines:
        #     fout.write(line+'\n')
        fout.write(input_string+'\n')


import google.generativeai as genai
#import PIL.Image
import os

genai.configure(api_key="AIzaSyBFQzvJcc7fwWintEoQJt71oT55gYfW06M") #my personal APi key

model = genai.GenerativeModel(model_name="gemini-1.5-flash")

# for fl in os.listdir('/home/suvenduk/OncoGenAI/Final_oncogen/OncoGenAI/results/'):
# 	if fl.endswith('_prompt.txt'):
# 		promt_fil='/home/suvenduk/OncoGenAI/Final_oncogen/OncoGenAI/results/'+fl


in_promt=''
with open(promt_fil)as fin:
	for line in fin:
		in_promt+=' '+line.rstrip()
#os.remove(promt_fil)
response = model.generate_content([in_promt])
characters_per_line = 100

format_paragraph(response.text, characters_per_line)
