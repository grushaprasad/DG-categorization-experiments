import re
import os
import csv

## fname, continuum_member, condition, itemnum, group  


stimdir = './combined_stims/mturk/ogg/'
#targetdir = './target_stims/'

target_stims = ['a_dg09_CV.ogg', 'a_dg10_CV.ogg', 'a_dg11_CV.ogg', 'a_dg12_CV.ogg', 'a_dg13_CV.ogg', 'a_dg14_CV.ogg', 'a_dg15_CV.ogg', 'a_dg16_CV.ogg']
practice_stims1 = ['a_dg01_CV.ogg', 'a_dg02_CV.ogg', 'a_dg19_CV.ogg', 'a_dg20_CV.ogg']

practice_stims2 = ['dg01_lowmid.ogg', 'dg02_midlow.ogg', 'dg19_midhigh.ogg', 'dg20_highmid.ogg']

num_lists = 4
num_reps = 10

rows = []
rows.append(['fname', 'continuum_member', 'condition', 'itemnum', 'Group', 'Block'])

for f in os.listdir(stimdir):
    if f.endswith(".ogg"):
        group = int(re.findall('\d+', f)[0]) #i.e. list
        continuum_member = int(re.findall('\d+', f)[1])
        cond = re.findall('\_([a-z]*?)\.',f)[0]
        itemnum = int(re.findall('\d+', f)[2])
        block = "with_precursor"

        rows.append([f, continuum_member, cond, itemnum, group, block])




for l in range(num_lists):
    for r in range(num_reps):
        for f in target_stims:
            group = l
            continuum_member = int(re.findall('\d+', f)[0])
            cond = 'NA'
            itemnum = 'NA'
            block = "without_precursor"

            rows.append([f, continuum_member, cond, itemnum, group, block])

    for p in practice_stims1:
        group = l
        continuum_member = int(re.findall('\d+', p)[0])
        cond = 'NA'
        itemnum = 'NA'
        block = "practice_trial1"

        rows.append([p, continuum_member, cond, itemnum, group, block])

    for p in practice_stims2:
        group = l
        continuum_member = int(re.findall('\d+', p)[0])
        cond = 'NA'
        itemnum = 'NA'
        block = "practice_trial2"

        rows.append([p, continuum_member, cond, itemnum, group, block])



with open('../../PCIbex experiments git/chunk_includes/exp.csv', 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(rows)