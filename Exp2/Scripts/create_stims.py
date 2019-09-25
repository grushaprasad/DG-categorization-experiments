import librosa, random, subprocess, sys, glob, re, csv
import numpy as np
from copy import deepcopy
import os
import glob


soundfile_path = '../../target_stims/wav/'
precursor_path = '../combined_stims/'

stim_files = ['a_dg09_CV.wav', 'a_dg10_CV.wav', 'a_dg11_CV.wav', 'a_dg12_CV.wav', 'a_dg13_CV.wav','a_dg14_CV.wav','a_dg15_CV.wav','a_dg16_CV.wav']

practice_stim_files = ['a_dg01_CV.wav', 'a_dg02_CV.wav', 'a_dg19_CV.wav', 'a_dg20_CV.wav']


# basic parameters

offset = 0.098 
duration = 0.365
rms_target = 0.06362659

sil_dur = 45
tone_dur = 70.0/1000.0 # 70 ms duration for each tone
isi_dur = 30.0/1000.0 # 30 ms silence between successive tones
sil_dur = 50.0/1000.0 # 50 ms silence between standard tone and speech
ramp_dur = 5.0/1000.0
sr = 11025


# Experiment parameters
num_lists = 4
num_conds = 2
num_trials_per_cond = 10

# Mean parameters

standard_freq = 2300.0  # standard tone at 2300 Hz

low = np.arange(2050.0, 2350.0+30.0, 30.0) # mean 2200
mid = np.arange(2350.0, 2650.0+30.0, 30.0) # mean 2500
high = np.arange(2650.0, 2950.0+50.0, 30.0) # mean 2800



def rms_amplitude(x):
    rms = np.sqrt(np.mean(x**2.0))
    return rms

def scale_rms(x, rms_target):
    rms_x = rms_amplitude(x)
    s = (rms_target / rms_x)
    y = s*x
    return y

def swap_positions(np_array, ind1, ind2):
    input_seq[[ix1, ix2]] = input_seq[[ix2, ix1]]


def shuffle_list(l, num_repeat):
    #new_l = l*num_repeat
    random.shuffle(l)
    combined = l
    
    for i in range(num_repeat-1):
        new_l = deepcopy(l)
        random.shuffle(new_l)
        # print(i, type(new_l), type(combined))
        # print(len(new_l))
        # print(new_l[1])
        if new_l[-1] == combined[-1]:  #makes sure no two items occur back to back
            new_l[[-1, -2]] = new_l[[-2, -1]]


        combined = np.concatenate((combined, new_l),0)

    return(combined)


def create_precursor_freqs(l1,l2=[], num_repeat=1):
    l1 = shuffle_list(l1, num_repeat)
    if len(l2) > 0:
        l2 = shuffle_list(l2, num_repeat)

    return(np.concatenate((l1, l2),0))

def create_precursors(l, tone_sr, tone_dur, num_standard=1, num_silence=1, num_standard_isi=0):   
    linear_ramp = np.linspace(0.0, 1.0, int(ramp_dur*tone_sr))
    mask = np.ones(int((tone_dur - 2.0*ramp_dur)*tone_sr))
    mask = np.concatenate((linear_ramp, mask, linear_ramp[::-1]))
    standard_tone = mask*librosa.core.tone(standard_freq, tone_sr, duration=tone_dur)[0:771] 

    sil = np.zeros(int(sil_dur * tone_sr))
    isi = np.zeros(int(isi_dur * tone_sr))

    precursor_list = [0]*len(l)
    for i, item in enumerate(l):

        curr_tones = [mask*librosa.core.tone(freq, tone_sr, duration=tone_dur)[0:771] for freq in item]

        curr_precursor = np.concatenate([np.concatenate((tone, isi)) for tone in curr_tones] + ([standard_tone] + [isi])*num_standard + [sil]*num_silence)

        precursor_list[i] = scale_rms(curr_precursor, rms_target)
    return(precursor_list)




# Delete existing files in combined_stim

files = glob.glob(precursor_path+'*')
for f in files:
    os.remove(f)

## Reading in the stimuli
dg_stims = [0] * len(stim_files)


for i,stim in enumerate(stim_files):
    curr_file = soundfile_path+stim
    curr_dga, curr_sr = librosa.core.load(curr_file, sr = sr, offset = offset, duration = duration)  #if I don't have sr, the sr becomes 22050. Why?
    dg_stims[i] = (curr_dga, stim)

dg_stims = dg_stims*num_conds*num_trials_per_cond




lowmean= [create_precursor_freqs(low, num_repeat = 2) for i in range(num_trials_per_cond*len(stim_files))]

highmean = [create_precursor_freqs(high, num_repeat = 2) for i in range(num_trials_per_cond*len(stim_files))]


low_precursors = create_precursors(lowmean, sr, tone_dur)
low_precursors = [(item, 'low') for item in low_precursors] 

high_precursors = create_precursors(highmean, sr, tone_dur)
high_precursors = [(item, 'high') for item in high_precursors]



all_precursors = low_precursors + high_precursors


# Create stims and stim list for block 2

all_stims = [0]*len(all_precursors)

for i, item in enumerate(all_precursors):
    curr_stim = np.concatenate((item[0], dg_stims[i][0]))
    fname = 'dg%s_%s_%s.wav'%(re.findall('\d+', dg_stims[i][1])[0], i+1, item[1])
    fpath = precursor_path + fname

    librosa.output.write_wav(fpath, curr_stim, sr=sr)
    #fpath = './tmp2/%s'%fname
    
    all_stims[i] = (fpath, dg_stims[i][1], item[1])

all_stims.insert(0, ['stim_fname', 'target_fname', 'condition'])

with open("exp2_block2_stimlist.csv",'w') as resultFile:
    wr = csv.writer(resultFile)
    wr.writerows(all_stims)


# Create stim list for block1
with open("exp2_block1_stimlist.csv",'w') as resultFile:
    resultFile.write('stim_fname\n')
    for i,item in enumerate(stim_files*num_trials_per_cond):
        resultFile.write(soundfile_path + item + '\n')
        if i == len(stim_files*num_trials_per_cond)-1: 
          resultFile.write(soundfile_path + item)
        else:
          resultFile.write(soundfile_path + item + '\n')



### Create practice stims

with open("exp2_practice1_stimlist.csv",'w') as resultFile:
    resultFile.write('stim_fname\n')
    #resultFile.write(soundfile_path + fname + '\n')
    for i,fname in enumerate(practice_stim_files):
        if i == len(stim_files*num_trials_per_cond)-1: 
          resultFile.write(soundfile_path + fname)
        else:
          resultFile.write(soundfile_path + fname + '\n')

lowmean = create_precursors([create_precursor_freqs(low, num_repeat=2)], sr, tone_dur)


highmean = create_precursors([create_precursor_freqs(high, num_repeat=2)], sr, tone_dur)


practice_precursors = [('low', lowmean), ('high',highmean)]*(len(practice_stim_files)/2)

practice_stims = [0]*len(practice_precursors)

for i, p in enumerate(practice_precursors):
    curr_file = soundfile_path+practice_stim_files[i]
    curr_dga, curr_sr = librosa.core.load(curr_file, sr = sr, offset = offset, duration = duration)

    curr_stim = np.concatenate((p[1][0], curr_dga))

    fname = 'dg%s_%s.wav'%(re.findall('\d+', practice_stim_files[i])[0], p[0])
    #fname = '%s_%s.wav'(practice_stim_files[i], p[0])
    fpath = precursor_path+fname
    #fpath = './tmp/%s'%fname

    practice_stims[i] = (fpath, practice_stim_files[i], p[0])

    librosa.output.write_wav(fpath, curr_stim, sr=sr)


practice_stims.insert(0, ['stim_fname', 'target_fname', 'condition'])
with open("exp2_practice2_stimlist.csv",'w') as resultFile:
    wr = csv.writer(resultFile)
    wr.writerows(practice_stims)


## To do: 
### Make correct practice precursors
### Set up on psychopy


