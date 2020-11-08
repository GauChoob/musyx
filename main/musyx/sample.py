import os
from musyx.constants import snd_ProjectData,sdp_SampleTableAddress,sdp_NumberOfSamples
import wave
from musyx.utils import transform_id,ID_SAMPLE
from musyx.utils import littledata_to_word, twobytearray
    
    
    
def get_sample_datum(data,index):
    datum = data[index//2]
    if(index % 2 == 0):
        return (datum & 0b11110000)
    else:
        return (datum & 0b00001111) << 4
    
def sample2wav(romfile,musyx_position,samplenames):
    #Designed for MUConv.exe v1.04
    #Takes rom data and converts back to the equivalent .wav files
    #The generated .wav files should compile back into exactly the same data
    #That being said, some parameters could be slightly different from the original .wav file as multiple .wav values sometimes round to the same data when parsed
    #Arguments:
    #   romfile = rom file path
    #   musyx_position = position in the file of MusyX.
    #       MusyX is placed at the beginning of a rom bank chosen by the original developer
    #       e.g. in Magi Nation, MusyX is placed at rom bank 0x30
    #   samplenames = An empty array, or an array of sample names that will be used as savefiles
    with open(romfile,'rb') as f:
        rom = f.read()
    
    projectdata_pos = musyx_position+snd_ProjectData-0x4000
    
    sampletable_address_pos = projectdata_pos + sdp_SampleTableAddress
    sampletable_offset = littledata_to_word(rom,sampletable_address_pos)
    sampletable_pos = sampletable_offset + projectdata_pos
    
    sample_n_pos = projectdata_pos + sdp_NumberOfSamples
    sample_n = rom[sample_n_pos]
    
    targetdir = "python/out/Samples/"
    os.makedirs(targetdir, exist_ok=True)
    
    for i in range(sample_n):
        if(i < len(samplenames)):
            name = "sample_{:02X}_{}".format(transform_id(i,ID_SAMPLE),samplenames[i])
        else:
            name = "sample_{:02X}".format(transform_id(i,ID_SAMPLE))
        print(name)
        
        sample_address = littledata_to_word(rom,sampletable_pos+i*6+0)
        sample_length = littledata_to_word(rom,sampletable_pos+i*6+2)
        sample_quality = rom[sampletable_pos+i*6+4]
        sample_bank = rom[sampletable_pos+i*6+5]
        
        sample_pos = musyx_position + sample_address - 0x4000 + sample_bank*0x4000
        
        f = wave.open(targetdir+name+".wav","wb")
        f.setparams((1,2,8192 if sample_quality else 1920,sample_length*32,'NONE','not compressed'))
        
        bytearraylist = []
        datalength = 0
        
        for row in range(sample_length):
            data = rom[sample_pos + row*0x10:sample_pos + row*0x10 + 0x10]
            firstdatum = get_sample_datum(data,0)
            if(firstdatum == 0x70):
                reps = data[1] + 1
                datalength += reps*0x20
                bytearraylist.append(bytearray([0x00,0x00]*0x20*reps))
            else:
                for j in range(0x20):
                    datum = get_sample_datum(data,j)
                    datum = (datum - 0x80) % 0x100
                    datalength += 1
                    bytearraylist.append(bytearray([0x00,datum]))

        f.writeframes(b''.join(bytearraylist))
       
    
