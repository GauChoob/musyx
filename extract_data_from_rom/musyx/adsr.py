import os, math
from musyx.constants import snd_ProjectData,sdp_ADSRTableAddress,sdp_SFXTableAddress
from musyx.utils import transform_id,ID_ADSR
from musyx.utils import littledata_to_word, twobytearray
    
    
def _solve_cycles(cycles):
    if(cycles == 0):
        return 0
    milli = math.ceil(cycles/0.060000002384185791015625-16.6666660308837890625)
    return milli
    
def adsr2mxt(romfile,musyx_position,adsrnames):
    #Designed for MUConv.exe v1.04
    #Takes rom data and converts back to the equivalent mxt files
    #The generated mxt files should compile back into exactly the same data
    #That being said, some parameters could be slightly different from the original mxt file as multiple mxt values sometimes round to the same data when parsed
    #Arguments:
    #   romfile = rom file path
    #   musyx_position = position in the file of MusyX.
    #       MusyX is placed at the beginning of a rom bank chosen by the original developer
    #       e.g. in Magi Nation, MusyX is placed at rom bank 0x30
    #   adsrnames = An empty array, or an array of adsr names that will be used as savefiles
    with open(romfile,'rb') as f:
        rom = f.read()
    
    projectdata_pos = musyx_position+snd_ProjectData-0x4000
    adsrtable_address_pos = projectdata_pos + sdp_ADSRTableAddress
    sfx_address_pos = projectdata_pos + sdp_SFXTableAddress
    adsrbase_offset = littledata_to_word(rom,adsrtable_address_pos)
    sfxbase_offset = littledata_to_word(rom,sfx_address_pos)
    if(adsrbase_offset == sfxbase_offset):
        print("There seems to be 0 ADSR entries. Aborting")
        return
    adsrbase_pos = adsrbase_offset + projectdata_pos
    
    
    targetdir = "python/out/Tables/"
    os.makedirs(targetdir, exist_ok=True)
    
    i = 0
    adsrs = []
    while True:
        if(i < len(adsrnames)):
            name = "adsr_{:03}_{}".format(transform_id(i,ID_ADSR),adsrnames[i])
        else:
            name = "adsr_{:03}".format(transform_id(i,ID_ADSR))
        print(name)
        newaddress = littledata_to_word(rom,adsrbase_pos+i*2)
        data_pos = newaddress + adsrbase_pos
        if(i == 0):
            adsrtable_endpos = newaddress + adsrbase_pos

        with open(targetdir+name+".mxt","wb") as f:
            data = [
                littledata_to_word(rom,data_pos+0),
                littledata_to_word(rom,data_pos+2),
                rom[data_pos+4],
                littledata_to_word(rom,data_pos+5)
            ]
            for d in [1,3]: #Signed values
                if(data[d] >= 256**2//2):
                    data[d] = data[d] - 256**2
            attack = _solve_cycles(math.floor(3840/(data[0]-0.5)))
            decay = _solve_cycles((-1)*(0x0F-data[2])*0x100//data[1])
            sustain = math.ceil((data[2]-0.5)/0.003662109375)
            release = _solve_cycles((-1)*data[2]*0x100//data[3])
            f.write(twobytearray(attack))
            f.write(twobytearray(decay))
            f.write(twobytearray(sustain))
            f.write(twobytearray(release))
            
        i += 1
        if(adsrbase_pos+i*2 == adsrtable_endpos) or (i>256): #Quit when you reach the address of the first adsr
            break
    
