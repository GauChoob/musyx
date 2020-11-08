import os
from musyx.constants import snd_ProjectData,sdp_SFXTableAddress,sdp_NumberOfSFXs
from musyx.utils import transform_id, ID_SFX, ID_MACRO
from musyx.utils import littledata_to_word


    
def sfx2info(romfile,musyx_position,sfxnames):
    #Compatible with MusyX > GM2SONG 1.03 and possibly other versions
    #
    #Arguments:
    #   romfile = rom file path
    #   musyx_position = position in the file of MusyX.
    #       MusyX is placed at the beginning of a rom bank chosen by the original developer
    #       e.g. in Magi Nation, MusyX is placed at rom bank 0x30
    #   sfxnames = An empty array, or an array of sfx names that will be used as savefiles
    with open(romfile,'rb') as f:
        rom = f.read()
    
    projectdata_pos = musyx_position+snd_ProjectData-0x4000
    sfxtable_address = projectdata_pos + littledata_to_word(rom,projectdata_pos + sdp_SFXTableAddress)
    sfxtable_len = rom[projectdata_pos+sdp_NumberOfSFXs]
    
    targetdir_base = "python/out/"
    os.makedirs(targetdir_base, exist_ok=True)
    
    with open(targetdir_base+"sfxinfo.txt","w") as f:
        f.write("{:32} - {:3} {:3} {:3} {:3}\n".format("Name","Macro","Pri","Key","Vel"))
        for i in range(sfxtable_len):
            if(i < len(sfxnames)):
                name = "sfx_{:03}_{}".format(transform_id(i,ID_SFX),sfxnames[i])
            else:
                name = "sfx_{:03}".format(transform_id(i,ID_SFX))
            print(name)
            
            pos = sfxtable_address + i*4
            
            index = transform_id(rom[pos],ID_MACRO)
            priority = rom[pos+1]
            defkey = rom[pos+2]
            defvel = rom[pos+3]*8
        
            f.write("{:32} - {:3} {:3} {:3} {:3}\n".format(name,index,priority,defkey,defvel))
        
        
    

