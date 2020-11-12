from musyx.constants import snd_ProjectData,sdp_SoundMacroLookupTable,sdp_NumberOfSongs

def get_NUMBER_OF_SONGS(romfile,musyx_position):
    with open(romfile,'rb') as f:
        rom = f.read()
    projectdata_pos = musyx_position+snd_ProjectData-0x4000
    number_of_songs = rom[projectdata_pos+sdp_NumberOfSongs]
    print("Number of songs: "+str(number_of_songs))
    return number_of_songs
    
def get_NUMBER_OF_MACROS(romfile,musyx_position):
    with open(romfile,'rb') as f:
        rom = f.read()
    
    projectdata_pos = musyx_position+snd_ProjectData-0x4000
    macrobase_pos = projectdata_pos + sdp_SoundMacroLookupTable
    
    number_of_macros = (rom[macrobase_pos]+rom[macrobase_pos+1]*256)//2
    print("Number of macros: "+str(number_of_macros))
    return number_of_macros
