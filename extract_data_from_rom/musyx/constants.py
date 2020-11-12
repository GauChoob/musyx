snd_ProjectData = None #Depends on the ROM. Could be 0x5A79 or 0x5A87 or some similar address
assert snd_ProjectData is not None
sdp_ADSRTableAddress = 0
sdp_SFXTableAddress = 2
sdp_NumberOfSFXs = 4
sdp_SampleTableAddress = 5
sdp_NumberOfSamples = 7
sdp_SampleMapMacro_Address = 9
sdp_NumberOfSampleMapEntries = 0xB
sdp_SongTableAddress = 0xC
sdp_NumberOfSongs = 0xE
sdp_SoundMacroLookupTable = 0xF