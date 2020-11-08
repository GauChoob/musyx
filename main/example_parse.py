import musyx.song, musyx.macro, musyx.adsr, musyx.sample, musyx.sfx, musyx.utils

musyxpos = 0x30*0x4000 #(MusyX is located at bank 0x30)
romfile = r"C:\example.gbc"

macronames = ["SomeMacro","SomeOtherMacro","etc"] #You can also leave this undefined
musyx.macro.macro2mxm(romfile,musyxpos,macronames)

songnames = ["SomeSong","AnotherSong","etc"]
musyx.song.song2wav(romfile,musyxpos,songnames)

adsrnames = []
musyx.adsr.adsr2mxt(romfile,musyxpos,adsrnames)

samplenames = []
musyx.sample.sample2wav(romfile,musyxpos,samplenames) #Note that the order of samples might be scrambled in MusyX.exe. However, I seem to be able to extract songs, macros, adsr and sfx in the correct order.

sfxnames = []
musyx.sfx.sfx2info(romfile,musyxpos,sfxnames)