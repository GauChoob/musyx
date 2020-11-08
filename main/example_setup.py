import musyx.info

musyxpos = 0x30*0x4000 #(MusyX is located at bank 0x30)
romfile = r"C:\example.gbc"

musyx.info.get_NUMBER_OF_SONGS(romfile,musyxpos)
musyx.info.get_NUMBER_OF_MACROS(romfile,musyxpos)