import os, math
from musyx.constants import snd_ProjectData,sdp_SampleMapMacro_Address,sdp_SoundMacroLookupTable,sdp_NumberOfSampleMapEntries
from musyx.utils import transform_id,ID_ADSR,ID_SAMPLE,ID_MACRO
from musyx.utils import littledata_to_word, bigdata_to_word

# Instead of doing a//b, do int(a/b) to simulate an idiv
# https://stackoverflow.com/questions/19919387/in-python-what-is-a-good-way-to-round-towards-zero-in-integer-division

command_lengths = {
                    #The commands marked "UNTESTED" have not been checked to see if the function works or
                    # if the function transcribes correctly
                    #(i.e. my test dataset did not use these commands)
                    #These commands might have errors or cause crashes
                    #However, in theory they work unless I made some coding mistakes or misread the disassembly code
                    #The other commands that are unmarked have been generally tested and generally seem to assemble back into exact original data
    0x00: 1, #00 END
    0x23: 1, #01 STOP                                   UNTESTED
    0x15: 4, #02 SPLITKEY                               UNTESTED
    0x17: 4, #03 SPLITVEL                               UNTESTED
    0x20: 1, #04 RESET_MOD
    0x05: 4, #05 LOOP
    0x06: 3, #06 GOTO                                   UNTESTED
    0x04: 5, #07 WAIT
    0x26: 3, #08 PLAYMACRO                              UNTESTED
    0x22: 1, #09 PLAYKEYSAMPLE                          UNTESTED
    0x1F: 1, #0A STOP_MOD
    0x0E: 2, #0B SETVOICE
    0x0F: 2, #0C SETADSR
    0x09: 2, #0D SETVOLUME
    0x0A: 2, #0E PANNING
    0x11: 3, #0F ENVELOPE (DESCENDING)
    0x27: 3, #0F ENVELOPE (ASCENDING)
    0x21: 2, #10 STARTSAMPLE
    0x0D: 1, #11 VOICE_OFF
    0x12: 2, #12 KEYOFF                                 UNTESTED
    0x16: 4, #13 SPLITRND                               UNTESTED
    0x0C: 2, #14 VOICE_ON
    0x10: 2, #15 SETNOISE
    0x1B: 5, #16 PORTLAST                               UNTESTED
    0x0B: 5, #17 RNDNOTE                                UNTESTED
    0x08: 4, #18 ADDNOTE
    0x07: 3, #19 SETNOTE
    0x14: 3, #1A LASTNOTE                               UNTESTED
    0x02: 6, #1B PORTAMENTO                             UNTESTED
    0x01: 4, #1C VIBRATO
    0x03: 6, #1D PITCHSWEEP
    0x13: 2, #1E HARDENVELOPE
    0x1D: 5, #1F PWN_START
    0x1E: 5, #20 PWN_UPDATE                             UNTESTED
    0x24: 2, #21 PWN_FIXED                              UNTESTED
    0x25: 1, #22 PWN_VELOCITY                           UNTESTED
    0x1A: 2, #23 SENDFLAG
             #24 SAMPLEMAP (NO OPCODE - length = 1)
    0x28: 2, #25 CURRENTVOL
    #0x0C: 2,#26 WAVE_ON (SAME OPCODE AS 14 VOICE_ON)
    0x29: 3, #27 ADD_SET_PRIO
    0x18: 3, #28 TRAP_KEYOFF                            UNTESTED
    0x19: 1, #29 UNTRAP_KEYOFF                          UNTESTED
    #Only undefined opcode is 1C
}

command_names = {
    0x00: "00 END",
    0x23: "01 STOP",
    0x15: "02 SPLITKEY",
    0x17: "03 SPLITVEL",
    0x20: "04 RESET_MOD",
    0x05: "05 LOOP",
    0x06: "06 GOTO",
    0x04: "07 WAIT",
    0x26: "08 PLAYMACRO",
    0x22: "09 PLAYKEYSAMPLE",
    0x1F: "0A STOP_MOD",
    0x0E: "0B SETVOICE",
    0x0F: "0C SETADSR",
    0x09: "0D SETVOLUME",
    0x0A: "0E PANNING",
    0x11: "0F ENVELOPE (DESCENDING)",
    0x27: "0F ENVELOPE (ASCENDING)",
    0x21: "10 STARTSAMPLE",
    0x0D: "11 VOICE_OFF",
    0x12: "12 KEYOFF",
    0x16: "13 SPLITRND",
    0x0C: "14 VOICE_ON / 26 WAVE_ON",
    0x10: "15 SETNOISE",
    0x1B: "16 PORTLAST",
    0x0B: "17 RNDNOTE",
    0x08: "18 ADDNOTE",
    0x07: "19 SETNOTE",
    0x14: "1A LASTNOTE",
    0x02: "1B PORTAMENTO",
    0x01: "1C VIBRATO",
    0x03: "1D PITCHSWEEP",
    0x13: "1E HARDENVELOPE",
    0x1D: "1F PWN_START",
    0x1E: "20 PWN_UPDATE",
    0x24: "21 PWN_FIXED",
    0x25: "22 PWN_VELOCITY",
    0x1A: "23 SENDFLAG",
          #24 SAMPLEMAP (NO OPCODE - length = 1)
    0x28: "25 CURRENTVOL",
    0x29: "27 ADD_SET_PRIO",
    0x18: "28 TRAP_KEYOFF",
    0x19: "29 UNTRAP_KEYOFF",
    #Only undefined opcode is 1C
}
    
    
class SoundMacro:
    def __init__(self,musyx_position,address,samplemap,index):
        self.address = address
        self.rom_pos = musyx_position + snd_ProjectData-0x4000 + sdp_SoundMacroLookupTable + address
        self.predictedvoice = None
        self.steps = []
        self.name = None
        self.samplemap = samplemap
        self.index = index
        
    def init_steps(self,rom):
        # inits steps, and returns the total size of the data
        curpos = self.rom_pos
        curadr = self.address
        if(self.samplemap):
            for i in range(self.samplemap):
                length = 1
                self.steps.append(SoundMacroStep(rom,curpos,curadr,length,i,True,self.index))
                curpos += length
                curadr += length
            assert rom[curpos] == 0x00 #Last entry is stop
            self.steps.append(SoundMacroStep(rom,curpos,curadr,1,self.samplemap,False,self.index))
            curpos += 1
            curadr += 1
        else:
            i = 0
            opcode = -1
            while opcode != 0:
                opcode = rom[curpos]
                length = command_lengths[opcode]
                self.steps.append(SoundMacroStep(rom,curpos,curadr,length,i,False,self.index))
                i += 1
                curpos += length
                curadr += length
        return curpos - self.rom_pos
        
    def mxm_steps(self,macros):
        for step in self.steps:
            step.create_mxm(macros)
            
        
class SoundMacroStep:
    def __init__(self,rom,curpos,curadr,length,step,samplemap,parent_index):
        self.samplemap = samplemap
        self.raw = rom[curpos:curpos+length]
        self.step = step
        self.address = curadr
        self.mxm = bytearray([0,0,0,0,0,0,0,0])
        self.parent_index = parent_index

    def val(self,index,size=1,signed=False):
        if(size==1):
            v = self.raw[index]
        elif(size==2):
            v = self.raw[index]*256+self.raw[index+1]
        else:
            raise ValueError
        if(signed):
            if(v >= 256**size//2):
                v = v - 256**size
        return v
    
    def bit(self,index,bit):
        if(self.raw[index] & (1<<bit)):
            return True
        return False
        
    def store(self,value,index,size=1):
        assert abs(value) < 256**size #Make sure value is not too big
        value = value % 256**size #Fix negative numbers
        if(size==1):
            self.mxm[index] = value
        elif(size==2):
            self.mxm[index] = value % 256
            self.mxm[index+1] = (value - (value % 256))//256
        
    def solve_address(self,macros,index):
        adr = self.val(index,2)
        for i in range(len(macros)):
            for j in range(len(macros[i].steps)):
                if(macros[i].steps[j].address == adr):
                    return i,j #SoundMacroID, SoundMacroStep
        raise ValueError #not found
        
    def solve_relative(self,macros,parent_index,step,index):
        rel = self.val(index,2,True)
        target = macros[parent_index].steps[step].address + len(macros[parent_index].steps[step].raw) #End of the step
        target_adr = target + rel #rel being a negative number
        for i in range(len(macros[parent_index].steps)):
            if(macros[parent_index].steps[i].address == target_adr):
                return i #SoundMacroStep
        raise ValueError #not found
        
    def solve_cycles(self,index):
        cycles = self.val(index,2)
        return self._solve_cycles(cycles)
    def _solve_cycles(self,cycles):
        if(cycles == 0):
            return 0
        milli = math.ceil(cycles/0.060000002384185791015625-16.6666660308837890625)
        return milli

    def solve_volume(self,index):
        vol = self.val(index,1)
        return vol*8
        
    def solve_keycents(self,index):
        keycents = self.val(index,2,True)
        return self._solve_keycents(keycents)
    def _solve_keycents(self,keycents):
        mult = 1
        if(keycents < 0):
            keycents = -keycents
            mult = -1
        key = keycents//256
        cent = keycents - key*256
        key *= mult
        cent *= mult
        
        cent = cent/2.575757503509521484375
        if(cent < 0):
            cent = max(-99,math.floor(cent))
        else:
            cent = min(99,math.ceil(cent))
        return key,cent
    def solve_cents(self,index):
        cent = self.val(index,1,False) #Assume positive number, but this is probably opcode-specific. More research required
        key,cent = self._solve_keycents(cent)
        assert key == 0
        return cent
        
    def debug(self):
        if(self.samplemap):
            out = "{:27}{:04X}    ".format("24 SAMPLEMAP",self.address+sdp_SoundMacroLookupTable)
        else:
            out = "{:27}{:04X}    ".format(command_names[self.raw[0]],self.address+sdp_SoundMacroLookupTable)
        rawt = ""
        for i in self.raw:
            rawt += " {:02X}".format(i)
        rawt = "{:18}    ".format(rawt)
        out += rawt
        for i in self.mxm:
            out += " {:02X}".format(i)
        out += "\n"
        return out
    
    def create_mxm(self,macros):
        if(self.samplemap):
            self.store(0x24,0)
            id = self.val(0)
            id = transform_id(id,ID_SAMPLE)
            self.store(id,1,2)
        else:
            opcode = self.val(0)
            
            command_lengths.pop(opcode,None)
            
            if(opcode == 0x00): #END
                self.store(0x00,0)
            elif(opcode == 0x23): #STOP
                self.store(0x01,0)
            elif(opcode == 0x15): #SPLITKEY
                self.store(0x02,0)
                key = self.val(1)
                self.store(key,1)
                id,step = self.solve_address(macros,2)
                id = transform_id(id,ID_MACRO)
                self.store(id,2,2)
                self.store(step,4,2)
            elif(opcode == 0x17): #SPLITVEL
                self.store(0x03,0)
                vel = self.val(1)
                self.store(vel,1)
                id,step = self.solve_address(macros,2)
                id = transform_id(id,ID_MACRO)
                self.store(id,2,2)
                self.store(step,4,2)
            elif(opcode == 0x20): #RESET_MOD
                self.store(0x04,0)
            elif(opcode == 0x05): #LOOP
                self.store(0x05,0)
                times = self.val(1)
                self.store(times,6)
                step = self.solve_relative(macros,self.parent_index,self.step,2)
                self.store(step,4,2)
            elif(opcode == 0x06): #GOTO
                self.store(0x06,0)
                id,step = self.solve_address(macros,1)
                id = transform_id(id,ID_MACRO)
                self.store(id,2,2)
                self.store(step,4,2)
            elif(opcode == 0x04): #WAIT
                self.store(0x07,0)
                bool1 = self.val(1)
                self.store(bool1,1)
                bool2 = self.val(2)
                self.store(bool2,2)
                cycles = self.val(3,2)
                if(cycles == 0x0001 and bool2 == 1):
                    milli = 0xFFFF
                elif(cycles == 0x0000):
                    milli = 0xFFFF
                else:
                    milli = self.solve_cycles(3) - 1
                self.store(milli,6,2)
            elif(opcode == 0x26): #PLAYMACRO
                self.store(0x08,0)
                voice = self.val(1)
                if(voice != 0xFF):
                    voice = voice & 0b11
                self.store(voice,1)
                id = self.val(2)
                id = transform_id(id,ID_MACRO)
                self.store(id,2,2)
                bool = self.bit(1,7)
                self.store(bool,4)
            elif(opcode == 0x22): #PLAYKEYSAMPLE
                self.store(0x09,0)
            elif(opcode == 0x1F): #STOP_MOD
                self.store(0x0A,0)
            elif(opcode == 0x0E): #SETVOICE
                self.store(0x0B,0)
                voice = self.val(1)
                if(voice == 0xFF):
                    self.store(0xFF,1)
                    self.store(0,4)
                    self.store(0,5)
                else:
                    voice = voice & 0b11
                    macros[self.parent_index].predictedvoice = voice # Set the predicted voice to discriminate between VOICE_ON and WAVE_ON
                    self.store(voice,1)
                    bool1 = self.bit(1,7)
                    self.store(bool1,4)
                    bool2 = self.bit(1,4)
                    self.store(bool2,5)
            elif(opcode == 0x0F): #SETADSR
                self.store(0x0C,0)
                id = self.val(1)
                id = transform_id(id,ID_ADSR)
                self.store(id,1,2)
            elif(opcode == 0x09): #SETVOLUME
                self.store(0x0D,0)
                vol = self.solve_volume(1)
                self.store(vol,1)
            elif(opcode == 0x0A): #PANNING
                self.store(0x0E,0)
                pan = self.val(1)
                if(pan == 0):
                    self.store(0,1)
                elif(pan == 1):
                    self.store(64,1)
                elif(pan == 2):
                    self.store(127,1)
                else:
                    raise ValueError
            elif(opcode == 0x11): #ENVELOPE (descending)
                self.store(0x0F,0)
                self.store(0,1)
                x = int(0x0F00/self.val(1,2,True))*(-1)
                milli = self._solve_cycles(x)
                self.store(milli,6,2)
            elif(opcode == 0x27): #ENVELOPE (ascending)
                self.store(0x0F,0)
                self.store(1,1)
                x = int(0x0F00/self.val(1,2,True))*(1)
                milli = self._solve_cycles(x)
                self.store(milli,6,2)
            elif(opcode == 0x21): #STARTSAMPLE
                self.store(0x10,0)
                id = self.val(1)
                id = transform_id(id,ID_SAMPLE)
                self.store(id,1,2)
            elif(opcode == 0x0D): #VOICE_OFF
                self.store(0x11,0)
            elif(opcode == 0x12): #KEYOFF
                self.store(0x12,0)
                voice = self.val(1)
                self.store(voice,1)
            elif(opcode == 0x16): #SPLITRND
                self.store(0x13,0)
                rand = self.val(1)
                self.store(rand,1)
                id,step = self.solve_address(macros,2)
                id = transform_id(id,ID_MACRO)
                self.store(id,2,2)
                self.store(step,4,2)
            elif(opcode == 0x0C):
                if(macros[self.parent_index].predictedvoice == 2): #WAVE_ON
                    self.store(0x26,0)
                    id = self.val(1)
                    if(id == 0xFF):
                        self.store(0,1)
                    else:
                        id = transform_id(id,ID_SAMPLE)
                        self.store(id,1,2)
                elif(macros[self.parent_index].predictedvoice in [0,1,3]): #VOICE_ON
                    self.store(0x14,0)
                    duty = self.val(1)
                    self.store(duty,1)
                else:
                    print("Macro {:02X} - guessing step {} to be VOICE_ON, but it could also be WAVE_ON".format(self.parent_index,self.step))
                    self.store(0x14,0)
                    duty = self.val(1)
                    self.store(duty,1)
            elif(opcode == 0x10): #SETNOISE
                self.store(0x15,0)
                macros[self.parent_index].predictedvoice = 3 # Set the predicted voice
                byte = self.val(1)
                clock = (byte & 0b11110000) >> 4
                self.store(clock,1)
                step = (byte & 0b00001000) >> 3
                self.store(step,2)
                freq = byte & 0b00000111
                self.store(freq,3)
            elif(opcode == 0x1B): #PORTLAST
                self.store(0x16,0)
                key,cent = self.solve_keycents(1)
                self.store(key,1)
                self.store(cent,2)
                milli = solve_cycles(3)
                self.store(milli,6)
            elif(opcode == 0x0B): #RNDNOTE
                self.store(0x17,0)
                bool1 = self.bit(1,7)
                self.store(bool1,4)
                bool2 = self.bit(1,0)
                self.store(bool2,5)
                note1 = self.val(2)
                self.store(note1,1)
                note2 = self.val(3)
                self.store(note2,3)
                cent = self.solve_cents(4) #cent is always positive
                self.store(cent,2)
            elif(opcode == 0x08): #ADDNOTE
                self.store(0x18,0)
                bool = 1-self.val(1)
                self.store(bool,3)
                add = self.val(2,1,True)
                self.store(add,1)
                key,cent = self.solve_keycents(2) #technically key is undefined, but maybe cent derives its sign from add (i.e. maybe MusyX has an interpretation bug here - I'm not sure so I'll guess - maybe this is changed in more recent versions of MuConv)
                self.store(cent,2)
            elif(opcode == 0x07): #SETNOTE
                self.store(0x19,0)
                key = self.val(1)
                self.store(key,1)
                key,cent = self.solve_keycents(1) #technically key is undefined, but maybe cent derives its sign from key (i.e. maybe MusyX has an interpretation bug here - I'm not sure so I'll guess - maybe this is changed in more recent versions of MuConv)
                self.store(cent,2)
            elif(opcode == 0x02): #PORTAMENTO
                self.store(0x1B,0)
                key,cent = self.solve_keycents(1)
                self.store(key,1)
                self.store(cent,2)
                milli = solve_cycles(3)
                self.store(milli,6,2)
                bool = self.val(5)
                self.store(bool,3)
            elif(opcode == 0x01): #VIBRATO
                self.store(0x1C,0)
                x = self.val(3)
                milli = self._solve_cycles(x)
                milli *= 2
                self.store(milli,6,2)
                y_ddiv_x = self.val(1,2,True)
                y = y_ddiv_x*x
                key,cent = self._solve_keycents(y)
                self.store(key,1)
                self.store(cent,2)
            elif(opcode == 0x03): #PITCHSWEEP
                self.store(0x1D,0)
                bool = self.bit(5,7)
                self.store(bool,5)
                key,cent = self.solve_keycents(3)
                self.store(key,1)
                self.store(cent,2)
                y = self.val(3,2,True)
                y_ddiv_x = self.val(1,2,True)
                x = int(y/y_ddiv_x)
                milli = self._solve_cycles(x)
                self.store(milli,6,2)
            elif(opcode == 0x13): #HARDENVELOPE
                self.store(0x1E,0)
                bool = self.bit(1,3)
                self.store(bool,1)
                x = self.val(1) & 0b111
                x = math.ceil((x-0.5)/0.0042666667141020298004150390625)
                self.store(x,6,2)
            elif(opcode == 0x1D): #PWN_START
                macros[self.parent_index].predictedvoice = 2 # Set the predicted voice
                self.store(0x1F,0)
                low = self.val(1)
                self.store(low,1)
                high = self.val(2)
                self.store(high,2)
                delta = (high-low)*256
                x = self.val(3,2)
                if(x == 0):
                    self.store(0,3,2)
                else:
                    cycle = int(delta/x)
                    milli = self._solve_cycles(cycle)
                    self.store(milli,3,2)
            elif(opcode == 0x1E): #PWN_UPDATE
                macros[self.parent_index].predictedvoice = 2 # Set the predicted voice
                self.store(0x20,0) #same as PWN_START except for this line
                low = self.val(1)
                self.store(low,1)
                high = self.val(2)
                self.store(high,2)
                delta = (high-low)*256
                x = self.val(3,2)
                if(x == 0):
                    self.store(0,3,2)
                else:
                    cycle = int(delta/x)
                    milli = self._solve_cycles(cycle)
                    self.store(milli,3,2)
            elif(opcode == 0x24): #PWN_FIXED
                macros[self.parent_index].predictedvoice = 2 # Set the predicted voice
                self.store(0x21,0)
                duty = self.val(1)
                self.store(duty,1)
            elif(opcode == 0x25): #PWN_VELOCITY
                macros[self.parent_index].predictedvoice = 2 # Set the predicted voice
                self.store(0x22,0)
            elif(opcode == 0x1A): #SENDFLAG
                self.store(0x23,0)
                flag = self.val(1)
                self.store(flag,1)
                                  #SAMPLEMAP - see above
            elif(opcode == 0x28): #CURRENTVOL
                self.store(0x25,0)
                vol = self.solve_volume(1)
                self.store(vol,1)
                                  #WAVE_ON - see above
            elif(opcode == 0x29): #ADD_SET_PRIO
                self.store(0x27,0)
                prio = self.val(2)
                self.store(prio,1)
                bool = self.val(1)
                self.store(bool,2)
            elif(opcode == 0x18): #TRAP_KEYOFF
                self.store(0x28,0)
                id,step = self.solve_address(macros,1)
                id = transform_id(id,ID_MACRO)
                self.store(id,2,2)
                self.store(step,4,2)
            elif(opcode == 0x19): #UNTRAP_KEYOFF
                self.store(0x19,0)
            else:
                raise ValueError
    
def macro2mxm(romfile,musyx_position,macronames):
    #Designed for MUConv.exe v1.04
    #Takes rom data and converts back to the equivalent mxm files
    #The generated mxm files should compile back into exactly the same data
    #That being said, some parameters could be slightly different from the original mxm file as multiple mxm values sometimes round to the same data when parsed
    #Arguments:
    #   songfile_name = rom file path
    #   musyx_position = position in the file of MusyX.
    #       MusyX is placed at the beginning of a rom bank chosen by the original developer
    #       e.g. in Magi Nation, MusyX is placed at rom bank 0x30
    #   macronames = An empty array, or an array of macro names that will be used as savefiles
    with open(romfile,'rb') as f:
        rom = f.read()
    
    projectdata_pos = musyx_position+snd_ProjectData-0x4000
    macrobase_pos = projectdata_pos + sdp_SoundMacroLookupTable
    macrosamplemap_pos = projectdata_pos + sdp_SampleMapMacro_Address
    macrosamplemap_n_pos = projectdata_pos + sdp_NumberOfSampleMapEntries
    samplemap_pos = littledata_to_word(rom,macrosamplemap_pos) - sdp_SoundMacroLookupTable
    samplemap_n = rom[macrosamplemap_n_pos]
    samplemap_found = False
    
    i = 0
    macros = []
    while True:
        samplemap_entries = 0
        newaddress = littledata_to_word(rom,macrobase_pos+i*2)
        if(i == 0):
            macrotable_endpos = newaddress + macrobase_pos
        if(newaddress == samplemap_pos):
            samplemap_entries = samplemap_n
            samplemap_found = True
        macros.append(SoundMacro(musyx_position,newaddress,samplemap_entries,i))
        i += 1
        if(macrobase_pos+i*2 == macrotable_endpos) or (i>256): #Quit when you reach the address of the first macro
            break
            
    for i in range(len(macros)):
        if(i < len(macronames)):
            name = "macro_{:03}_{}".format(transform_id(i,ID_MACRO),macronames[i])
        else:
            name = "macro_{:03}".format(transform_id(i,ID_MACRO))
        macros[i].name = name
        macros[i].init_steps(rom)
           
    for i in range(len(macros)):
        macros[i].mxm_steps(macros) 
        
    targetdir_base = "python/out/"
    targetdir = "python/out/Soundmacros/"
    os.makedirs(targetdir, exist_ok=True)
    for macro in macros:
        print(macro.name)
        with open(targetdir+macro.name+".mxm","wb") as f:
            for step in macro.steps:
                f.write(step.mxm)
    with open(targetdir_base+"macrodebug.txt","w") as f:
        for macro in macros:
            f.write("{:16} {:06X}\n".format(macro.name,macro.address+sdp_SoundMacroLookupTable))
            for step in macro.steps:
                f.write(step.debug())
            f.write("\n\n")
        
    #for i in command_lengths:
    #    print("{:02X}".format(i))

