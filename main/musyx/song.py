import os
import mido
import subprocess
import hashlib
from musyx.constants import snd_ProjectData,sdp_SongTableAddress,sdp_NumberOfSongs
from musyx.utils import transform_id, ID_SONG, ID_MACRO
from musyx.utils import bigdata_to_word, littledata_to_word


#def data_to_word(data):
#    assert len(data)==2
#    return data[1]*256+data[0]

targetdir_base = "python/out/"
targetdir = "python/out/Midifiles/"

class songTrackHeader:

    class BlockA:
        def __init__(self,file,position):
            self.startdelay_ticks = bigdata_to_word(file,position)
            self.index = file[position+2]
            assert self.index < 0xFE

    class BlockB:
        def __init__(self,file,position):
            self.loop_ticks = bigdata_to_word(file,position)
            self.loopflag = file[position+2]
            self.loopstart_block_pos =  bigdata_to_word(file,position + 3)
            self.loopstart_delay =  bigdata_to_word(file,position + 5)
            assert self.loopflag >= 0xFE
            
    def __init__(self,file,position,index):
        def isBlockB(file,position):
            # Returns True if BlockB
            # Returns False if BlockA
            return file[position+2] >= 0xFE
        
        self.channel = index
        
        assert isBlockB(file,position) == False
        self.blocka = [songTrackHeader.BlockA(file,position)]
        position += 3
        if(isBlockB(file,position) == False):
            self.blocka.append(songTrackHeader.BlockA(file,position)) #Optional second blocka
            position += 3
        assert isBlockB(file,position) == True
        self.blockb = songTrackHeader.BlockB(file,position)
        
    def getStartDelay(self,index):
        absolute_time = 0
        for i in range(index,-1,-1):
            absolute_time += self.blocka[i].startdelay_ticks
        return absolute_time
        
    def getLoopStart(self):
        if(self.hasLoop()):
            assert self.blockb.loopstart_block_pos % 3 == 0
            targetindex = self.blockb.loopstart_block_pos//3
            absolute_time = self.getStartDelay(targetindex) - self.blockb.loopstart_delay
            return songTrackData.eventController(absolute_time,102,0)
        return False
        
    def getLoopEnd(self):
        if(self.hasLoop()):
            assert self.blockb.loopstart_block_pos % 3 == 0
            targetindex = self.blockb.loopstart_block_pos//3
            absolute_time = self.getStartDelay(targetindex) + self.blockb.loop_ticks
            return songTrackData.eventController(absolute_time,103,0)
        return False
        
    def hasLoop(self):
        return self.blockb.loopflag == 0xFE
        
        
class songTrackData:
    class eventNote: #9
        # velocity 1-F for noteon
        # velocity 0 for noteoff
        def __init__(self,absolute_time,key,velocity):
            self.absolute_time = absolute_time
            self.key = key
            self.velocity = velocity
        def getMessage(self,channel,last_absolute_time):
            if(self.velocity>0):
                midi_velocity = 8*self.velocity - 4# - (self.key & 0b111)
                if(midi_velocity < 1):
                    midi_velocity = 1
                if(midi_velocity > 127): #This shouldn't happen
                    raise ValueError
            else:
                midi_velocity = 0
            delta_time = self.absolute_time - last_absolute_time
            assert delta_time >= 0
            return mido.Message('note_on',
                    time=delta_time,
                    channel=channel,
                    note=self.key,
                    velocity=midi_velocity), self.absolute_time
    class eventProgram: #C
        def __init__(self,absolute_time,program):
            self.absolute_time = absolute_time
            self.program = program
        def getMessage(self,channel,last_absolute_time):
            delta_time = self.absolute_time - last_absolute_time
            assert delta_time >= 0
            return mido.Message('program_change',
                    time=delta_time,
                    channel=channel,
                    program=self.program), self.absolute_time
    class eventController: #B
        def __init__(self,absolute_time,controller_number,value=0):
            self.absolute_time = absolute_time
            self.controller_number = controller_number
            self.value = value
        def getMessage(self,channel,last_absolute_time):
            delta_time = self.absolute_time - last_absolute_time
            assert delta_time >= 0
            return mido.Message('control_change',
                    time=delta_time,
                    channel=channel,
                    control=self.controller_number,
                    value=self.value), self.absolute_time
    
    END_OF_TRACK = 1
    EXCLUSIVE_PROGRAM_CHANGE = 2
    EXCLUSIVE_NOTE = 3
    PROGRAM_AND_NOTE_CHANGE = 4
    
    def __init__(self,file,position,trackheader,jndex):
        def isEndOfTrack(file,position):
            return file[position+0] == 0xF0 and file[position+1] == 0x00 and file[position+2] == 0xFF
        def isProgramChange(file,position):
            return not(file[position+0] & 0xF0)
        def noteHasProgramChange(file,position):
            return file[position+2] & 0b10000000
        def checkEventType(file,position):
            if isEndOfTrack(file,position):
                return songTrackData.END_OF_TRACK
            if isProgramChange(file,position):
                return songTrackData.EXCLUSIVE_PROGRAM_CHANGE
            if noteHasProgramChange(file,position):
                return songTrackData.PROGRAM_AND_NOTE_CHANGE
            return songTrackData.EXCLUSIVE_NOTE
        def getExclusiveProgram(events,absolute_time,firstnote,file,position):
            assert file[position+0] & 0xF0 == 0
            absolute_time += bigdata_to_word(file,position+0) & 0xFFF
            assert file[position+2] & 0b10000000 == 0b10000000
            program = file[position+2] & 0b01111111
            events.append(songTrackData.eventProgram(absolute_time,program))
            position += 3
            return events, absolute_time, position
        def getExclusiveNote(events,absolute_time,firstnote,file,position):
            velocity = (file[position+0] & 0xF0) >> 4
            assert velocity != 0
            absolute_time += bigdata_to_word(file,position+0) & 0xFFF
            assert file[position+2] & 0b10000000 == 0
            key = file[position+2] & 0b01111111
            position += 3
            position, duration = getVariableNoteEnd(file,position)
            events.append(songTrackData.eventNote(absolute_time,key,velocity))
            events.append(songTrackData.eventNote(absolute_time+duration,key,0))
            return events, absolute_time, position
        def getProgramAndNote(events,absolute_time,firstnote,file,position):
            #For the first note, usually the track header will give the offset to the first note
            #However, if the midi file has desynced programs and notes, for example
            #   Program at time 10 and Note at time 20,
            #The track header will give offset 10, but then will write the Program and Note at (absolute time) 20
            #(The expected behaviour would be track header offset 20 and Program and Note at absolute time 20 (or relative time 0)

            velocity = (file[position+0] & 0xF0) >> 4
            assert velocity != 0
            if(firstnote):
                firstnote_absolute_time = absolute_time  #Cause the midi quirk
            absolute_time += bigdata_to_word(file,position+0) & 0xFFF
            if(not(firstnote)):
                firstnote_absolute_time = absolute_time  #Cause the midi quirk
            assert file[position+2] & 0b10000000 == 0b10000000
            key = file[position+2] & 0b01111111
            assert file[position+3] & 0b10000000 == 0
            program = file[position+3]
            position += 4
            position, duration = getVariableNoteEnd(file,position)
            events.append(songTrackData.eventProgram(firstnote_absolute_time,program)) #Cause the midi quirk
            events.append(songTrackData.eventNote(absolute_time,key,velocity))
            events.append(songTrackData.eventNote(absolute_time+duration,key,0))
            return events, absolute_time, position
            
        self.events = []
        if(not(trackheader)):
            assert checkEventType(file,position) == songTrackData.END_OF_TRACK
            return
            
        absolute_time = trackheader.getStartDelay(jndex)
        firstnote = True
        while True:
            nextEventType = checkEventType(file,position)
            if(nextEventType == songTrackData.END_OF_TRACK):
                position += 3
                self.maxseek = position
                self.events.sort(key=lambda x:x.absolute_time)
                return
            if(nextEventType == songTrackData.EXCLUSIVE_PROGRAM_CHANGE):
                self.events,absolute_time,position = getExclusiveProgram(self.events,absolute_time,firstnote,file,position)
                assert checkEventType(file,position) == songTrackData.END_OF_TRACK
                position += 3
                self.maxseek = position
                self.events.sort(key=lambda x:x.absolute_time)
                return
            if(nextEventType == songTrackData.EXCLUSIVE_NOTE):
                self.events,absolute_time,position = getExclusiveNote(self.events,absolute_time,firstnote,file,position)
            if(nextEventType == songTrackData.PROGRAM_AND_NOTE_CHANGE):
                self.events,absolute_time,position = getProgramAndNote(self.events,absolute_time,firstnote,file,position)
            firstnote = False
            
def getVariableNoteEnd(file,position):
    if(file[position+0] & 0b10000000):
        duration = (file[position] & 0b01111111)*256+file[position+1]
        position += 2
        return position,duration
    else:
        duration = file[position]
        position += 1
        return position,duration
    
def song2gm(songfile,startposition,midifile):
    #Compatible with MusyX > GM2SONG 1.03 and possibly other versions
    #
    #Arguments:
    #   songfile = rom file path
    #   startposition = position in the rom file of the converted midi data. This is 0x84 bytes past the beginning of the MusyX datafile (e.g. expect the first song of a bank to start at address 0x4084)
    #   midifile = output filename
    with open(songfile,'rb') as f:
        songfile = f.read()
    
    fileend = startposition
    
    trackheadertable_pos = bigdata_to_word(songfile,startposition+0)
    trackdatatable_pos = bigdata_to_word(songfile,startposition+2)
    bpm = bigdata_to_word(songfile,startposition+4)
    
    trackheader_pos = [0]*4
    trackheader = [False]*4
    trackdata_pos = [0]*4
    trackdata = [False]*8
    for i in range(4):
        trackheader_pos[i] = bigdata_to_word(songfile,startposition+trackheadertable_pos+i*2)
        if(trackheader_pos[i]):
            trackheader[i] = songTrackHeader(songfile,startposition+trackheader_pos[i],i)
            for j in range(len(trackheader[i].blocka)):
                trackdata_pos[i] = bigdata_to_word(songfile,startposition+trackdatatable_pos+trackheader[i].blocka[j].index*2)
                trackdata[trackheader[i].blocka[j].index] = songTrackData(songfile,startposition+trackdata_pos[i],trackheader[i],j)
                fileend = max(fileend,trackdata[trackheader[i].blocka[j].index].maxseek)
                

    
    midi = mido.MidiFile()
    midi.type = 1
    midi.ticks_per_beat = 24
    
    for i in range(4):
        if(not(trackheader[i])):
            continue
            
        track = mido.MidiTrack()
        track.sortindex = trackheader[i].blocka[0].index
        channel = trackheader[i].channel
        if(trackheader[i].blocka[0].index == 0 or trackheader[i].blocka[-1].index == 0):
            track.append(mido.MetaMessage('text',time=0,text="Song2Gm v1.01 written by Gau Cho"))
            track.append(mido.MetaMessage('set_tempo',time=0,tempo=60000000//bpm)) #mido.bpm2tempo(bpm))) #mido gives a rounding error because it always rounds up, but it should floor instead
            if(trackheader[i].hasLoop()):
                trackdata[trackheader[i].blocka[-1].index].events.insert(0,trackheader[0].getLoopStart())
                trackdata[trackheader[i].blocka[-1].index].events.append(trackheader[0].getLoopEnd())
                trackdata[trackheader[i].blocka[-1].index].events.sort(key=lambda x:x.absolute_time)
            
        last_absolute_time = 0
        for j in range(len(trackheader[i].blocka)):
            for event in trackdata[trackheader[i].blocka[j].index].events:
                midi_event,last_absolute_time = event.getMessage(channel,last_absolute_time)
                track.append(midi_event)
            
        track.append(mido.MetaMessage('end_of_track'))
        midi.tracks.append(track)
    midi.tracks.sort(key=lambda x:x.sortindex)

    midi.save(targetdir+midifile)
    subprocess.call(["gm2song.exe","-G",targetdir+midifile,targetdir+midifile[:-4]+"_new.dat"], stdout=subprocess.DEVNULL) #Reconvert the generated midi file back into a MusyX datafile to do a checksum comparison
                                                                                              #You should use the gm2song.exe that the original developer used (possible gm2song versions include v1.03 or v1.29g)
    with open(targetdir+midifile[:-4]+"_new.dat","rb") as f:
        temp = f.read()
    hash = hashlib.sha256(temp).digest()

    return fileend - startposition, hash
    
def song2wav(songfile_name,musyx_position,songnames,debug=False):
    #Compatible with MusyX > GM2SONG 1.03 and possibly other versions
    #
    #Arguments:
    #   songfile_name = rom file path
    #   musyx_position = position in the file of MusyX.
    #       MusyX is placed at the beginning of a rom bank chosen by the original developer
    #       e.g. in Magi Nation, MusyX is placed at rom bank 0x30
    #   songnames = An empty array, or an array of song names that will be used as savefiles
    with open(songfile_name,'rb') as f:
        songfile = f.read()
    projectdata_pos = musyx_position+snd_ProjectData-0x4000
    
    songtable_pos = projectdata_pos + littledata_to_word(songfile,projectdata_pos+sdp_SongTableAddress) #data_to_word(songfile[projectdata_pos+sdp_SongTableAddress:projectdata_pos+sdp_SongTableAddress+2])
    
    number_of_songs = songfile[projectdata_pos+sdp_NumberOfSongs]
    print("{} songs".format(number_of_songs))
    last_songhash = None
    songlist_i = -1
    
    os.makedirs(targetdir, exist_ok=True)

    for i in range(number_of_songs-1,-1,-1):
        if(i < len(songnames)):
            name = "song_{:03}_{}".format(transform_id(i,ID_SONG),songnames[i])
        else:
            name = "song_{:03}".format(transform_id(i,ID_SONG))
        print(name)
        
        songlookup_pos = songtable_pos + 3*i
        relativebank = songfile[songlookup_pos]
        address =  + littledata_to_word(songfile,songlookup_pos+1)  #data_to_word(songfile[songlookup_pos+1:songlookup_pos+3])
        song_pos = musyx_position + relativebank*0x4000+address-0x4000 + 0x84
        filesize,sha = song2gm(songfile_name,song_pos,name+".mid")
        
        if(debug):
            with open(targetdir+name+"_ori.dat","wb") as f:
                f.write(songfile[song_pos:song_pos+filesize])
        else:
            os.remove(targetdir+name+"_new.dat")
        if(hashlib.sha256(songfile[song_pos:song_pos+filesize]).digest()!=sha):
            print("NOT IDENTICAL")
                
        song_pos = musyx_position + relativebank*0x4000+address-0x4000 #header info
        defaults = [transform_id(songfile[song_pos+j],ID_MACRO) for j in range(4)]
        song_pos += 4
        soundlist = [transform_id(songfile[song_pos+j],ID_MACRO) for j in range(0x80)]
        soundhash = hashlib.sha256(bytes(soundlist)).hexdigest()
        if soundhash != last_songhash:
            songlist_i += 1
            last_songhash = soundhash
            
            nullprg = transform_id(0,ID_MACRO)
            soundlist_2 = soundlist.copy()
            for j in range(len(soundlist_2)):
                if(soundlist_2[j] == nullprg):
                    soundlist_2[j] = "{:3} or undefined".format(nullprg)
            with open(targetdir_base+"soundlist_{:02}.txt".format(songlist_i),"w") as f:
                for j in range(len(soundlist_2)):
                    f.write("Soundlist {} .mxm id: {:3}\n".format(j+1,soundlist_2[j]))
                f.write("\n\n")
        with open(targetdir_base+"soundlist_{:02}.txt".format(songlist_i),"a") as f:
            f.write(name+"\n")
            for j in range(len(defaults)):
                success = False
                for k in range(len(soundlist)):
                    if(defaults[j] == soundlist[k]):
                        f.write("MidiSetup Channel {} Pgr.: {:3}\n".format(j+1,k+1))
                        success = True
                        break
                if(not(success)):
                        f.write("MidiSetup Channel {} Pgr.: UNKNOWN?\n".format(j+1))
            f.write("\n")
            
                
        
    

