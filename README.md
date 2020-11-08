MusyX is a sound system developed by Factor 5 for the Gameboy and other consoles.

It essentially acts as a sound driver, simplifying the life of the musician and programmer. The musician provides midi files and programmed sound effects which are converted and assigned ids. These ids can be used by the programmer to play music and sound effects.

This document focuses on the structure of the Gameboy Color and Monochrome Gameboy MusyX sound driver. The driver is identical for both gameboy versions.

An archived version of the MusyX musician and programmer manual is available on archive.org: https://archive.org/details/MusyXAudioToolsForNintendo64AndGameBoy/page/n5/mode/2up

Please browse the manual to get a general understanding of how MusyX works. All public functions are documented there. This document assumes you have at least a superficial understanding of the following sections of the manual:
	Introduction (p3)
	Working with MusyX (p11)
	Overview (p33)
	SoundMacros and the SoundMacro Editor (p35)
	The Project Manager (p69)
	Walk Through (p75)
	Additional Tools (p91)
	Data Conversion (p99)
	Appendix 2 - Game Boy Musicians Reference (p177)
	Appendix 4 - Game Boy Programmers Reference (p345)

1.0 MusyX Versions

There are several released versions of MusyX. Examples include:

A) The MusyX installation cd .iso found somewhere on the internet:
	MusyX.exe version 1.01
	gm2song.exe version 1.03
	MUConv.exe version 1.04

B) The MusyX version used by Orkiz in his publically released MusyX examples:
	MusyX.exe version ?
	gm2song.exe version 1.29g
	MUCONV.exe version 1.15

There are definitely other versions of the sound driver as well.

This document is based on research from MusyX.exe 1.01 / gm2song.exe version 1.03 / MUConv.exe version 1.04
The document is also based on research from the version of the MusyX driver used in MagiNation which seems to be a version almost identical to (A)

2.0 Structure Overview

The MusyX driver is composed of several components. First, there is the driver code that varies from MusyX version to MusyX version. Secondly, there is generated sound data which is compiled from the musician's midi files and sound effect code.

Driver code:

1) musyx.o
	This is the main code of the driver. It's about $1A79 bytes long. It must be placed at the beginning of any bank, address $4000. This bank is called bank MUSYX
2) musyxb0.o
	This contains driver code that needs to use the second ROM bank to access data. It is $550 bytes long and needs to be placed anywhere in bank 0. It is often placed at the end of bank 0 (address $3AB0 to $4000)
3) WRAM memory
	MusyX reserves $DF00 - $DFFF of a WRAM bank to store information to operate the driver

Sound data:

1) game.proj (snd_ProjectData)
	Located immediately after musyx.o (i.e. approximately at address $5A79 in bank MUSYX)
	Contains lookup tables to all the sound data, as well as some sound data
2) game.pool (Pool Data)
	Located at address $4000 of bank (MUSYX+1). Is usually multiple banks long. Contains Sample data, and Midi file data.

3.0 How MusyX Handles Sound

The gameboy version of MusyX ignores many parameters from the MusyX.exe program. If a parameter is not mentioned in this guide, it is very likely completely ignored.

There are two types of sound:
	1) Raw samples played using the Wave Pattern RAM of Channel 3
	2) SoundMacros

3.1 Raw sample

These are played using the functions snd_StartSample or snd_PlaySample
These functions are seldom used. The format of the samples are identical to that of SoundMacros (see below).

3.2 SoundMacros

SoundMacros are very short snippets of code that send commands the the gameboy's sound registers, generating a specific sound effect. The sound can range from a simple frequency to simulate a note to an elaborate explosion-type sound effect, or anything in-between. The sound can also be a sample. 99% of sounds via MusyX are likely generated via SoundMacros as opposed to raw samples.

Note that Keymaps and Layers are converted to SoundMacros in an as-yet undetermined fashion.

There are two ways of triggering a SoundMacro:
	1) snd_StartSFX will trigger 1 target SoundMacro
	2) snd_StartSong will play a midi song. Every single "note" played in the original midi file triggers one SoundMacro that handles what each note should sound like

The SoundMacro itself can choose which channel 1-4 that it will use. For example, a SoundMacro playing a sample must use channel 3, and a SoundMacro generating noise must use channel 4, but a SoundMacro just playing a simple frequency to simulate a note could choose any channel.

Since multiple SoundMacros can be triggered and all want to use the same channel, each SoundMacro has a priority from 0-255. Only the SoundMacro with the highest priority will play, even potentially interrupting a currently-playing sound. Lower-priority SoundMacros will simply not play.

This means that although a Song + SFX in total can trigger as many SoundMacros as you want, there will always only be a maximum of 4 SoundMacros playing at any given point in time, with the rest ignored.

4.0 Setting Up Output Files and the Format of Output Files

This section covers the format of game.proj and game.pool, and how to generate them

4.1 Generating .pool and .proj files from musician's project

These two files are created by MUConv.exe.

An easy way to setup MUConv.exe is to create a .bat file with the following code:
muconv -t GB -v Script.txt GAME.des >out.txt

out.txt will contain debugging data for the output file generation.

Before running the .bat file, make sure to setup your GAME.des file with the following contents:

[Pool]
GROUPS
[Samples]
GROUPS
[Project]
GROUPS
[OutputDirectory]
Output
[Name]
GAME
[Include]
SoundIDs.i

SoundIDs.i contains the id of all the SFX and Songs
GAME is the name of your output files
GROUPS is a list of all the subfolders under "Groups" in the MusyX project.
	In many cases, GROUPS will equate to:
		Songgroup\nSFXgroup

5.0 .proj file format

MUConv.exe is responsible for generating the .proj file.

The .proj file is appended to the same bank as musyx.o (bank MUSYX), right after musyx.o. Both musyx.o and the .proj file must fit within one $4000 bank.

All addresses are relative to the start of the .proj file. In other words, the .proj file is usually placed at address $5A79. An address of $0010 for example would point to $5A89 in the bank.

Banks are relative to bank MUSYX. For example, if musyx.o and the .proj files are placed in bank $20, then a bank of 3 would refer to the actual bank $23.

Offset
	Name
		Info
$0000
	ADSR_TableAddress
			Address of the ADSR lookup table
$0002
	SFX_TableAddress
			Address of the SFX lookup table
$0004
	SFX_n
			Total number of SFX
$0005
	Sample_TableAddress
			Address of the Sample lookup table
$0007
	Sample_n
			Total number of samples
$0008
	db $00
			This value is only non-zero when making a test GB slave ROM, in which case the value is equal to Number_of_Samples
$0009
	SampleMap_SoundMacro_Address
			Address of the start of the SoundMacro that contains SampleMap commands. dw $0000 if there is no such SoundMacro
$000B
	SampleMap_n
			Number of entries in the SampleMap, excluding the END command
$000C
	Song_TableAddress
			Address of the Song lookup table
$000E
	Song_n
			Total number of songs
$000F
	SoundMacro_Table
			See filestructure below
ADSR_TableAddress
	ADSR_Table
			See filestructure below
SFX_TableAddress
	SFX_Table
			See filestructure below
Sample_TableAddress
	Sample_Table
			See filestructure below (section 6.1)
Song_TableAddress
	Song_Table
			See filestructure below
EOF

5.1 SoundMacro_Table

All addresses are relative to the start of SoundMacro_Table. In other words, SoundMacro_Table is usually placed at address $5A79+$000F. An address of $0010 for example would point to $5A98 in the bank.

SoundMacro_Table has two sections:

1) A lookup table
	A list of n addresses in sequential order pointing to the opcodes of each SoundMacro
	e.g. dw $0006, $0015, $002A
2) Raw opcode data in sequential order of each SoundMacro
	All n SoundMacros are written here sequentially. Each SoundMacro is converted from .mxm to a more compact data form. See below for more info. Each SoundMacro can be a maximum of 0x200 bytes after being converted to data

You can tell how long the lookup table is by taking address at index 0 and dividing it by 2, because this address points to the first SoundMacro which is placed immediately at the end of the lookup table.

5.2 ADSR_Table

This section is formatted in a similar manner to SoundMacro_Table

All addresses are relative to the start of ADSR_Table.

ADSR_Table has two sections:

1) A lookup table
	A list of n addresses in sequential order pointing to raw ADSR data.
	e.g. dw $0004, $000B
2) Raw ADSR data in sequential order of each ADSR
	See below for the data format. Each ADSR data entry is always exactly 7 bytes long.

You can tell how long the lookup table is by taking address at index 0 and dividing it by 2.

5.3 SFX_Table

As a reminder, each SFX triggers exactly 1 SoundMacro

Each entry in this table is 4 bytes long (total length SFX_n*4)

	db SoundMacro_Table_index - points to the linked SoundMacro
	db Priority - Default priority of the SFX (0-255) to pass to the SoundMacro
	db DefKey - Default MIDI key (0-127) to pass to the SoundMacro
	db DefVel - Default velocity (i.e. volume)*

*The default velocity is converted from a number 0-127 using the following code:
	IF Volume == 0
		0
	ELSE
		max(1,Volume//8)

5.4 Sample_Table

Each entry in this table is 6 bytes long (total length Sample_n*6)

See the .pool data structure for more info on samples

	dw AbsoluteAddress - points to the absolute address of the sample.
	dw Size/$10 - Size of the sample data divided by $10
	db HighQuality - 1 if high-quality sample, or else 0 if low-quality sample
	db RelativeBank - bank of the sample minus bank MUSYX

5.5 Song_Table

Each entry in this table is 3 bytes long (total length Song_n*3)

	db RelativeBank
	dw AbsoluteAddress

6.0 .pool file format

MUConv.exe is responsible for generating the .pool file. gm2song.exe is executed by MUConv.exe to convert .mid files to .song files, which are inserted into the .pool file.

The .pool file starts in bank (MUSYX + 1) and can be multiple banks long.

The .pool file has two sections:

1) Sample data section
2) Song data section

6.1 Sample data

Sample data starts at bank (MUSYX+1). Even if there are no samples and there is therefore no sample data, this section takes up at least 1 bank. Empty space at the end of the last bank is uninitialized, meaning that other file fragments from the developer's computer might show up in the uninitialized space.

You can convert .aiff, .mort or .wav into samples.

Here's how you convert a .wav into a sample:

First, the .wav must be in 16-bit, mono format

If the .wav is 4000Hz+, MUConv.exe will assume that the .wav had a frequency of 8192 Hz. If the quality is 3999Hz-, it will assume the .wav had a frequency of 1920 Hz. Therefore, any value other than 1920Hz or 8192Hz will cause the sample to be played back at the wrong speed.

Note that even though the MusyX manual mentions low-quality, medium-quality and high-quality samples, there are actually only 2 speeds: low (1920 Hz) and medium/high (8192 Hz). A high-quality sample is considered medium-quality if it is played via a SoundMacro or snd_StartSample, and is considered high-quality if it is played via snd_PlaySample.

Since the .wav is in a 16-bit mono format, each input datum is a 16-bit word, where $0000 is neutral, $7FFF is the highest positive value, and $8000 is the lowest negative value.

The output datum that is stored in .pool is a 4-bit (one nibble) datum:
	datum = datum >> 12 ;take the 4 most significant bits
	datum = (datum + 8) % 16

The output datum are sequentially ordered. If the total number of data are not evenly divisable by 32, The data is padded with nibbles of $7 until it is evenly divisable. (i.e. each block of $10 bytes contains 32 data points) (See Pan Docs info on the registers FF30-FF3F Wave Pattern RAM). The length of a sample is always a multiple of $10.

There is one special exception the above rules. The very first datum of a 32-data block contains special info: if the first datum is $7, it indicates compressed silent data.

Therefore, if the very first datum is supposed to evaluate to $7, it is changed to $8 to avoid indicating compressed data.

If MUConv.exe comes across two or more sequential 32-data blocks in a row that evaluate to [$88]*16 (i.e. multiple blocks of 32*$0000 in the original .wav file), then the special compressed silent data format will be set.

The special compression format is:
	[$7?] + [$NN] + [$??]*14,
		where the data in ? is ignored
		and where ($NN + 1) indicates the number of times to write the silent sample. $NN's maximum value is $FE.

Practically speaking, MUConv.exe encodes the data as:
	[$78] + [$NN] + [$88]*14

The MusyX driver will copy the raw contents of each sample block directly into the Wave Pattern RAM. However, if it comes across compressed silent data, it will fill the Wave RAM with [$88]*16 for ($NN + 1) times. The only exception is the function snd_PlaySample, where the Wave Pattern RAM is filled with [$77]*16 instead.

Note that samples can cross over from the end of one bank into the next bank.

6.2 Song data

The song data section is placed at the bank immediately after the sample data. This means that if the sample data takes up 1 bank or less, the song data will start at bank (MUSYX+2).

A song must fit within a single bank, and cannot cross into another bank. However, MUConv.exe will fit multiple songs into the same bank if there is space.

Each song comes with a header and raw song data.

The header is created by MUConv.exe. The header is always $84 bytes long

This section assumes you have a superficial understanding of the .midi file format (see http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html or https://jimmenard.com/midi_ref.html)

The first 4 bytes indicate the default Program of channels 1-4. The default Program for the first 4 channels are defined in The MIDI Setup Window (page 93 of the MusyX manual) as "Prg.". The first 4 bytes have a value of (0-127), corresponding to the 128 possible Program choices.

The next $80 bytes map all 128 Programs to SoundMacros (Soundlist in the midi setup of MusyX.exe). Essentially, each byte is a pointer to the id of the target SoundMacro.

Following the header immediately comes the raw song data.

The raw song data (.song) is generated by gm2song.exe

6.2.0 .song info

This is how gm2song.exe interprets a midi file and generates a .song file. Note that all addresses are relative to the start of the .song file

All meta events ($FF) except End of Track and Set Tempo are ignored
System messages ($F0/F2/F3) are ignored. Other System messages > $F0 are unsupported and give warnings
If a note starts and stops on the same tick (same pitch and channel), the program will assume they are inverted to fix the presumed error
	e.g.	Time 0 Start Note
			Time 10 Start Note  --\
			Time 10 End Note    --/  These two notes will be swapped
			Time 20 End Note

For GBC, the 4 tracks are defined by Channels 0-3. Other channels are discarded...? 

BPM defaults to 120 if undefined

The earliest defined BPM is used. Tempo changes are not supported

The division of the midi file is always converted to 24 ticks.  All references to "time" below refer to number of ticks, where 24 ticks = 1 quarter note.
The maximum distance between notes is 4096 ticks, or 170 quarter notes.

When adding a program change event, it will only be executed at the time of the subsequent note, so it always assumes a program change event will occur simultaneously with a note.

NOTE THAT ALL 2-BYTE VALUES ARE BIG-ENDIAN

File header
	ds 2 - pointer to Track header table (always $0006)
	ds 2 - pointer to Track table
	ds 2 - BPM

Track header table
	ds 2 - pointer to Track 0 header, or 0 if undefined
	ds 2 - pointer to Track 1 header, or 0 if undefined
	ds 2 - pointer to Track 2 header, or 0 if undefined
	ds 2 - pointer to Track 3 header, or 0 if undefined

Track header
	null if track is unused
	Each track header defines 1 or 2 tracks (BLOCKA)
	Each track header ends with loop information (BLOCKB)

	BLOCKA
		First BLOCKA defines a track from start of the song until the beginning of the song loop
		ds 2 - time to first event
		ds 1 - track index number

	BLOCKA
		Second BLOCKA defines a track from when the loop starts
		This block only exists if (1) there is a song loop and (2) the song doesn't loop back to the very beginning, but somewhere in the middle
		ds 2 - time to first event after song loop, minus the time of the first BLOCKA
		ds 1 - track index number

	BLOCKB
		If there is a loop:
			ds 2 - time between the first event until LoopEndRepeat
			db $FE
			ds 2 - The offset from the start of the header pointing to the beginning of the last BLOCKA (i.e. a value of 3*(count(BLOCKA)-1))
			ds 2 - If the Loop Start is in front or simultaneous with the first event, return the delta time between these two (0 if simultaneous)
		If there is no loop:
			dw $0000
			db $FF
			dw $0000
			dw $0000

Track table
	ds 2*number_of_tracks - pointers to Track x

	There are up to 8 tracks. Each BLOCKA references a new track
	In other words, a song that loops back to the middle of the song is split
		There are up to 4 tracks to represent the music before the loop, and up to 4 tracks defining the music during the loop
	If there are 4 defined track headers each with a single BLOCKA,
		then there are 4 tracks
	If there are only 3 defined track headers each with a single BLOCKA,
		some versions of gm2song.exe will still define 4 tracks (an empty track will be inserted)
	If there are 4 defined track headers with two BLOCKAs,
		then there are 8 tracks

Track data
	Each entry in the track table has a track.
	Each track will have X events (notes to play), a possible ending ProgramChangeEvent, and an end of track
	At the very minimum, a null track has only the end of track bytes (3 bytes)

	Track schema:
		(Event)*X
		(Possible ProgramChangeEvent)
		$F0 $00 $FF = end of track


Event (4-6 bytes long):
	1) ds 2
		Bit 15-12 = Velocity (always 1-F)
			Velocity is calculated as (Velocity + 4)/8, capped for a range 1-F
		Bit 11-0 = DeltaTime from last event's NoteOn (Max delta $FFF ticks)
	2) ds 1
		Bit 7 = Flag saying that (3) is defined (extra program command)
		Bit 6-0 = NoteOn key
	3) ds 1 - only defined if Bit 7 from (2) was set to 1, or else this byte is skipped
	4) Duration of note (time until NoteOff)
		If bit 7 of the first byte is 1, then
			ds 2: A length defined by bits 14-0 (15 bits long)
		Else
			ds 1: A length defined by bits 6-0 (7 bits long)

ProgramChangeEvent:
	Usually program changes are merged with the next note. However, if the program change event occurs after the very last note, it shows up by itself as follows:
	1) ds 2
		Bit 15-12 = Velocity 0 (only time the velocity can be 0)
		Bit 11-0 = DeltaTime from the last NoteOn
	2) ds 1
		Bit 7: 1
		Bit 6-0: Program data

7.0 ADSR format (.mxt files)

The source .mxt file is little-endian and contains the following 8 bytes of data:
	0:2 Attack time (milliseconds)
	2:4 Decay time (milliseconds)
	4:6 Sustain level ($1600 = 100.00%)
	6:8 Release time (milliseconds)

The output data is little-endian and is 7 bytes long:
	v = min($0F,math.floor(Sustain*0.003662109375+0.5)) ;about 1/273
	dw math.floor(3840/cycles(max(1,Attack))+0.5)
	dw -(($0F-v)*$100//cycles(max(1,Decay)))
	db v
	dw -(v*$100//cycles(max(1,Release)))

8.0 SoundMacro format (.mxm files)

Each SoundMacro can be a maximum of $200 bytes after being converted to data

Note that the endianness switches
.mxm files are little-endian
The lookup table in the .proj file is is little-endian
The SoundMacro opcode/raw data is BIG-ENDIAN

.mxm files conversion instructions

The .mxm format is a set of n instructions, where each instruction is 8 bytes long
In MusyX.exe, the values of all the bytes are labelled so it's easy to see how the data is represented
Alternatively, gameboy_macrodef.mxd included in MusyX indicates all the opcodes and the definition of each byte for each instruction
Finally, the MusyX.pdf guide details each instruction's parameters in detail

Before explaining every opcode one-by-one, here are some functions used in the conversion program that I will define

	id(SoundMacroID or ADSRTableID or SampleID)
		Given the development files' ID, return the index id of the Macro/ADSR/Sample in the output file (for samples, see the documentation on SAMPLEMAP)

	address(SoundMacroID,SoundMacroStep)
		Given the developer's MacroID + SoundMacroStep, return the address to jump to to continue reading (address $0000 points to SoundMacro_Table)

	relative(SoundMacroLoopStep)
		Returns the number of bytes from the END of the command to the start of the target command. Negative numbers and positive numbers are both supported

	cycles(Milliseconds)
		Converts Milliseconds to GB cycles (approx 60 per second)
		If Milliseconds == 0, return 0
		Else
			x = double(Milliseconds)
			return floor((x+16.6666660308837890625)*0.060000002384185791015625)

	keycents(Key,Cents)
		Converts Key and Cents into a word, where the upper is the Key and the lower is the Cents normalized from 0-99 to 0-255
		First, normalize Key and Cents to be the same sign:
		x = Key*100+Cents
		Key2,Cents2 are extracted to have the same sign
			e.g. if x = -356, then Key = -3 and Cents = -56
		Cents2 *= 2.575757503509521484375 ; i.e. 255/99
		return signedbyte(Key2)<<8 + signedbyte(Cents2)

OPCODE(Byte0) NAME(gameboy_macrodef.mxd)
	StartByte:EndByte ValueName
	db OutputByte
	dw OutputWord

00 END
	db $00

01 STOP
	db $23

02 SPLITKEY
	1:2 KeyNumber
	2:4 SoundMacroID
	4:6 SoundMacroStep
	db $15
	db KeyNumber
	dw address(SoundMacroID,SoundMacroStep)

03 SPLITVEL
	1:2 Velocity
	2:4 SoundMacroID
	4:6 SoundMacroStep
	db $17
	db Velocity
	dw address(SoundMacroID,SoundMacroStep)

04 RESET_MOD
	db $20

05 LOOP
	(Can only loop backwards)
	4:6 SoundMacroLoopStep
	6:7 Times
	db $05
	db Times
	dw relative(SoundMacroLoopStep)

06 GOTO
	2:4 SoundMacroID
	4:6 SoundMacroStep
	db $06
	dw address(SoundMacroID,SoundMacroStep)

07 WAIT
	1:2 KeyOff
	2:3 RandomTimeBool
	6:8 Milliseconds ($FFFF = infinite wait)
	db $04
	db KeyOff
	db RandomTimeBool
	dw IF Milliseconds != $FFFF
	        cycles(Milliseconds)+1
	   IF RandomTimeBool and Milliseconds == $FFFF
	        $0001 ;Looks like infinite wait is not supported if RandomTimeBool is on?
	   IF not(RandomTimeBool) and Milliseconds == $FFFF
	        $0000

08 PLAYMACRO
	1:2 Voice ($FF "keeps the voice chosen by the midi sequence")
	2:4 SoundMacroID
	4:5 DontResetBool
	db $26
	db IF Voice <= 3
	        x = Voice
	   ELSE
	        x = $FF
	   x | (DontResetBool*$80)
	db id(SoundMacroID)

09 PLAYKEYSAMPLE
	db $22

0A STOP_MOD
	db $1F

0B SETVOICE
	(Error if this is not the first command of a macro)
	1:2 Voice
	4:5 DontResetBool
	5:6 ToggleBool
	db $0E
	db SWITCH Voice,DontResetBool,ToggleBool:
	    case 0-3,_,_    Voice | (DontResetBool*$80) | (ToggleBool*$10)
	    ?case $FF,_,_    $FF (not sure, this might not happen)
	    case _,_,0      $FF
	    case _,_,1      (Voice & 3)| (DontResetBool*$80) | (ToggleBool*$10)

0C SETADSR
	1:3 ADSRTableID
	db $0F
	db id(ADSRTableID)

0D SETVOLUME
	1:2 Volume
	db $09
	db IF Volume == 0
	        0
	   ELSE
	        max(1,Volume//8)

0E PANNING
	1:2 Panning
	db $0A
	db switch Panning:
	    case 0-$29      0      (represents 0)
	    case $2A-$54    1      (represents 64)
	    case $54-$FF    2      (represents 127)

0F ENVELOPE
	1:2 AscendingBool
	6:8 Milliseconds
	IF AscendingBool
	    db $27
	    dw x = cycles(max(Milliseconds,1))
	       max(1,$0F00//x)
	IF !AscendingBool
	    db $11
	    dw x = cycles(max(Milliseconds,1))
	       max(1,$0F00//x)*(-1)

10 STARTSAMPLE
	1:3 SampleID
	db $21
	db id(SampleID)

11 VOICE_OFF
	db $0D

12 KEYOFF
	1:2 Voice
	db $12
	db Voice if Voice <= 3 else $FF

13 SPLITRND
	1:2 Rand
	2:4 SoundMacroID
	4:6 SoundMacroStep
	db $16
	db Rand
	dw address(SoundMacroID,SoundMacroStep)

14 VOICE_ON
	(Same OpCode as WAVE_ON - VOICE_ON is used for Voice 1,2,4)
	1:2 DutyCycle
	db $0C
	db DutyCycle if (DutyCycle <= 3 or DutyCycle == $FF) else $00

15 SETNOISE
	1:2 PolyClock 0-13
	2:3 PolyStep 0-1
	3:4 FreqRatio 0-7
	db $10
	db PolyClock*%10000 + PolyStep*%1000 + FreqRatio

16 PORTLAST
	1:2 Keys (signed)
	2:3 Cents (signed)
	6:8 Milliseconds
	db $1B
	dw keycents(Key,Cents)
	dw cycles(max(Milliseconds,1))

17 RNDNOTE
	1:2 NoteLow
	2:3 Detune
	3:4 NoteHigh
	4:5 FreeBool
	5:6 AbsBool
	IF NoteLow == NoteHigh and NoteHigh == 127
	    NoteLow -= 1
	ELSEIF NoteLow == NoteHigh and NoteHigh != 127
	    NoteHigh += 1
	db $0B
	db (FreeBool << 7) + AbsBool
	db NoteLow
	db NoteHigh
	db keycents(0,Detune) ;Lower 8 bits

18 ADDNOTE
	1:2 Add (signed)
	2:3 Detune (signed)
	3:4 TempKeyBool
	db $08
	db TempKeyBool
	db Add
	db keycents(0,Detune) ;Lower 8 bits

19 SETNOTE
	1:2 Key
	2:3 Detune (signed)
	db $07
	db Key
	db keycents(0,Detune) ;Lower 8 bits

1A LASTNOTE
	1:2 Add (signed)
	2:3 Detune (signed)
	db $14
	db Add
	db keycents(0,Detune) ;Lower 8 bits

1B PORTAMENTO
	1:2 Key (signed)
	2:3 Cents (signed)
	3:4 RelBool
	6:8 Milliseconds
	db $02
	dw keycents(Key,Cents)
	dw cycles(max(Milliseconds,1))
	db RelBool

1C VIBRATO
	1:2 Keys (signed)
	2:3 Cents (signed)
	6:8 Milliseconds
	x = min(cycles(max(Milliseconds//2,1)),$FF)
	y = keycents(Key,Cents)
	db $01
	dw y//x
	db x

1D PITCHSWEEP
	1:2 NoteLimit (signed)
	2:3 CentLimit (signed)
	5:6 SweepBool
	6:8 Milliseconds
	x = min(cycles(max(Milliseconds,1)),$FF)
	y = keycents(NoteLimit,CentLimit)
	db $03
	dw y//x
	dw y
	db SweepBool*$80

1E HARDENVELOPE
	1:2 FadeInBool
	6:8 Milliseconds
	db $13
	db FadeInBool*0b1000 + max(1,min(7,floor(Milliseconds*0.0042666667141020298004150390625 + 0.5)))

1F PWN_START
	1:2 LowLimit
	2:3 HighLimit
	3:5 Speed
	If LowLimit > HighLimit the values are swapped to fix the error
	db $1D
	db LowLimit
	db HighLimit
	dw 0 if cycles(Speed) == 0, else ((HighLimit - LowLimit) << 8)//cycles(Speed)

20 PWN_UPDATE
	1:2 LowLimit
	2:3 HighLimit
	3:5 Speed
	If LowLimit > HighLimit the values are swapped to fix the error
	db $1E
	db LowLimit
	db HighLimit
	dw 0 if cycles(Speed) == 0, else ((HighLimit - LowLimit) << 8)//cycles(Speed)

21 PWN_FIXED
	1:2 Duty
	db $24
	db Duty

22 PWN_VELOCITY
	db $25

23 SENDFLAG
	1:2 FlagBit
	db $1A
	db FlagBit & 7

24 SAMPLEMAP
	(max 127 entries)
	(error if a non-SAMPLEMAP command precedes a SAMPLEMAP command)
	1:3 SampleId
	;There's no opcode header
	db id(SampleID)

25 CURRENTVOL
	1:2 Volume
	db $28
	db IF Volume == 0
	        0
	   ELSE
	        max(1,Volume//8)

26 WAVE_ON
	(Same OpCode as VOICE_ON - WAVE_ON is used for Voice 3)
	1:3 SampleID
	db $0C
	db id(SampleID) if SampleID != 0 else $FF

27 ADD_SET_PRIO
	1:2 Priority (unsigned if SetBool else signed)
	2:3 SetBool
	db $29
	db SetBool
	db Priority

28 TRAP_KEYOFF
	2:4 SoundMacroID
	4:6 SoundMacroStep
	db $18
	dw address(SoundMacroID,SoundMacroStep)
	
29 UNTRAP_KEYOFF
	db $19

9.0 Python Package

The python package is able to convert data from a rom file into the original contents of a MusyX.exe project. You can then convert the MusyX.exe project back into the original rom file data with a little bit of work.

The python package extracts the following:

1) SoundMacros
2) Midi files and parameters
3) ADSR
4) Samples
5) SFX parameters

This information is extracted in python/out/

The package was designed with MUConv.exe version 1.04 in mind, but it might be useful or compatible with other MUConv.exe versions.

9.0.1 Finding the MusyX data

First, you need to find your MusyX bank. Here are some somewhat unique byte strings that probably exist in all versions of MusyX:

1) $10 $11 $01 $20 $22 $02 $40 $44 $04 $80 $88 $08
2) $01 $23 $45 $67 $78 $9A $BC $DE

Search for one of these byte strings and use that bank as your MusyX bank

Alternatively, find the bank that makes multiple read/writes to WRAM in the $DF00-DFFE memory range, or the bank that makes multiple read/writes to the sound registers.

9.0.2 Determining Song and SoundMacro count

To properly label the songs and soundmacros, the total number of songs and soundmacros must be known.

To do this, use example_setup.py

Set your MusyX bank and rom filepath.

Run the program and it should give you a number for the number of songs and soundmacros.

Edit musyx/utils.py and input these numbers.

9.0.3 Extracting the Data

After editing musyx.utils.py as described in 9.0.2, use example_parse.py to extract all the data from the rom.

By default, extract files will be labelled filetype_number.ext. However, if you provide a custom name, the files will be labelled filetype_number_label.ext

The custom names are defined in the arrays, where the index of the array corresponds to the index used in the rom file.

SOUNDMACROS

.mxm files will automatically be generated. Simply import these files in the indicated order.

For debug purposes, a file called macrodebug.txt is generated that labels the original data contents of the rom.

MIDI

Files called soundlist_XX.txt will be created. Each soundlist.txt represents a unique Songgroup. In MusyX.exe, you need to create a Songgroup folder in the correct order for each soundlist.txt. You must then open each Songgroup set all 128 of the Soundlist > Object IDs to match the information provided in the .txt file. Then, for each midi file, you must double-click on the Midisetup and change the Prg. of channels 1-4 to match the information provided in the .txt file.

The program will convert the .mid file back into data and compare to see if the conversion was perfect. It therefore needs gm2song.exe to be in the directory of the python project to run the conversion of the .mid file into data.

ADSR

.mxt files will automatically be generated. Simply import these files in the indicated order.

SAMPLES

.wav files will automatically be generated. Import these files in the indicated order.

I was not able to figure out the exact algorithm for determining the right order of the samples. Therefore, you might have to experimentally change the sample ids in order to get the sample order as in the original rom file. To do this, right-click a sample and select Properties, and then change the ObjectID (select Yes to updating the references to keep the SoundMacros in sync with the samples). Keep changing the ObjectIDs until you get the desired output order.

SFX parameters

This will generate a file called sfxinfo.txt that will tell you what parameters to put in each for SFX. Simply create an SFXgroup and then transcribe the info from the .txt file into the SFXgroup
