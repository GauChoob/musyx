SECTION "WRAM_MUSYX", WRAMX[$DF00]

; The $100 bytes of RAM used by MusyX
; See the MusyX pdf manual for some details
; The bank can be changed, as long as you make sure to switch to the correct bank
;   before calling any MusyX function

    ;ds $DF00 - @
wSnd_WRAM:: ;size $FF (last byte is unused)
wSnd_BaseBank::
    ; The bank of musyx.asm, used for relative offsets to access sample and song data
    ds 1
    ; ds $DF01 - @
wSnd_CurrentROMBank_HRAMAddress::
    ; Points to where the currently selected ROM bank should be stored
    ; Upper byte is $FF, lower byte is this
    ds 1
    ;ds $DF02 - @
wSnd_Initialized::
    ; $F5 after snd_Init and $00 after snd_Exit
    ds 1
    ;ds $DF03 - @
wSnd_RandomSeed::
    ; Initializes to $DE
    ds 1
    ;ds $DF04 - @
wSnd_GameboyModel::
    ; See snd_Init
    ; bit 0 = 1 if CGM rom, else 0 if DMG rom
    ; bit 1-6: Unused
    ; bit 7 = 1 if CGM hardware, else 0 if DMG hardware
    ; i.e.
    ;   $81 Running a Game Boy Color game on Game Boy Color
    ;   $01 Running a conventional game on Game Boy Color
    ;   $00 Running a conventional game on Game Boy
    ds 1
    ;ds $DF05 - @
wSnd_Voice_SoundLengthIndex::
    ; When this value hits zero, 0 is sent to the sound length register NR01
    ds 1*3
    ;ds $DF08 - @
wSnd_TMA_Mirror::
    ; Contains the value of TMA. $00 in normal-speed, $80 in double-speed
    ds 1
    ;ds $DF09 - @
wSnd_Voice_FrequencyStatus::
    ; Most but not all bytes relate to a voice's frequency
    ; bit 0: 1 if Vibrato effect
    ; bit 1: 1 if SoundMacro comes from SFX, 0 if comes from Song
    ; bit 2: 1 if Portamento effect
    ; bit 3: 1 if Pitchsweep effect #0
    ; bit 4: 1 if Pitchsweep effect #1
    ; bit 5: 1 if register's NR03-04 frequency should be updated during snd_Handle
    ; bit 6: 1 if register's NR01 soundlength should be set to 0 when wSnd_Voice_SoundLengthIndex hits 0 (Call_030_4524)
    ;   enabled after frequency is updated (bit 5)
    ; bit 7: 1 if Voice is On, 0 if voice is Off
    ds 1*4
    ;ds $DF0D - @
wSnd_Voice_VolumeStatus::
    ; Most but not all bytes relate to a voice's volume
    ; bit 0: 1 if Envelope_Ascending OR if ADSR_Attack
    ; bit 1: 1 if ADSR_Decay
    ; bit 2: 1 if ADSR_Sustain (checked by KEYOFF to setup ADSR_Release)
    ; bit 3: 1 if Envelope_Descending OR if ADSR_Release
    ; bit 4: 1 if Envelope_Ascending, 0 if ADSR. (To decide whether to terminate the ascending envelope or move on to ADSR_Decay)
    ; bit 5: 1 if register's volume should be updated during snd_Handle
    ; bit 6: Unused
    ; bit 7: 1 if KeyOff was sent to this Voice (used to terminate WAIT)
    ds 1*4
    ;ds $DF11 - @
wSnd_NR51Panning_Mirror::
    ; The status of NR51, if all 4 voices were turned on
    ; This way, you can change the panning of a voice that is off, and then
    ; turn the voice on later and preserve the panning information
    ds 1
    ;ds $DF12 - @
wSnd_NR04FreqUpper_Mirror::
    ; A copy of bits 2-0 of NR14/NR24/NR34
    ; This is used as an OR mask to set bit 7 or 6 without accidentally
    ;   changing the values of bits 2-0
    ds 1*3
    ;ds $DF15 - @
wSnd_Voice_KeyCents::
    ; Byte low: Cents
    ; Byte high: Key
    ds 2*4
    ;ds $DF1D - @
wSnd_Voice_Modulation_DeltaKeyCents::
    ; Delta set by Vibrato and Pitchsweep effects. Can be reset to 0 by RESET_MOD
    ds 2*3 ;No voice 3
    ;ds $DF23 - @
wSnd_Voice_AddNote_DeltaKeyCents::
    ; Temporary KeyCent offset used by RNDNOTE and ADDNOTE
    ;   This allows you to define a new absolute or relative KeyCent
    ;   without losing the original KeyCent value
    ds 2*3 ;No voice 3
    ;ds $DF29
wSnd_Voice_LastKey::
    ; The key of the previously played note (KeyCents + AddNote_DeltaKeyCents)
    ; (Modulation DeltaKeyCents is not considered)
    ; Used by PORTLAST to slide from a previous note to the current note
    ds 2*3
    ; ds $DF2F - @
wSnd_Voice_InternalVolume::
    ; Mirror of volume sent by _snd_SendVolumeToRegister directly to register
    ; Set whenever volume is changed or CURRENTVOL is called
    ; Used by HARDENVELOPE
    ; For Voice 2, even though only the upper 2 bits are used, all 4 bits are stored
    ds 1*4
    ;ds $DF33 - @
wSnd_Flags::
    ; See snd_CheckFlag
    ds 1
UNION ; PORTamento, VIBRATO and PITCHSWEEP share the memory since only 1 can be active at a time
    ;ds $DF34 - @
wSnd_Voice_Port_DeltaKeyCentsPerCycle::
    ds 2*3
    ;ds $DF3A - @
wSnd_Voice_Port_Cycles::
    ; Number of cycles to finish the Portamento
    ds 2*3
    ;ds $DF40 - @
wSnd_Voice_Port_Undefined::
    ; Used by Vibrato and Pitchsweep
    ds 2*3
    ;ds $DF46 - @
wSnd_Voice_Port_TargetKeyCents::
    ds 2*3
    ;ds $DF4C - @
wSnd_Voice_Port_Undefined2::
    ; Used by Pitchsweep
    ds 2*3
    ds 2*3
NEXTU
    ;ds $DF34 - @
wSnd_Voice_Vibrato_DeltaKeyCentsPerCycle::
    ds 2*3
    ;ds $DF3A - @
wSnd_Voice_Vibrato_CycleCounter::
    ; 2x a full frequency cycle
    ds 2*3
    ;ds $DF40 - @
wSnd_Voice_Vibrato_CycleLength::
    ; Time of a full frequency cycle (half of the full period)
    ds 2*3
    ;ds $DF46 - @
wSnd_Voice_Vibrato_CurrentKeyCentOffset::
    ; Initially 0
    ds 2*3
    ;ds $DF4C - @
wSnd_Voice_Vibrato_Undefined::
    ; Used by Pitchsweep
    ds 2*3
    ds 2*3
NEXTU
    ;ds $DF34 - @
wSnd_Voice_Sweep_DeltaKeyCentsPerCycle::
    ds 2*3 ; Pitchsweep 0
    ds 2*3 ; Pitchsweep 1
    ;ds $DF40 - @
wSnd_Voice_Sweep_TargetKeyCents::
    ds 2*3 ; Pitchsweep 0
    ds 2*3 ; Pitchsweep 1
    ;ds $DF4C - @
wSnd_Voice_Sweep_CurrentKeyCentOffset::
    ; Initially 0
    ds 2*3 ; Pitchsweep 0
    ds 2*3 ; Pitchsweep 1
ENDU
    ;ds $DF58 - @
wSnd_Voice_Envelope_ADSRPointer::
    ; Points to Decay, Sustain or Release in the ADSR table, depending on time
    ds 2*4
    ;ds $DF60 - @
wSnd_Voice_Envelope_VolumeCents::
    ; The upper byte is copied to wSnd_Voice_EnvelopeVolume
    ds 2*4
    ;ds $DF68 - @
wSnd_Voice_Envelope_DeltaVolumeCents::
    ds 2*4
    ;ds $DF70 - @
wSnd_Voice_EnvelopeVolume::
    ; Used in software envelopes and ADSR
    ds 1*4
    ;ds $DF74 - @
wSnd_Voice_SampleStatus::
    ; bit 0: PWN (Pulse wave mode) running from a SoundMacro using snd_Handle
    ; bit 1: Unused
    ; bit 2: Unused
    ; bit 3: Unused
    ; bit 4: 1 if sample started via snd_StartSample. 0 if started by a SoundMacro
    ; bit 5: Unused
    ; bit 6: 1 if normal quality sample running using snd_DoSample
    ; bit 7: 1 if low quality sample running using snd_Handle
    ds 1
UNION ; Since PWN and samples share the same voice, the storage memory addresses overlap below
    ;ds $DF75 - @
wSnd_Sample_Bank::
    ds 1
    ;ds $DF76 - @
wSnd_Sample_Address::
    ds 2
    ;ds $DF78 - @
wSnd_Sample_Length::
    ; Length/$10 (number of $10 byte sections)
    ds 2
    ;ds $DF7A - @
wSnd_Sample_Undefined::
    ; Reset to 0 in snd_StartSample and STARTSAMPLE
    ; Seems to theoretically extend wSnd_Sample_Length to 24 bits but
    ; the extended 8 bits are never ever used.
    ; This byte is used by PWN however.
    ds 1
    ;ds $DF7B - @
wSnd_Sample_CallbackAddress::
    ; de in snd_StartSample
    ds 2
    ;ds $DF7D - @
wSnd_Sample_BlockRepetitions::
    ; Number of times to repeat the current sample block
    ds 1
NEXTU
    ;ds $DF75 - @
wSnd_PWN_LastDuty::
    ; wSnd_PWN_LastDuty contains the upper 8 bits of wSnd_PWN_DutyDelta.
    ; AUD3WAVERAM is only updated if the duty changes (or if the volume changes)
    ds 1
    ;ds $DF76 - @
wSnd_PWN_DutyDelta::
    ds 2
    ;ds $DF78 - @
wSnd_PWN_LimitLow::
    ds 1
    ;ds $DF79 - @
wSnd_PWN_Undefined::
    ; Upper 8 bits of a 16-bit variable used by Sample at address $DF78
    ds 1
    ;ds $DF7A - @
wSnd_PWN_LimitHigh::
    ds 1
    ;ds $DF7B - @
wSnd_PWN_CurrentDuty::
    ; The duty is the upper 8 bits and the cents the lower 8 bits
    ds 2
    ;ds $DF7D - @
wSnd_PWN_LastInternalVolume::
    ; AUD3WAVERAM is only updated if the volume changes (or if the duty changes)
    ds 1
ENDU
    ;ds $DF7E
wSnd_Voice_MacroReadAddress::
    ; Position of the reading frame
    ds 2*4
    ;ds $DF86 - @
wSnd_Voice_TrapKeyOffAddress::
    ; TRAP_KEYOFF and UNTRAP_KEYOFF
    ds 2*4
    ;ds $DF8E - @
wSnd_Voice_LoopIndex::
    ; LOOP
    ds 1*4
    ;$DF92 - @
wSnd_Voice_WaitIndex::
    ; WAIT
    ; Set to N and then ticks down until 0. Pass once it hits 0
    ds 2*4
    ;ds $DF9A - @
wSnd_SelectedVoice_MacroReadAddress::
    ds 2
    ;ds $DF9C - @
wSnd_SelectedVoice_Index::
    ; The current track, used to offset to access other variable such as wSnd_Voice_Priority
    ds 1
    ;ds $DF9D - @
wSnd_Voice_Priority::
    ; ADD_SET_PRIO
    ; Priority of each track
    ds 1*4
    ;ds $DFA1 - @
wSnd_Voice_UniqueID::
    ; Used with StopSFX
    ds 1*4
    ;ds $DFA5 - @
wSnd_Voice_UniqueID_Generator::
    ; When starting a SFX, increments until MusyX finds an unused unique ID to assign
    ds 1
    ;ds $DFA6 - @
wSnd_SequencerStateStart::
wSnd_sss_Status::
    ; bit 0: 1 if Track0 enabled. 0 if track does not exist or if end-of-track reached.
    ; bit 1: 1 if Track1 enabled. 0 if track does not exist or if end-of-track reached.
    ; bit 2: 1 if Track2 enabled. 0 if track does not exist or if end-of-track reached.
    ; bit 3: 1 if Track3 enabled. 0 if track does not exist or if end-of-track reached.
    ; bit 4: 1 if paused song
    ; bit 5: SETVOICE toggle bit state
    ; bit 6: 0 if SETVOICE toggle bit uninitialized. 1 if initialized. (Set to 0 when a new Program is loaded for Track 0 or 1)
    ; bit 7: 1 if playing song, 0 if song stopped or paused
    ds 1
    ;ds $DFA7 - @
wSnd_sss_SongHeaderAddress::
    ; Points to the beginning of the the .song file
    ; (i.e. the file generated by gm2song.exe, skipping the $84 program header)
    ds 2
    ;ds $DFA9 - @
wSnd_sss_Bank::
    ds 1
    ;ds $DFAA - @
wSnd_sss_Speed::
    ; By default it's bpm*1.7
    ds 2
    ; $DFAC - @
wSnd_sss_TrackToVoiceMap::
    ; Maps tracks to voice - used to turn off the right SoundMacro when a note ends
    ; bits 0-1: Voice of Track 0
    ; bits 2-3: Voice of Track 1
    ; bits 4-5: Voice of Track 2
    ; bits 6-7: Voice of Track 3
    ds 1
    ; $DFAD - @
wSnd_sss_Track_CurrentProgram::
    ds 1*4
    ;ds $DFB1 - @
wSnd_sss_ProgramLookupAddress::
    ;A pointer to the table that contains the Soundlist info
    ; (address $04-$84 before the .song file)
    ds 2
    ;ds $DFB3 - @
wSnd_sss_Track_HeaderPointer::
    ; Points to the current header track information for each track, or 0 if no track
    ds 2*4
    ;ds $DFBB - @
wSnd_sss_Track_PatternIndex::
    ; 0-3
    ds 1*4

    ; All song times are 24-bit values:
    ; 1 beat is 24 ticks, which is represented as $1800
    ; If you subtract bpm*1.71438138289 at 59.73 Hz, you get an accurate timer
    ; MusyX uses 1.7 as a multiplier as a compromise (runs at 99.161132812% speed)

    ;ds $DFBF - @
wSnd_sss_Track_PatternStartTimer::
    ; 24-bit little-endian timer
    ; When this timer hits 0, pattern specified by wSnd_sss_Track_PatternIndex starts
    ; and the next pattern indicated by wSnd_sss_Track_HeaderPointer is prepared
    ds 3*4
    ;ds $DFCB - @
wSnd_sss_Track_PatternReadAddress::
    ds 2*4
    ;ds $DFD3 - @
wSnd_sss_PatternStatus::
    ; bit 0: 1 if Track0's pattern is enabled. 0 if no pattern is yet started or if End-of-pattern is reached.
    ; bit 1: 1 if Track1's pattern is enabled. 0 if no pattern is yet started or if End-of-pattern is reached.
    ; bit 2: 1 if Track2's pattern is enabled. 0 if no pattern is yet started or if End-of-pattern is reached.
    ; bit 3: 1 if Track3's pattern is enabled. 0 if no pattern is yet started or if End-of-pattern is reached.
    ; bit 4: 1 if Track0 has a note currently playing, therefore tick the NoteEndTimer
    ; bit 5: 1 if Track1 has a note currently playing, therefore tick the NoteEndTimer
    ; bit 6: 1 if Track2 has a note currently playing, therefore tick the NoteEndTimer
    ; bit 7: 1 if Track3 has a note currently playing, therefore tick the NoteEndTimer
    ds 1
    ;ds $DFD4 - @
wSnd_sss_Track_NoteStartTimer::
    ; 24-bit little-endian timer
    ; Within a pattern, the time until the next note starts
    ds 3*4
    ;ds $DFE0 - @
wSnd_sss_LastKey::
    ; The key of the note preceding the current one
    ; Used in LASTNOTE
    ds 1*4
    ;ds $DFE4
wSnd_sss_CurrentKey::
    ; The initial input key from the midi file
    ; Used in PLAYMACRO
    ds 1*4
    ;ds $DFE8 - @
wSnd_sss_Track_NoteEndTimer::
    ; 24-bit little-endian timer
    ; Once a note has started playing, time until it should be sent a KeyOff
    ds 3*4
    ;$DFF4 - @
wSnd_Voice_Velocity::
    ;0-F for each voice
    ds 1*4
wSnd_SequencerStateEnd::
    ;ds $DFF8 - @
wSnd_SFXSongVolume::
    ; Upper 4 bits: SFX master volume
    ; Lower 4 bits: Song master volume
    ds 1

    ;ds $DFF9 - @
wSnd_Temp_9::
    ; These variables are used for short-term storage in various functions
    ds 1
    ;ds $DFFA - @
wSnd_Temp_A::
    ds 1
    ;ds $DFFB - @
wSnd_Temp_B::
    ds 1
    ;ds $DFFC - @
wSnd_Temp_C::
    ds 1
    ;ds $DFFD - @
wSnd_Temp_D::
    ds 1
    ;ds $DFFE - @
wSnd_Temp_E::
    ds 1
    ;ds $DFFF - @
wSnd_WRAM_End::
;Unused last byte::
    ds 1