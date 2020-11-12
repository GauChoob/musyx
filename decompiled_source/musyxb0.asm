SECTION "MUSYXB0", ROM0

; This is the source code for the part of the MusyX Synthesizer
; that needs to be in bank 0

; See the MusyX pdf manual for details

; This version comes from Magi-Nation.
; It seems to be a slightly updated version compared to the
; MusyX cd available on the internet

; Compared to the cd version, the order of the functions in bank0 are
; slightly different and might have small changes.

; All public functions (that start with snd_) use original names
; All private functions (that start with _snd_) have been given names

; The size of this section is fixed at $550 (in fact it's padded with nops at the end)
; However, it can be placed anywhere in bank 0

    ; $3AB0
_snd_MusyXB0:

    ; $3AB0
_snd_LoadBank:
    ; Loads the bank a
    ; Stores the value a into $FFXX, where XX = wSnd_CurrentROMBank_MirrorHRAM_Address
    ;
    ; In the case of Magi-Nation, wSnd_CurrentROMBank_MirrorHRAM_Address has a value of $00
    ;   so the Joypad register receives gibberish
    push af
    ld a, [wSnd_CurrentROMBank_HRAMAddress]
    ld c, a
    pop af
    ld [$FF00+c], a
    ld [rROMB0+$100], a
    ret

    ; $3ABB
_snd_DoSample_RepeatSameBlock:
    dec a
    ld [wSnd_Sample_BlockRepetitions], a
    ret

    ; $3AC0
snd_DoSample::
    ; Purpose:
    ;   This function needs to be called, when the timer interrupt occurs.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   To insure good quality of the sample, you need to respond to the
    ;   occurrence of the timer interrupt the moment it occurs. To do this, you
    ;   will need to allow for interrupt nesting. Please refer to the Game Boy
    ;   Development Manual, "CPU Control Register" for details.
    ;
    ; Aborts if sample is disabled (bit 6,wSnd_Voice_SampleStatus is disabled)
    ; Repeats the same sample if wSnd_Sample_BlockRepetitions is non-zero
    ; Ends the sample if the end of the sample is reached,
    ;   and call wSnd_Sample_CallbackAddress if it was defined
    ; If the sample block starts with $7 in the first 4 bits, then it's a repeating null sample
    ;   The second byte defines the number of repetitions wSnd_Sample_BlockRepetitions (max $FE)
    ;   Write $88 to all 16 bytes of the RAM
    ; Otherwise, copy the 16 bytes into RAM
        ld a, [wSnd_Voice_SampleStatus]
        bit 6, a
        ret z

        ld a, [wSnd_TMA_Mirror]
        ldh [rTIMA], a
        ldh [rTMA], a

        ld a, [wSnd_Sample_BlockRepetitions]
        and a
        jr nz, _snd_DoSample_RepeatSameBlock

        ld hl, wSnd_Sample_Length
        ld a, [hl]
        sub $01
        ld [hl+], a
        ld c, a
        ld a, [hl]
        sbc $00
        ld [hl], a
        or c
        jr z, .EndOfSample

        ld a, [wSnd_Sample_Bank]
        call _snd_LoadBank
        ld hl, wSnd_Sample_Address
        ld a, [hl+]
        ld h, [hl]
        ld l, a
        ld c, $30
        xor a
        ldh [rNR30], a
        ld a, [hl+]
        ld b, a
        and $F0
        cp $70
        jr z, .LoopingSample

        ld a, b
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a
        inc c
        ld a, [hl+]
        ld [$FF00+c], a

    .DoneLoadingSampleIntoRAM:
        ld a, $80
        ldh [rNR30], a
        ld a, [wSnd_NR04FreqUpper_Mirror+2]
        or $80 ;Restart sound
        ldh [rNR34], a
        and $7F ;Restart sound off
        ldh [rNR34], a
        ld a, l
        ld b, h
        ld hl, wSnd_Sample_Address
        ld [hl+], a
        ld [hl], b
        bit 7, b
        jr z, .SkipSampleCrossesBankBoundary
            ld [hl], $40
            ld hl, wSnd_Sample_Bank
            inc [hl]
    .SkipSampleCrossesBankBoundary:
        ret

    .EndOfSample:
        call snd_SampleOff
        ld a, [wSnd_Sample_CallbackAddress]
        ld l, a
        ld a, [wSnd_Sample_CallbackAddress+1]
        ld h, a
        or l
        ret z
        jp hl

    .LoopingSample:
        ld a, [hl+]
        ld [wSnd_Sample_BlockRepetitions], a
        ld bc, $000E
        add hl, bc
        ld c, l
        ld b, h
        ld hl, _AUD3WAVERAM
        ld a, $88
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl], a
        ld l, c
        ld h, b
        jr .DoneLoadingSampleIntoRAM

    ; $3B7C
snd_SampleOff:
    ld hl, wSnd_Voice_SampleStatus
    res 6, [hl]
    xor a
    ld [wSnd_Voice_Priority+2], a
    ldh a, [rIE]
    and $1B
    ldh [rIE], a
    ldh a, [rIF]
    and $1B
    ldh [rIF], a
    xor a
    ldh [rNR30], a
    ldh [rTAC], a
    ld a, $80
    ldh [rNR30], a
    ldh a, [rNR51]
    and $BB
    ldh [rNR51], a
    ret

    ; $3BA1
snd_TransferSampleBlock::
    push bc
    ld a, [wSnd_Sample_Bank]
    call _snd_LoadBank
    call _snd_TransferSampleBlock
    ld a, [wSnd_BaseBank]
    call _snd_LoadBank
    pop bc
    ret

    ; $3BB3
snd_InitSampleBlock::
    ; Reads the next sample block (looping or not) and
    ; loads it into the audio ram
        push bc
        ld a, [wSnd_Sample_Bank]
        call _snd_LoadBank
        ld a, [hl]
        and $F0
        cp $70
        jr nz, .NonLoopingSample

        inc hl ;Looping Sample
        ld a, [hl+]
        ld [wSnd_Sample_BlockRepetitions], a
        xor a
        ldh [rNR30], a
        ld a, $88
        ld c, $10
        push hl
        ld hl, _AUD3WAVERAM
    .LoadSilentSampleLoop:
        ld [hl+], a
        dec c
        jr nz, .LoadSilentSampleLoop
        ld a, $80
        ldh [rNR30], a
        ld bc, $000E
        pop hl
        add hl, bc
        jr .DoneLoadingSampleIntoRAM

    .NonLoopingSample:
        xor a
        ld [wSnd_Sample_BlockRepetitions], a
        call _snd_TransferSampleBlock

    .DoneLoadingSampleIntoRAM:
        ld a, [wSnd_BaseBank]
        call _snd_LoadBank
        pop bc
        ret

    ; $3BEF
snd_TransferSampleBlockRestart::
    ; Identical to snd_InitSampleBlock
    ; except (1) initializes wSnd_Sample_BlockRepetitions to 0
    ; and (2) sends Restart sound to rNR34
        push bc
        ld a, [wSnd_Sample_Bank]
        call _snd_LoadBank
        ld a, [hl]
        and $F0
        cp $70
        jr nz, .NonLoopingSample

        inc hl ;Looping Sample
        ld a, [hl+]
        ld [wSnd_Sample_BlockRepetitions], a
        xor a
        ldh [rNR30], a
        ld a, $88
        ld c, $10
        push hl
        ld hl, _AUD3WAVERAM
    .LoadSilentSampleLoop:
        ld [hl+], a
        dec c
        jr nz, .LoadSilentSampleLoop
        ld a, $80
        ldh [rNR30], a
        ld bc, $000E
        pop hl
        add hl, bc
        jr .DoneLoadingSampleIntoRAM

    .NonLoopingSample:
        call _snd_TransferSampleBlock

    .DoneLoadingSampleIntoRAM:
        ld a, [wSnd_NR04FreqUpper_Mirror+2]
        or $80  ;Restart sound
        ldh [rNR34], a
        and $7F  ;Restart sound off
        ldh [rNR34], a
        ld a, [wSnd_BaseBank]
        call _snd_LoadBank
        pop bc
        ret

    ; $3C32
_snd_TransferSampleBlock:
    ; Loads 16 bytes into RAM (non-looping)
    ld c, $30
    xor a
    ldh [rNR30], a ;off
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    inc c
    ld a, [hl+]
    ld [$FF00+c], a
    ld a, $80
    ldh [rNR30], a ;on
    ret

    ; $3C6B
snd_HardSample::
    call _snd_HardSample
    ld a, [wSnd_BaseBank]
    ld [rROMB0+$100], a
    ret

    ; $3C75
_snd_HardSample:
    ; Plays a high quality sample
    ; Instead of using channel 3,
    ; Use channels 1 and 2 in an identical manner:
    ; Take each 4-bit datum
    ; With channels 1 and 2 operating at max frequency 131072 Hz,
    ; Use the volume setting as the wave output form
    ; i.e. the volume flicks up and down following the wav info
    ; Abort when keybutton is pressed or when end of sample is reached
        ld a, [wSnd_Sample_Bank]
        ld [rROMB0+$100], a
        ld e, $00
        ld hl, wSnd_Sample_Address
        ld a, [hl+]
        ld h, [hl]
        ld l, a

    .NextBlock:
        ld a, [hl]
        ld d, a
        and $F0
        cp $70
        ld a, d
        jr z, .LoopingSample

        ld d, $20
    .NextByteOfBlock:
        bit 0, d
        jr nz, .GrabLower4Bits
        nop
        nop
        jr .Upper4BitsWereGrabbed
    .GrabLower4Bits:
        inc hl
        swap a
    .Upper4BitsWereGrabbed:
        and $F0

        cp e
        ld e, a
        jr z, .UnchangedFrequency

        ldh [rNR12], a
        ldh [rNR22], a
        ld a, $87
        ldh [rNR14], a ;Restart sound. Higher frequency %111 i.e. 131072 Hz
        ldh [rNR24], a
        jr .NewFrequencyWasSet
    .UnchangedFrequency:
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
    .NewFrequencyWasSet:

        ld a, $14
    .IdleTime:
        dec a
        jr nz, .IdleTime

        dec d
        ld a, [hl]
        jr nz, .NextByteOfBlock

    .EndBlockProcessing:
        bit 7, h
        jr z, .SkipSampleCrossesBankBoundary
            ld h, $40
            ld a, [wSnd_Sample_Bank]
            inc a
            ld [wSnd_Sample_Bank], a
            ld [rROMB0+$100], a
    .SkipSampleCrossesBankBoundary:
        push bc
        call _snd_HardSample_GetButtonKeys
        ld a, [wSnd_Temp_E] ;Abort buttons (e from snd_PlaySample)
        and b
        pop bc
        ret nz ;Abort button

        dec bc
        ld a, b
        or c
        jr nz, .NextBlock
        ret


    .LoopingSample:
        inc hl
        ld a, [hl+]
        ld [wSnd_Sample_BlockRepetitions], a
        push bc
        ld bc, $000E
        add hl, bc
        pop bc

    .LoopingFromHere:
        ld a, $77
        ld d, $20

    .NextByteOfLoopingBlock:
        bit 0, d
        jr nz, .LoopingGrabLower4BitsLoop
        jr .LoopingUpper4BitsWereGrabbed
    .LoopingGrabLower4BitsLoop:
        swap a
    .LoopingUpper4BitsWereGrabbed:
        and $F0

        cp e
        ld e, a
        jr z, .LoopingUnchangedFrequency

        ldh [rNR12], a
        ldh [rNR22], a
        ld a, $87
        ldh [rNR14], a ;Restart sound. Higher frequency %111 i.e. 131072 Hz
        ldh [rNR24], a
        jr .LoopingNewFrequencyWasSet
    .LoopingUnchangedFrequency:
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
    .LoopingNewFrequencyWasSet:

        ld a, $14
    .IdleTimeLooping:
        dec a
        jr nz, .IdleTimeLooping

        dec d
        ld a, $77
        jr nz, .NextByteOfLoopingBlock

        ld a, [wSnd_Sample_BlockRepetitions]
        dec a
        ld [wSnd_Sample_BlockRepetitions], a
        jr nz, .LoopingFromHere

        jr .EndBlockProcessing

_snd_HardSample_GetButtonKeys:
    ld c, $00
    ld a, $10 ;Select button keys
    ld [$FF00+c], a
    nop
    nop
    nop
    nop
    nop
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    ld a, [$FF00+c]
    and $0F
    cpl
    ld b, a
    ld a, $30
    ld [$FF00+c], a
    ret

    ; $3D4A
snd_sss_GetBPM::
    ; Returns bpm in hl of a song
    ld a, [wSnd_sss_Bank]
    call _snd_LoadBank
    ld a, [wSnd_sss_SongHeaderAddress]
    ld l, a
    ld a, [wSnd_sss_SongHeaderAddress+1]
    ld h, a
    ld bc, $0004 ;BPM is located at offset $0004
    add hl, bc
    ld a, [hl+]
    ld l, [hl]
    ld h, a
    ld a, [wSnd_BaseBank]
    jp _snd_LoadBank

    ; $3D65
snd_sss_SetupTracks::
    ; Sets up the default program and soundlist
    ; Sets up all 4 tracks' patterns
    ;
    ; Given wSnd_sss_SongHeaderAddress, initialize the following variables:
    ;   wSnd_sss_Track_CurrentProgram
    ;   wSnd_sss_ProgramLookupAddress
    ;   wSnd_sss_Track_HeaderPointer
    ;   wSnd_sss_Track_PatternStartTimer
    ;   wSnd_sss_Track_PatternIndex
    ld a, [wSnd_sss_Bank]
    call _snd_LoadBank

    ld a, [wSnd_sss_SongHeaderAddress]
    ld l, a
    ld a, [wSnd_sss_SongHeaderAddress+1]
    ld h, a
    push hl

    ld bc, -$84
    add hl, bc ;First 4 bytes are the default programs for the 4 tracks
    ld a, [hl+]
    ld [wSnd_sss_Track_CurrentProgram], a
    ld a, [hl+]
    ld [wSnd_sss_Track_CurrentProgram+1], a
    ld a, [hl+]
    ld [wSnd_sss_Track_CurrentProgram+2], a
    ld a, [hl+]
    ld [wSnd_sss_Track_CurrentProgram+3], a
    ld a, l
    ld [wSnd_sss_ProgramLookupAddress], a ;$04-84 is the lookup table
    ld a, h
    ld [wSnd_sss_ProgramLookupAddress+1], a

    pop hl        ;wSnd_sss_SongHeaderAddress
    ld a, [hl+]
    ld b, a
    ld a, [hl-]
    ld c, a
    ld e, l
    ld d, h     ;de = wSnd_sss_SongHeaderAddress
    add hl, bc  ;Go to track header table
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    or b
    jr z, .CheckTrack1 ;If the pointer is $0000, then the track does not exist
        ;HandleTrack0
            push hl
            ld l, e
            ld h, d
            add hl, bc ;Go to the track header
            ld a, l
            ld [wSnd_sss_Track_HeaderPointer], a
            ld a, h
            ld [wSnd_sss_Track_HeaderPointer+1], a
            ld hl, wSnd_sss_Status
            set 0, [hl] ;Activate the track
            pop hl
    .CheckTrack1:
        ld a, [hl+]
        ld b, a
        ld a, [hl+]
        ld c, a
        or b
        jr z, .CheckTrack2
        ;HandleTrack1
            push hl
            ld l, e
            ld h, d
            add hl, bc
            ld a, l
            ld [wSnd_sss_Track_HeaderPointer+2], a
            ld a, h
            ld [wSnd_sss_Track_HeaderPointer+3], a
            ld hl, wSnd_sss_Status
            set 1, [hl]
            pop hl
    .CheckTrack2:
        ld a, [hl+]
        ld b, a
        ld a, [hl+]
        ld c, a
        or b
        jr z, .CheckTrack3
        ;HandleTrack2
            push hl
            ld l, e
            ld h, d
            add hl, bc
            ld a, l
            ld [wSnd_sss_Track_HeaderPointer+4], a
            ld a, h
            ld [wSnd_sss_Track_HeaderPointer+5], a
            ld hl, wSnd_sss_Status
            set 2, [hl]
            pop hl
    .CheckTrack3:
        ld a, [hl+]
        ld b, a
        ld a, [hl+]
        ld c, a
        or b
        jr z, .FinishedChecking
        ;HandleTrack3
            push hl
            ld l, e
            ld h, d
            add hl, bc
            ld a, l
            ld [wSnd_sss_Track_HeaderPointer+6], a
            ld a, h
            ld [wSnd_sss_Track_HeaderPointer+7], a
            pop hl
            ld hl, wSnd_sss_Status
            set 3, [hl]
    .FinishedChecking:
        ld hl, wSnd_sss_Track_HeaderPointer
        ld de, wSnd_sss_Track_PatternStartTimer+2
        xor a

    .CheckEachTrackLoop:
        ld [wSnd_Temp_9], a ;Current track loop index (0-3)
        ld a, [hl+]
        ld c, a
        ld a, [hl+]
        ld b, a
        or c
        jr z, .SkipNullTrack
            ld a, [bc] ;bc = Track data
            inc bc
            ld [de], a
            dec de
            ld a, [bc]
            ld [de], a ;Store an offset (flip endianness)

            inc de
            inc bc
            ld a, [bc]
            ld [wSnd_Temp_A], a ;Pattern index
            ld bc, wSnd_sss_Track_PatternIndex
            ld a, [wSnd_Temp_9]
            add c
            ld c, a
            ld a, b
            adc $00
            ld b, a
            ld a, [wSnd_Temp_A]
            ld [bc], a ;wSnd_sss_Track_PatternIndex <- Pattern index
        .SkipNullTrack:
            inc de
            inc de
            inc de
            ld a, [wSnd_Temp_9]
            inc a
            cp $04
            jr nz, .CheckEachTrackLoop
        ld a, [wSnd_BaseBank]
        jp _snd_LoadBank

    ; $3E3A
snd_sss_SetupNextTrackPattern::
    ; After a pattern has been started for a track
    ; load the info for the next pattern
    ; Sets
    ;   wSnd_sss_Track_PatternIndex
    ;   wSnd_sss_Track_HeaderPointer
    ;   wSnd_sss_Track_PatternStartTimer
    ; a = Track index
    sla a
    ld e, a
    ld d, $00
    ld a, [wSnd_sss_Bank]
    call _snd_LoadBank
    ld hl, wSnd_sss_Track_HeaderPointer
    add hl, de
    push hl
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    ld bc, $0003
    add hl, bc
    pop bc
    ld a, l
    ld [bc], a
    inc bc
    ld a, h
    ld [bc], a ;[wSnd_sss_Track_HeaderPointer] += 3 (next pattern)

    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    ld a, [hl]

    ld hl, wSnd_sss_Track_PatternIndex
    srl e
    add hl, de
    ld [hl], a

    ld a, e
    sla e
    add e
    ld e, a
    ld hl, wSnd_sss_Track_PatternStartTimer+1
    add hl, de
    ld a, [hl]
    add c
    ld [hl+], a
    ld a, [hl]
    adc b
    ld [hl], a ; wSnd_sss_Track_PatternStartTimer += TimeToStartNextPattern

    ld a, [wSnd_BaseBank]
    jp _snd_LoadBank

    ; $3E78
snd_sss_HandleLoopPattern::
    ; Handles a PATTERNB loop
    ; Sets
    ;   wSnd_sss_Track_PatternIndex
    ;   wSnd_sss_Track_HeaderPointer
    ;   wSnd_sss_Track_PatternStartTimer
    ; a = Track index
    sla a
    ld e, a
    ld d, $00
    ld a, [wSnd_sss_Bank]
    call _snd_LoadBank

    ld hl, wSnd_sss_Track_HeaderPointer
    add hl, de
    push hl
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    ld bc, $0003
    add hl, bc

    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    push bc ;Offset to target PATTERNA

    ld a, [hl+]
    ld b, a
    ld c, [hl] ;New wSnd_sss_Track_PatternStartTimer for target PATTERNA

    push de
    ld a, e
    srl a
    add e
    ld e, a
    ld hl, wSnd_sss_Track_PatternStartTimer+1
    add hl, de
    ld a, [hl]
    add c
    ld [hl+], a
    ld a, [hl]
    adc b
    ld [hl], a ;wSnd_sss_Track_PatternStartTimer += new time
    pop de

    ld a, [wSnd_sss_SongHeaderAddress]
    ld l, a
    ld a, [wSnd_sss_SongHeaderAddress+1]
    ld h, a
    ld a, [hl+]
    ld b, a
    ld a, [hl-]
    ld c, a
    push hl
    add hl, bc
    add hl, de
    ld a, [hl+]
    ld b, a
    ld c, [hl]
    pop hl
    add hl, bc
    pop bc
    add hl, bc   ;bc = Target Track header + Offset to target PATTERNA

    pop bc
    ld a, l
    ld [bc], a
    inc bc
    ld a, h
    ld [bc], a ;Update wSnd_sss_Track_HeaderPointer

    inc hl
    inc hl
    ld a, [hl]
    ld hl, wSnd_sss_Track_PatternIndex
    srl e
    add hl, de
    ld [hl], a ;Update wSnd_sss_Track_PatternIndex

    ld a, [wSnd_BaseBank]
    jp _snd_LoadBank

    ; $3ED4
snd_sss_SetupPattern::
    ; Loads the correct starting address into wSnd_sss_Track_PatternReadAddress
    ;   Sets wSnd_sss_Track_PatternReadAddress
    ; a = track index
    ; c = pattern index
    sla a
    ld e, a
    ld d, $00
    push de
    ld b, $00
    sla c
    ld a, [wSnd_sss_Bank]
    push bc
    call _snd_LoadBank
    pop bc

    ld a, [wSnd_sss_SongHeaderAddress]
    ld l, a
    ld a, [wSnd_sss_SongHeaderAddress+1]
    ld h, a
    inc hl
    inc hl
    ld a, [hl+]
    ld d, a
    ld a, [hl-]
    ld e, a
    dec hl
    dec hl
    push hl
    add hl, de
    add hl, bc  ;Pattern table + offset for pattern index
    ld a, [hl+]
    ld l, [hl]
    ld h, a
    pop bc
    add hl, bc
    ld c, l
    ld b, h ;Address of start of pattern

    pop de
    ld hl, wSnd_sss_Track_PatternReadAddress
    add hl, de
    ld a, c
    ld [hl+], a
    ld [hl], b

    ld a, [wSnd_BaseBank]
    jp _snd_LoadBank

    ; $3F0E
snd_sss_SetupPatternNote::
    ; Gets information about the current note to play
    ; a = InputTrack
    ; Outputs:
    ;   bc = Note duration (time to NoteOff)
    ;   d = Note Key
    ;       For Event with or without program, bit 7 = 0 and bit 6-0 contains the key
    ;       $FF for end of pattern
    ;       For ProgramChangeEvent, bit 7 = 1 and bit 6-0 contains the program
    ;   e = Velocity
    ;   a = Program
    ;       For Event without program and end of pattern, $00
    ;       For Event with program or ProgramChangeEvent, bit 7 = 1 and bit 6-0 contains the program
    sla a
    ld e, a
    ld d, $00
    ld a, [wSnd_sss_Bank]
    call _snd_LoadBank

    xor a
    ld [wSnd_Temp_A], a

    ld hl, wSnd_sss_Track_PatternReadAddress
    add hl, de
    push hl
    ld a, [hl+]
    ld h, [hl]
    ld l, a

    ld a, [hl+]
    inc hl
    swap a
    and $0F
    ld e, a ;Velocity

    ld a, [hl+] ;NoteOn byte
    ld d, a
    bit 7, a ;HasProgram bit
    jr z, .GetNoteDuration ;There is no Program

    cp $FF
    jr z, .EndOfPattern ;End-of-pattern

    .EventWithProgramByte:
        and $7F
        ld d, a ;Store note without the flag
        ld a, e
        and a
        jr z, .ProgramChangeEvent
        ;EventWithProgramByte
            ld a, [hl+]
            or $80 ;set bit 7
            ld [wSnd_Temp_A], a
            jr .GetNoteDuration

    .EndOfPattern:
        ld d, a ;End of pattern: $F? $?? $FF
        ld a, e
        cp $0F
        ld a, d
        jr nz, .EventWithProgramByte ;If velocity is not $0F, then it's not actually an end of pattern
        jr .IncrementPatternReadAddress

    .ProgramChangeEvent:
        ld a, d
        or $80 ;set bit 7
        ld d, a
        ld [wSnd_Temp_A], a
        jr .IncrementPatternReadAddress

    .GetNoteDuration:
        ld a, [hl+] ;NoteDuration, Variable Length Quantity
        bit 7, a
        jr z, .OneByteLong
            ;2-Bytes-Long
            and $7F
            ld b, a
            ld a, [hl+]
            ld c, a
            jr .IncrementPatternReadAddress
        .OneByteLong:
            ld c, a
            ld b, $00

    .IncrementPatternReadAddress:
    ld a, h
    ld [wSnd_Temp_C], a
    ld a, l
    pop hl
    ld [hl+], a
    ld a, [wSnd_Temp_C]
    ld [hl], a ;wSnd_sss_Track_PatternReadAddress

    ld a, [wSnd_BaseBank]
    push bc
    call _snd_LoadBank
    pop bc
    ld a, [wSnd_Temp_A]
    ret

    ; $3F7D
snd_sss_SetupNoteStartDelay::
    ; Adds the time for the note to start from an Event to wSnd_sss_Track_NoteStartTimer
    ; Sets
    ;   wSnd_sss_Track_NoteStartTimer
    ; a = track
    sla a
    ld e, a
    ld d, $00
    ld a, [wSnd_sss_Bank]
    call _snd_LoadBank

    ld hl, wSnd_sss_Track_PatternReadAddress
    add hl, de
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a

    ld hl, wSnd_sss_Track_NoteStartTimer+1
    push de
    ld a, e
    srl a
    add e
    ld e, a
    add hl, de
    pop de
    ld a, [hl]
    add c
    ld [hl+], a
    push af
    ld a, b
    and $0F
    ld c, a
    pop af
    ld a, [hl]
    adc c
    ld [hl], a ; ;wSnd_sss_Track_NoteStartTimer += DeltaTime

    ld a, [wSnd_BaseBank]
    jp _snd_LoadBank

    ; $3FB0
snd_sss_GetProgram::
    ; It returns the value of [wSnd_sss_ProgramLookupAddress+a]
    ; a = program
    ; Returns a = SoundMacroID
    push de
    push hl
    push bc
    ld e, a
    ld d, $00
    ld hl, wSnd_sss_ProgramLookupAddress
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    add hl, de
    ld a, [wSnd_sss_Bank]
    call _snd_LoadBank
    ld e, [hl]
    ld a, [wSnd_BaseBank]
    call _snd_LoadBank
    ld a, e
    pop bc
    pop hl
    pop de
    ret

    ; $3FCF
REPT ($550 + _snd_MusyXB0) - @ ;$31 repeats
    nop
ENDR
    ; $4000