SECTION "MUSYX", ROMX[$4000]

; This is the source code for the main MusyX Synthesizer.
; See the MusyX pdf manual for details

; This version comes from Magi-Nation.
; It seems to be a slightly updated version compared to the
; MusyX cd available on the internet

; This bank is extremely similar to the cd version,
; but there might be very small changes

; There are two undocumented public functions in this version
; called snd_SFXPitch and snd_SFXVolume.

; All public functions (that start with snd_) use original names
; All private functions (that start with _snd_) have been given names

    ; $4000
_snd_c_Times_a:
    ; Multiply c times a and store in hl
    ld b, $00

    ; $4002
_snd_bc_Times_a:
    ; Multiply bc times a and store in hl
    ld hl, $0000
    rrca
    jr nc, .Bit1
        add hl, bc
    .Bit1:
    sla c
    rl b
    rrca
    jr nc, .Bit2
        add hl, bc
    .Bit2:
    sla c
    rl b
    rrca
    jr nc, .Bit3
        add hl, bc
    .Bit3:
    sla c
    rl b
    rrca
    jr nc, .Bit4
        add hl, bc
    .Bit4:
    sla c
    rl b
    rrca
    jr nc, .Bit5
        add hl, bc
    .Bit5:
    sla c
    rl b
    rrca
    jr nc, .Bit6
        add hl, bc
    .Bit6:
    sla c
    rl b
    rrca
    jr nc, .Bit7
        add hl, bc
    .Bit7:
    sla c
    rl b
    rrca
    ret nc
        add hl, bc
        ret

    ; $4041
_snd_hl_Times_bc:
    ; Multiplies hl*bc and store in 32-bit bchl
        push de
        ld e, l
        ld d, h
        ld hl, $0000
        xor a
        ld [wSnd_Temp_9], a
        ld [wSnd_Temp_A], a ;The product is extended to 32-bit A9hl (TempA, Temp9, h, l)
        ld [wSnd_Temp_B], a
        ld [wSnd_Temp_C], a ;hl is extended to 32-bit CBde (TempC, TempB, d, e)
        ld a, $10
    .BitLoop:
        ld [wSnd_Temp_D], a ;Counter of bit position
        rr b
        rr c
        jr nc, .SkipMultiply
            add hl, de
            push hl
            ld a, [wSnd_Temp_B]
            ld l, a
            ld a, [wSnd_Temp_9]
            adc l
            ld [wSnd_Temp_9], a
            ld a, [wSnd_Temp_C]
            ld l, a
            ld a, [wSnd_Temp_A]
            adc l
            ld [wSnd_Temp_A], a

            pop hl
            ld a, [wSnd_Temp_9] ;?
            adc $00
            ld [wSnd_Temp_9], a
            ld a, [wSnd_Temp_A]
            adc $00
            ld [wSnd_Temp_A], a
        .SkipMultiply:
            sla e
            rl d
            ld a, [wSnd_Temp_B]
            rla
            ld [wSnd_Temp_B], a
            ld a, [wSnd_Temp_C]
            rla
            ld [wSnd_Temp_C], a
            ld a, [wSnd_Temp_D]
            dec a
            jr nz, .BitLoop
        ld a, [wSnd_Temp_9]
        ld c, a
        ld a, [wSnd_Temp_A]
        ld b, a
        pop de
        ret

    ; $40AA
_snd_hl_Div_bc_preserve_de:
    push de
    call _snd_hl_Div_bc
    pop de
    ret

    ; $40B0
_snd_hl_Div_bc:
    ; Does hl/bc
    ; Returns hl with remainder bc
    ;
    ; Does elementary-school style long-division in binary
    ; There can be off-by-one errors for divisions with no remainder
    ;   (Result might be 1 less than expected, with a large remainder)
        call _snd_cp_hl_bc
        jr z, .HLequalsBC
        ld a, b
        or c
        jr z, .DivideByZero

        ld a, l
        ld [wSnd_Temp_9], a
        ld a, h
        ld [wSnd_Temp_A], a ;A9 = dividend
        ld a, c
        ld [wSnd_Temp_B], a
        ld a, b
        ld [wSnd_Temp_C], a ;CB = divisor
        ld e, $0F ;Number of shifts counter
        ld a, $01
        ld [wSnd_Temp_D], a ;DE = divisor_offset
        xor a
        ld [wSnd_Temp_E], a
        ld a, [wSnd_Temp_9]
        ld l, a
        ld a, [wSnd_Temp_A]
        ld h, a
        ld a, [wSnd_Temp_B]

    .ShiftLeftUntilDivisorBiggerThanDividend:
        ld c, a
        ld a, [wSnd_Temp_C]
        ld b, a
        call _snd_cp_hl_bc ;cp dividend, divisor
        jr c, .DivisorBiggerThanDividend

        ld a, [wSnd_Temp_B]
        sla a
        ld [wSnd_Temp_B], a
        ld a, [wSnd_Temp_C]
        rla
        ld [wSnd_Temp_C], a ;divisor = divisor << 1

        ld a, [wSnd_Temp_D]
        sla a
        ld [wSnd_Temp_D], a 
        ld a, [wSnd_Temp_E]
        rla
        ld [wSnd_Temp_E], a ;divisor_offset = divisor_offset << 1

        ld a, e
        dec a
        ld e, a
        bit 7, a
        jr z, .ShiftLeftUntilDivisorBiggerThanDividend

        ld hl, $0000 ;Shifted through all bits and failed to find a good frame
        ld a, [wSnd_Temp_9] ;Return 0 with remainder dividend
        ld c, a
        ld a, [wSnd_Temp_A]
        ld b, a
        ret

    .HLequalsBC:
        ld hl, $0001
        ld bc, $0000
        ret

    .DivideByZero:
        ld hl, $0000
        ld bc, $0000
        ret

    .DivisorBiggerThanDividend:
        xor a
        ld e, a
        ld d, a ;de = quotient

    .ShiftFrameRight:
        ld a, [wSnd_Temp_E]
        srl a
        ld [wSnd_Temp_E], a
        ld b, a
        ld a, [wSnd_Temp_D]
        rra
        ld [wSnd_Temp_D], a ;divisor_offset = divisor_offset >> 1
        or b
        jr z, .ReturnResult ;End if divisor_offset = 0

        ld a, [wSnd_Temp_C]
        srl a
        ld [wSnd_Temp_C], a
        ld a, [wSnd_Temp_B]
        rra
        ld [wSnd_Temp_B], a ;divisor = divisor >> 1

        ld a, [wSnd_Temp_9]
        ld l, a
        ld a, [wSnd_Temp_A] ;dividend
        ld h, a
        ld a, [wSnd_Temp_B]
        ld c, a
        ld a, [wSnd_Temp_C] ;divisor
        ld b, a
        call _snd_cp_hl_bc ;cp dividend, divisor
        jr c, .ShiftFrameRight ; divisor > dividend, shift frame and check again

        ld a, c
        cpl
        add $01
        ld c, a
        ld a, b
        cpl
        adc $00
        ld b, a
        add hl, bc
        ld a, l
        ld [wSnd_Temp_9], a
        ld a, h
        ld [wSnd_Temp_A], a ;dividend -= divisor

        ld a, [wSnd_Temp_D]
        ld l, a
        ld a, [wSnd_Temp_E]
        ld h, a
        add hl, de
        ld e, l
        ld d, h             ;quotient += divisor_offset

        jr .ShiftFrameRight

    .ReturnResult:
        ld l, e
        ld h, d
        ld a, [wSnd_Temp_9]
        ld c, a
        ld a, [wSnd_Temp_A] ;hl = quotient
        ld b, a             ;bc = remainder
        ret

    ; $418D
_snd_cp_hl_bc:
    ; Equivalent to cp hl,bc
    ; Carry if bc>hl
    ; Zero if bc=hl
    ld a, h
    cp b
    ret c  ; b>h, so C, NZ
    jr z, .CheckLower
        xor a 
        inc a
        ret ;b<h, so NC, NZ
    .CheckLower:
        ld a, l
        cp c     ;b=h
        ret      ;return cp l,c (set carry and zero flag accordingly)

    ; $4198
_snd_cp_hl_de:
    ; Equivalent to cp hl,de
    ; Carry if de>hl
    ; Zero if de=hl
    ld a, h
    cp d
    ret c  ; d>h, so C, NZ
    jr z, .CheckLower
        xor a
        inc a
        ret ;d<h, so NC, NZ
    .CheckLower:
        ld a, l
        cp e     ;b=h
        ret      ;return cp l,e (set carry and zero flag accordingly)

    ; $41A3
_snd_rnd_256:
    ; Returns a random number from 0-255 in a
    ld a, [wSnd_RandomSeed]
    rla
    jr c, .SkipXor
        xor $E9
    .SkipXor:
        ld [wSnd_RandomSeed], a
        ret

    ; $41AF
_snd_rnd_b:
    ; Returns a random number from 0 to (b-1) in a
    call _snd_rnd_256
    cp b
    ret c
    .ModuloLoop:
        sub b
        cp b
        jr nc, .ModuloLoop ; _snd_rnd_256 % b
    ret

    ; $41B9
_snd_rnd_h00:
    ; Supposed to return a random number from 0 to (hl-1) (i.e. _snd_rnd_hl)
    ; In reality, returns a random number from 0 to (h*$100-1)
    ; If h is less than 1, buggy
    ;   if l is 0, returns 0
    ;   if l is non-zero, returns a random number from 0-(a-1)
    ; The number is returned in hl
    ;
    ; This function is horribly bugged
    ; (1), it returns a random number in the incorrect range
    ; (2), it accidentally uses a instead of l for hl < $100
    push bc
    inc h
    dec h
    jr z, .hIs0
        ld b, h
        call _snd_rnd_b
        cp h
        ld h, a
        jr nc, .NeverHappens ;rnd_b returns 0-(b-1), so the output is NEVER = h
            ld a, [wSnd_RandomSeed] ;In theory, if h would randomize to any number 0-(h-1), then l can randomize to any number 00-$FF
            rla
            jr c, .SkipXor
                xor $E9
            .SkipXor:
            ld [wSnd_RandomSeed], a
            ld l, a
            pop bc
            ret
        .NeverHappens: ;In theory, if h would randomize to h, then l would have to be randomized between 0-l
            ld b, l
            call _snd_rnd_b
            ld l, a
            pop bc
            ret
    .hIs0:
        inc l
        dec l
        jr nz, .lIsNot0
            pop bc
            ret ;Return hl, i.e. 0
    .lIsNot0:
        ld b, a ;Bug? Should be "ld b, l"
        call _snd_rnd_b
        ld l, a
        pop bc
        ret

    ; $41E8
snd_Silence::
    ; Purpose:
    ;   This function will immediately stop all sounds and any active song, but
    ;   will leave MusyX in an active state.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   None.
    xor a
    ld [wSnd_sss_Status], a ;Disable song

    ; $41EC
_snd_Silence_SkipSongStatus:
    xor a
    ld [wSnd_Voice_SampleStatus], a ;Disable voice 2

    .LoopVoice:
        push af
        ld [wSnd_SelectedVoice_Index], a
        ld c, a
        ld b, $00
        xor a
        ld hl, wSnd_Voice_Priority ;Set priority to 0
        add hl, bc
        ld [hl], a
        sla c
        ld hl, wSnd_Voice_MacroReadAddress ;Set read address to 0
        add hl, bc
        ld [hl+], a
        ld [hl], a
        call _snd_KeyOff_SelectedTrack ;Send keyoff
        call _snd_MacroOp_0D_VOICE_OFF ;Turn off voice
        pop af
        inc a
        cp $04
        jr nz, .LoopVoice
    ret

    ; $4212
snd_Exit::
    ; Purpose:
    ;   This function immediately stops all sounds and disables MusyX.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   After calling snd_Exit, you will need to call snd_Init aqain, prior to any
    ;   other API call.
    xor a
    ld [wSnd_Initialized], a
    call snd_Silence
    xor a
    ldh [rNR52], a
    ret

    ; $421D
snd_PlaySample::
    ; Purpose:
    ;   This function suspends MusyX, stops all sound output, then plays back a
    ;   high quality sample using all CPU power. When playback is completed, it
    ;   resumes MusyX and your application program.
    ;   
    ; Inputs:
    ;   A   ROM bank number of the sample to play back
    ;   HL  ROM address of the sample to play back
    ;   BC  Length of the sample / 16
    ;   E   Bit mask for the buttons A,B,SELECT and START.
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   The bitmask you supply, identifies one or more buttons that can cancel
    ;   playback of the sample. This is required, because no interrupts occur
    ;   during playback, and your application is suspended, as well.
    ;   
    ;   The sample cannot be longer than 1 MByte.
    ;   
    ;   The sample start address needs to be aligned on a 16 byte boundary.
    ;
    ; wSnd_Temp_E <- E
    ; PlaySample is always run in normal speed on GBC
    di
    push af
    push hl

    ld a, [wSnd_GameboyModel]
    cp $81
    jr nz, .AlreadyInNormalMode ;DMG
    ld hl, rKEY1
    bit 7, [hl]
    jr z, .AlreadyInNormalMode ;CGB in normal mode
            call .GBCSpeedSwitch ;Switch to normal, run the function, then go back to double
            pop hl
            pop af
            call .PlaySample
            call .GBCSpeedSwitch
            ret
    .AlreadyInNormalMode:
        pop hl
        pop af

    ; $423C
    .PlaySample:
        di
        ld [wSnd_Sample_Bank], a
        ld a, l
        ld [wSnd_Sample_Address], a
        ld a, h
        ld [wSnd_Sample_Address+1], a
        ld a, e
        ld [wSnd_Temp_E], a ;Abort buttons (e from snd_PlaySample)
        push bc
        call _snd_Silence_SkipSongStatus
        xor a
        ldh [rNR52], a
        ld a, $80
        ldh [rNR52], a ;Reset sound
        ld a, $77
        ldh [rNR50], a ;Max volume
        ld a, $33
        ldh [rNR51], a ;Only enable channel 1 and 2
        ld a, $C0
        ldh [rNR11], a ;Wave pattern 75%
        ldh [rNR21], a
        ld a, $F0
        ldh [rNR12], a ;Max volume envelopes
        ldh [rNR22], a
        ld a, $FF
        ldh [rNR13], a ;Lower 8 bits frequency $FF
        ldh [rNR23], a
        ld a, $87
        ldh [rNR14], a ;Restart sound. Higher frequency %111 i.e. 131072 Hz
        ldh [rNR24], a
        pop bc
        call snd_HardSample
        xor a
        ldh [rNR52], a
        ld a, $80
        ldh [rNR52], a ;Reset sound
        ld a, $77
        ldh [rNR50], a ;Max volume
        ei
        ret

    ; $4288
    .GBCSpeedSwitch:
        ; Swap the GBC between doublespeed<->normalspeed
        ld hl, rKEY1
        set 0, [hl]
        ldh a, [rIE]
        ld l, a
        xor a
        ldh [rIE], a
        ldh [rIF], a
        ld a, $30
        ldh [rP1], a
        stop
        nop
        ld a, l
        ldh [rIE], a
        ret

    ; $42A0
snd_StartSample::
    ; Purpose:
    ;   This function will start the playback of a normal quality sample.
    ;   
    ; Inputs:
    ;   A   ROM bank number of the sample to play back
    ;   HL  ROM address of the sample to play back
    ;   BC  Length of the sample / 16
    ;   DE  Bank0 address of a callback to call when sample ends
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   The sample cannot be longer than 1 MByte. This function requires the
    ;   availability of the timer interrupt. The sample address needs to be
    ;   aligned to 16 byte boundaries.
    push hl
    push af
    push bc
    push de
    xor a
    ld [wSnd_Voice_Priority+2], a ;hijack the priority to guarantee that the new sample plays

    ld b, $FF ;priority $FF
    ld c, $02 ;voice 2
    ld hl, $0000 ;No ReadAdress
    inc a
    ld [wSnd_Temp_E], a  ;(taken as SoundMacro id)
    call _snd_TryAssignSoundMacroToVoiceC_andResetVoiceUnlessDisabled ;assign voice

    pop de
    ld hl, wSnd_Sample_CallbackAddress
    ld a, e
    ld [hl+], a
    ld [hl], d

    pop bc
    pop af
    pop hl
    ld [wSnd_Sample_Bank], a
    call snd_InitSampleBlock ;Load the first row of data

    bit 7, h
    jr z, .SkipSampleCrossesBankBoundary
        ld h, $40
        ld a, [wSnd_Sample_Bank]
        inc a
        ld [wSnd_Sample_Bank], a
    .SkipSampleCrossesBankBoundary:
    push hl

    call _snd_EnableSampleTimer
    ld a, $50
    ld [wSnd_Voice_SampleStatus], a ;set bits 6 and 4

    ld hl, wSnd_Sample_Length
    ld a, c
    ld [hl+], a
    ld a, b
    ld [hl+], a
    ld [hl], $00

    ld hl, wSnd_Sample_Address
    pop bc
    ld a, c
    ld [hl+], a
    ld [hl], b

    ld a, $FF ;don't load the sample since we loaded it above
    ld c, $02 ;voice
    call _snd_VoiceOn

    ld a, $0F
    ld [wSnd_Voice_Velocity+2], a
    ld hl, wSnd_Voice_VolumeStatus+2
    set 5, [hl] ;max volume
    ret

    ; $42FE
snd_StopSample::
    ; Purpose:
    ;   This function will stop a sample being played back.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   None.
    ld hl, wSnd_Voice_SampleStatus
    bit 4, [hl] ;make sure sample was started from snd_StartSample
    ret z
    jp snd_SampleOff

    ; $4307
snd_SetMasterVolume::
    ; Purpose:
    ;   This function is used to set a new master volume in the final mixing
    ;   circuit. It does affect both sound effects and music.
    ;   
    ; Inputs:
    ;   A   New master volume (0-7)
    ;   
    ; Output:
    ;   Previous master volume.
    ;   
    ; Remarks:
    ;   This setting works together with the snd_SetSFXVolume and
    ;   snd_SetSongVolume. It acts as a final volume scaler.
    ;   
    ; See also:
    ;   snd_SetSFXVolume, snd_SetSongVolume
    ;
    ; BUG!!! If you provide a number 0-7 it will set the number to 7
    ; You need to provide a higher number modulo 8 e.g. $80-$87 and it will work
        push hl
        push bc
        cp $07
        jr nc, .InvalidNumber ;BUG this should be jr c, @+4
            ld a, $07 ;BUG - The number will be reset to 7 if you provide 0-7
    .InvalidNumber: ;You need to provide a larger number like $80-$87 to provide accepted input
        and $07
        ld b, a
        swap b
        or b
        ld hl, rNR50
        ld c, [hl]
        ld [hl], a
        ld a, c
        and $07
        pop bc
        pop hl
        ret

    ; $4320
snd_SetSongVolume::
    ; Purpose:
    ;   This function sets a new master volume for the playback of a song.
    ;   Sound effects are not affected.
    ;   
    ; Inputs:
    ;   A   New master volume (0-15)
    ;   
    ; Output:
    ;   A   Previous master volume
        push bc
        cp $0F
        jr c, .ValidNumber
            ld a, $0F ;Max value is $0F
    .ValidNumber:
        and $0F
        ld b, a
        ld a, [wSnd_SFXSongVolume]
        ld c, a
        and $F0
        or b
        ld [wSnd_SFXSongVolume], a
        ld a, c
        and $0F
        pop bc
        ret

    ; $4339
snd_SetSFXVolume::
    ; Purpose:
    ;   This function sets a new master volume for the sound effects.
    ;   The volume of a song remains unaffected.
    ;   
    ; Inputs:
    ;   A New master volume (0-15)
    ;   
    ; Output:
    ;   A Previous master volume
        push bc
        cp $0F
        jr c, .ValidNumber
            ld a, $0F
    .ValidNumber:
        and $0F
        swap a
        ld b, a
        ld a, [wSnd_SFXSongVolume]
        ld c, a
        and $0F
        or b
        ld [wSnd_SFXSongVolume], a
        ld a, c
        swap a
        and $0F
        pop bc
        ret

    ; $4356
snd_Handle::
    ; Purpose:
    ;   This function needs to be called once every vertical blank interrupt.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   To ensure the best possible timing for samples and songs, call this
    ;   function as soon as possible in vertical blank. This assumes that all tasks
    ;   you do before calling this function will take approximately the same time,
    ;   every interrupt.
    ;   
    ;   You will also need to allow interrupt nesting (see Game Boy
    ;   Development Manual, Revision G, "CPU Control Register") right before
    ;   calling this function, if you are utilizing normal quality samples.
        ld a, [wSnd_Initialized]
        cp $F5
        ret nz ;initialization check

        ld hl, wSnd_Voice_Modulation_DeltaKeyCents
        xor a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl], a
        ld a, [wSnd_Voice_SampleStatus]
        bit 7, a
        call nz, _snd_HandleLowQSample
        call _snd_HandleMacros
        call _snd_HandleModulation
        call _snd_HandleEnvelopes
        ld a, [wSnd_Voice_SampleStatus]
        bit 0, a
        call nz, _snd_HandlePWN

        ld bc, $0003
        jr .checkVolUpdate
    .checkFreqUpdate:  ;Checks for voices 0-2
        ld hl, wSnd_Voice_FrequencyStatus
        add hl, bc
        bit 5, [hl]
        jr z, .checkVolUpdate
            res 5, [hl]
            set 6, [hl]
            push bc
            sla c
            ld d, $00
            ld e, c
            ld hl, wSnd_Voice_KeyCents
            add hl, de
            ld a, [hl+]
            ld b, [hl]
            ld c, a
            ld hl, wSnd_Voice_Modulation_DeltaKeyCents
            add hl, de
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            add hl, bc
            ld c, l
            ld b, h
            ld hl, wSnd_Voice_AddNote_DeltaKeyCents
            add hl, de
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            add hl, bc
            pop bc
            call _snd_SendFrequencyToRegister ;wSnd_Voice_KeyCents+wSnd_Voice_Modulation_DeltaKeyCents+wSnd_Voice_AddNote_DeltaKeyCents
    .checkVolUpdate:
        ld hl, wSnd_Voice_VolumeStatus ;Checks for voices 0-3
        add hl, bc
        bit 5, [hl]
        jr z, .SkipVolUpdate
            res 5, [hl] ;Update volume registers
            ld hl, wSnd_Voice_Velocity
            add hl, bc
            ld a, [hl]
            ld hl, wSnd_Voice_EnvelopeVolume
            add hl, bc
            ld e, [hl]
            call _snd_CalculateOutputVolume
            push bc
            call _snd_SendVolumeToRegister ;wSnd_Voice_Velocity*wSnd_Voice_EnvelopeVolume
            pop bc
    .SkipVolUpdate:
        dec c
        bit 7, c
        jr z, .checkFreqUpdate

        ld a, [wSnd_GameboyModel] ;End of loop
        bit 0, a
        call nz, _snd_CGB_UnknownSoundLength
        call _snd_HandleSong
        ret


    ; $43E0
_snd_HandlePWN:
    ld hl, wSnd_PWN_DutyDelta
    ld a, [hl+]
    ld d, [hl]
    ld e, a
    ld hl, wSnd_PWN_CurrentDuty
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    add hl, de
    ld b, h
    ld c, l ;Modify the Duty

    bit 7, d
    jr nz, .DescendingDelta
            ld a, [wSnd_PWN_LimitHigh] ;Ascending Delta
            ld h, a
            ld l, $00
            call _snd_cp_hl_bc
            jr nc, .SaveNewDuty ;Did not hit LimitHigh
        .ReflectDutyAndFlipDelta:
            ld a, c
            cpl
            add $01
            ld c, a
            ld a, b
            cpl
            adc $00
            ld b, a ;bc = -bc
            sla h
            add hl, bc ; hl = Limit - (Duty-Limit) (i.e. reflect the overshoot back - works for LimitHigh and LimitLow)
            ld b, h
            ld c, l

            ld a, e
            cpl
            add $01
            ld e, a
            ld a, d
            cpl
            adc $00
            ld d, a ;de = -de
            ld hl, wSnd_PWN_DutyDelta
            ld a, e
            ld [hl+], a
            ld [hl], d ;Invert the wSnd_PWN_DutyDelta

        .SaveNewDuty:
            ld hl, wSnd_PWN_CurrentDuty
            ld a, c
            ld [hl+], a
            ld [hl], b
            jr .FinishedStoringTargetKeyCents

    .DescendingDelta:
            ld a, [wSnd_PWN_LimitLow]
            ld l, a
            bit 7, b
            jr nz, .PassedLowLimit ;Negative number -> flip
            ld a, b
            cp l                  ;Less than LimitLower -> flip
            jr nc, .SaveNewDuty2
            .PassedLowLimit:
                ld h, l
                ld l, $00
                jr .ReflectDutyAndFlipDelta
            .SaveNewDuty2:
                ld hl, wSnd_PWN_CurrentDuty
                ld a, c
                ld [hl+], a
                ld [hl], b

    .FinishedStoringTargetKeyCents:
        ld a, [wSnd_PWN_LastDuty]
        cp b
        jr nz, .UpdateRegister ;Update if the duty changes
        ld a, [wSnd_PWN_LastInternalVolume]
        ld c, a
        ld a, [wSnd_Voice_InternalVolume+2] ;Or if the volume changes
        cp c
        ret z
    .UpdateRegister:
        ld a, b
        ld [wSnd_PWN_LastDuty], a ;Save the new duty
        ld c, a
        ld d, $00
        srl a
        ld e, a
        ld a, $07
        sub e ; a = 7-Duty/2. c = Duty (to check bit 0)
        ld e, a
        ld hl, .UpdateAudioRAM
        add hl, de
        push hl
        xor a
        ldh [rNR30], a
        ld a, [wSnd_Voice_InternalVolume+2]
        ld [wSnd_PWN_LastInternalVolume], a ;Save the new volume
        ld hl, _AUD3WAVERAM
        ld [hl+], a
        ld b, a
        swap a
        or b
        ret ;UpdateAudioRAM

    ; $4470
    .UpdateAudioRAM:
        ; X number of "ld [hl+], a" commands are skipped, depending on parameters in _snd_HandlePWN
        ld [hl+], a   ;Duty = E-F  15-16 nibbles
        ld [hl+], a   ;Duty = C-D  13-14 nibbles
        ld [hl+], a   ;Duty = A-B  11-12 nibbles
        ld [hl+], a   ;Duty = 8-9  9-10 nibbles
        ld [hl+], a   ;Duty = 6-7  7-8 nibbles
        ld [hl+], a   ;Duty = 4-5  5-6 nibbles
        ld [hl+], a   ;Duty = 2-3  3-4 nibbles
        bit 0, c      ;Duty = 0-1  1-2 nibbles
        jr z, .SkipLastNibble
            and $F0
            ld [hl+], a   ;If Duty is odd, write the last nibble
        .SkipLastNibble:
        xor a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a
        ld [hl+], a ;Erase the previous duty cycle RAM
        ld [hl], a
        ld a, $80
        ldh [rNR30], a ;Sound on
        xor a
        ldh [rNR31], a ;Sound length 1 second
        ld a, $20
        ldh [rNR32], a ;100% volume (volume is controlled by the value of a instead)
        ld a, [wSnd_NR04FreqUpper_Mirror+2]
        or $80
        ldh [rNR34], a ;Reset sound to start
        and $7F
        ldh [rNR34], a
        ret


    ; $449D
_snd_Init_Registers:
    ; Initializes sound registers
    xor a
    ld hl, rNR50    ;Turns off all sound registers
    ld [hl+], a     ;NR50
    ld [hl+], a     ;NR51
    ld [hl], a      ;NR52
    ld a, $80
    ld [hl], a      ;Turns on sound (NR52)
    ld c, $10       
    ld a, $08
    ld [$FF00+c], a ;NR10
    ld a, $80
    inc c
    ld [$FF00+c], a ;NR11
    xor a
    inc c
    ld [$FF00+c], a ;NR12
    inc c
    ld [$FF00+c], a ;NR13
    ld a, $80
    inc c
    ld [$FF00+c], a ;NR14
    ld c, $16
    ld a, $80
    ld [$FF00+c], a ;NR21
    xor a
    inc c
    ld [$FF00+c], a ;NR22
    inc c
    ld [$FF00+c], a ;NR23
    ld a, $80
    inc c
    ld [$FF00+c], a ;NR24
    ld c, $1A
    xor a
    ld [$FF00+c], a ;NR30
    inc c
    ld [$FF00+c], a ;NR31
    inc c
    ld [$FF00+c], a ;NR32
    inc c
    ld [$FF00+c], a ;NR33
    ld a, $80
    inc c
    ld [$FF00+c], a ;NR34
    xor a
    ld hl, _AUD3WAVERAM
    ld b, $10
    .EraseWavRAM:
        ld [hl+], a
        dec b
    jr nz, .EraseWavRAM

    ld c, $20
    ld [$FF00+c], a ;NR41
    inc c
    ld [$FF00+c], a ;NR42
    inc c
    ld [$FF00+c], a ;NR43
    ld a, $80
    inc c
    ld [$FF00+c], a ;NR44
    ld a, $77
    ldh [rNR50], a
    ret

    ; $44EF
snd_Init::
    ; Purpose:
    ;   This function needs to be called once, during the initialization phase of
    ;   your application. It will setup all internal structures and variables of
    ;   MusyX. You must call this function to make sound.
    ;
    ; Inputs:
    ;   A   Bit 0: Set when running on Game Boy Color. Clear if not.
    ;       Bit 7: Set when the flash ROM is for Game Boy Color
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   There are two distinctions you need to make before calling this routine.
    ;   • Are you currently running on Game Boy Color?
    ;   • Is the flash ROM enabling Game Boy Color?
    ;   
    ;   For instance, you could be writing a program that does not require the
    ;   Game Boy Color features, at all. In this case you would not have the
    ;   CGB compatibility flag set in the ROM registration area, and Game Boy
    ;   Color would behave like a conventional Game Boy. So all Game Boy
    ;   Color features, such as double speed mode, do not function. But still this
    ;   flash ROM can be plugged into Game Boy Color. This needs to be
    ;   communicated to MusyX.
    ;   
    ;   So:
    ;   A = $81     Running a Game Boy Color game on Game Boy Color
    ;   A = $80     Running a Game Boy Color game on Game Boy
    ;                   (not permissable)
    ;   A = $01     Running a conventional game on Game Boy Color
    ;   A = $00     Running a conventional game on Game Boy
    ;
    ; Undocumented input c:
    ;   c contains the lower nibble of an HRAM address that will store a copy
    ;       of the currently selected bank. e.g. c = $FE would store the bank value
    ;       into $FFFE every time _snd_LoadBank is called
    ;
    ; In the case of Magi-Nation, c is uninitialized and happens to be 0, meaning
    ;   that $FF00, the joypad register is sent gibberish
        push af
        ld hl, wSnd_WRAM
        ld de, wSnd_WRAM_End-wSnd_WRAM ; = $00FF
    .EraseDF00:
        ld a, d
        or e
        jr z, .FinishedLoop
        xor a
        ld [hl+], a
        dec de
        jr .EraseDF00 ;Sets $DF00 - $DFFE to 0
    .FinishedLoop:
        ld a, c
        ld [wSnd_CurrentROMBank_HRAMAddress], a
        call _snd_Init_Registers
        ld a, $FF
        ld [wSnd_NR51Panning_Mirror], a
        pop af
        ld [wSnd_GameboyModel], a
        ld a, BANK(@)
        ld [wSnd_BaseBank], a
        ld a, $DE
        ld [wSnd_RandomSeed], a
        ld a, $FF
        ld [wSnd_SFXSongVolume], a
        ld a, $F5
        ld [wSnd_Initialized], a
        ret

    ; $4524
_snd_CGB_UnknownSoundLength:
    ; Run exclusively on CGB roms
    ; Once per second, for each voice,
    ;   resets sound length to 0.25 seconds, only if wSnd_Voice_FrequencyStatus bit 6 is nz
    ; I'm not sure what this actually achieves
    ld de, $0002
    ld hl, wSnd_Voice_SoundLengthIndex+2

    .LoopVoiceDec:
        ld a, [hl]
        and a
        jr z, .CheckSetSoundLengthToZero
            dec [hl]
    .PrepareNextLoopVoiceDec:
        dec hl
        dec e
        bit 7, e
        jr z, .LoopVoiceDec
        ret


    .CheckSetSoundLengthToZero:
        push hl
        ld hl, wSnd_Voice_FrequencyStatus
        add hl, de
        bit 6, [hl]
        jr nz, .SetSoundLengthToZero
            pop hl
            jr .PrepareNextLoopVoiceDec

    .SetSoundLengthToZero:
        res 6, [hl]

        ld hl, .Registers_NR01SoundLengthTable
        add hl, de
        ld c, [hl]
        ld b, $C0 ;Bitmask to keep the wave pattern duty
        ld a, e
        cp $02
        jr nz, .NotVoice2
            ld b, $00 ;Voice 2 does not have any wave pattern duty
        .NotVoice2:
        ld a, [$FF00+c]
        and b
        ld [$FF00+c], a ;Set sound length to 0 (i.e. last 0.25 seconds)

        pop hl
        ld [hl], $3C ;Reset wSnd_Voice_SoundLengthIndex to 60 i.e. 1 second
        jr .PrepareNextLoopVoiceDec

    ; $455A
    .Registers_NR01SoundLengthTable:
        ; NR11, NR21, NR31
        db $11, $16, $1B

    ; $455D
_snd_MacroOp_00_END_23_STOP:
    ; END
    ;   End of the Macro
    ; Type: Structure
    ; Description:
    ;   This is always the last macro command. It can not be deleted from the
    ;   macro. It terminates the macro permanently.
    ;
    ;   db $00
    ;
    ; STOP
    ;   Similar to end
    ; Type: Structure
    ; Description:
    ;   This macro command serves the same function as END, but in contrast
    ;   to END it can be placed anywhere in the macro.
    ;
    ;   db $23
    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_Priority
    add hl, de
    xor a
    ld [hl], a
    ld hl, wSnd_Voice_UniqueID
    add hl, de
    ld [hl], a
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld [hl+], a
    ld [hl], a
    ret

    ; $4574
_snd_MacroOp_0E_SETVOICE:
    ;   Selects a voice for this macro
    ; Type: Voice/Sample
    ; Description:
    ;   Selects a new voice channel for the current macro. This command
    ;   overrides any selections by the MIDI channel.
    ;   This command *must* be the first one in the macro.
    ; Parameters:
    ;   Voice = Specifies the voice number to use (0-3). A value of 255 keeps
    ;       the voice chosen by the MIDI sequence
    ;   Rst.Flag = If this flag is set to ON the voice selected will not be reset. This is
    ;       useful to take control over the voice in its present state.
    ;   Toggle Flag = If this flag is set to ON and the voice selected by the MIDI 
    ;       sequencer is voice 1 or 2, the voice really used to play the sound
    ;       will toggle between voice 1 and 2 on every key played.
    ;       This is very effective to create echos.
    ;
    ;   db $0E
    ;   db SWITCH Voice,DontResetBool,ToggleBool:
    ;       case 0-3,_,_    Voice | (DontResetBool*$80) | (ToggleBool*$10)
    ;       ?case $FF,_,_    $FF (not sure, this might not happen)
    ;       case _,_,0      $FF
    ;       case _,_,1      (Voice & 3)| (DontResetBool*$80) | (ToggleBool*$10)
    ;
    ; This just skips this command. This command is actually handled in _snd_TryChooseVoice_andAssignSoundMacro
    inc bc
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b
    ret

    ; $457C
_snd_MacroOp_04_WAIT:
    ;   Suspends macro execution for some time
    ; Type: Structure
    ; Description:
    ;   The execution of the current macro will be suspended until the specified
    ;   time has elapsed or a keyoff occurs.
    ; Parameters:
    ;   key release = If this flag is set to ON, the macro will resume when it receives
    ;       a keyoff regardless of the specified wait time
    ;       random = If this flag is set, the macro will resume after a random time has
    ;       elapsed. In this case the ticks/millisec. parameter defines the
    ;       maximum wait time
    ;   ms = Specifies, in milliseconds, the time to delay macro execution
    ;       A value of 65535 will cause the wait to be endless
    ;
    ;   db $04
    ;   db KeyOff
    ;   db RandomTimeBool
    ;   dw IF Milliseconds != $FFFF
    ;           cycles(Milliseconds)+1
    ;      IF RandomTimeBool and Milliseconds == $FFFF
    ;           $0001 ;Looks like infinite wait is not supported if RandomTimeBool is on?
    ;      IF not(RandomTimeBool) and Milliseconds == $FFFF
    ;           $0000
        ld a, [wSnd_SelectedVoice_Index]
        ld d, $00
        ld e, a
        sla e
        ld hl, wSnd_Voice_WaitIndex
        add hl, de
        ld a, [hl+]
        or [hl]
        jr nz, .FinishedStoringTargetKeyCents ;Index is non-zero, so this is not the first cycle
            push bc ;Index is 0, so it's the first cycle, so setup the wait time
            inc bc
            ld a, [bc]
            inc bc
            and a
            jr z, .SkipRand ;Check RandomTimeBool
                push hl ;Get a random wait time
                ld a, [bc]
                ld h, a
                inc bc
                ld a, [bc]
                ld l, a
                call _snd_rnd_h00
                inc hl ;At least 1
                ld b, l
                ld a, h
                pop hl
                ld [hl-], a ;Save the random wait time
                ld [hl], b
                pop bc
                jr .FinishedStoringTargetKeyCents
            .SkipRand:
                ld a, [bc] ;Save the fixed wait time
                ld [hl-], a
                inc bc
                ld a, [bc]
                ld [hl], a
                pop bc
    .FinishedStoringTargetKeyCents:
        ld a, [bc]
        and a
        jr z, .SkipKeyOff ;No KeyOff

            ld hl, wSnd_Voice_VolumeStatus
            srl e
            add hl, de
            sla e
            bit 7, [hl] ;Check to see if you escape
            jr z, .SkipKeyOff

                ld hl, wSnd_Voice_WaitIndex+1
                add hl, de
                xor a
                ld [hl-], a
                ld [hl], a ;Escaping, so set wSnd_Voice_WaitIndex to 0

            .EscapeWaitLoop:
                ld hl, wSnd_SelectedVoice_MacroReadAddress
                ld a, [hl]
                add $04
                ld [hl+], a
                ld a, [hl]
                adc $00
                ld [hl], a ;Update the wSnd_SelectedVoice_MacroReadAddress so that you read the next instruction
                ret

    .SkipKeyOff:
        ld hl, wSnd_Voice_WaitIndex
        add hl, de
        ld a, [hl+]
        ld e, a
        ld d, [hl]
        dec de
        ld a, d
        ld [hl-], a
        ld [hl], e ;Decrement wSnd_Voice_WaitIndex
        or e
        jr nz, .NotYetEscape ;Check if wSnd_Voice_WaitIndex is 0
            ld hl, $0003
            add hl, bc
            ld a, [hl-]
            or [hl]
            jr nz, .EscapeWaitLoop ;Escape, unless the original wait time was 0 (i.e. infinite loop)
    .NotYetEscape:
        dec bc
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        ld a, c
        ld [hl+], a
        ld [hl], b ;wSnd_SelectedVoice_MacroReadAddress resets to run WAIT again
        ret

    ; $45EC
_snd_MacroOp_05_LOOP:
    ;   Loops back to a macrostep
    ; Type: Structure
    ; Description:
    ;   Loops to the specified location within the current macro n-times.
    ; Parameters:
    ;   step = The step number inside the current macro to loop back to
    ;   times = Number of times to loop back (0=infinite)
    ;
    ;   db $05
    ;   db times
    ;   dw relative(step)
    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_LoopIndex
    add hl, de
    ld a, [bc]
    and a
    jr z, .SetupLoop ; 0 = infinite loop
    cp [hl]
    jr nz, .SetupLoop
        ld [hl], $00 ;Loop end. Set wSnd_Voice_LoopIndex to 0 and move forward to next command
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        inc bc
        inc bc
        inc bc
        ld a, c
        ld [hl+], a
        ld [hl], b
        ret
    .SetupLoop:
        inc [hl] ;Increment loop index and then jump relative from end of command to loop
        inc bc
        ld l, c
        ld h, b
        ld a, [hl+]
        ld d, a
        ld a, [hl+]
        ld e, a
        add hl, de
        ld a, l
        ld d, h
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        ld [hl+], a
        ld [hl], d
        ret

    ; $461A
_snd_MacroOp_06_GOTO:
    ;   Jumps to another macro
    ; Type: Structure
    ; Description:
    ;   Performs an unconditional jump to the specified location.
    ; Parameters:
    ;   macro = The ID of the macro to jump into
    ;   step = The step number inside the target macro to jump to
    ;
    ;   db $06
    ;   dw address(SoundMacroID,SoundMacroStep)
    ld l, c
    ld h, b
    ld a, [hl+]
    ld e, [hl]
    ld d, a
    ld hl, sdp_SoundMacroLookupTable
    add hl, de
    ld a, l
    ld d, h
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld [hl+], a
    ld [hl], d
    ret

    ; $462B
_snd_MacroOp_0F_SETADSR:
    ;   Uses a software ADSR envelope on the current voice
    ; Type: Volume/Panning
    ; Description:
    ;   The data from the specified ADSR table will be used to perform an
    ;   ADSR envelope on the current voice.
    ; Parameters:
    ;   Table = The ID of the ADSR table to use
    ld hl, sdp_ADSRTableAddress
    ld e, l
    ld d, h
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    add hl, de ;hl+snd_ProjectData
    push hl
    ld a, [bc]
    ld e, a
    ld d, $00
    sla e
    rl d
    add hl, de
    ld a, [hl+]
    ld d, [hl] ;Relative offset
    ld e, a
    pop hl
    add hl, de ;Pointer to ADSR data
    ld a, [wSnd_SelectedVoice_Index]
    ld c, a
    call _snd_ADSR_LoadData
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    inc [hl]
    ret nz
    inc hl
    inc [hl]
    ret


    ; $4652
_snd_MacroOp_09_SETVOLUME:
    ;   Sets an absolute volume
    ; Type: Volume/Panning
    ; Description:
    ;   Sets a fixed volume on the current channel.
    ; Parameters:
    ;   volume = Specifies an absolute volume for the current channel (0-127).
    ;
    ;   db $09
    ;   db IF Volume == 0
    ;           0
    ;      ELSE
    ;           max(1,Volume//8)
    ld a, [bc]
    ld e, a
    inc bc
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b

    ; $465B
_snd_SetSelectedVoiceToVolumeE:
    ld a, [wSnd_SelectedVoice_Index]
    ld c, a
    ld b, $00
    ld hl, wSnd_Voice_FrequencyStatus
    add hl, bc
    bit 1, [hl]
    ld a, [wSnd_SFXSongVolume]
    jr z, .DontSwapCauseSong
        swap a
    .DontSwapCauseSong:
    and $0F
    call _snd_CalculateOutputVolume
    ld hl, wSnd_Voice_Velocity
    add hl, bc
    ld [hl], a
    ld hl, wSnd_Voice_VolumeStatus
    add hl, bc
    set 5, [hl]
    ret

    ; $467F
_snd_MacroOp_28_CURRENTVOL:
    ;   Fakes the internal volume
    ; Type: Special
    ; Description:
    ;   This command is used to change the internal volume in the sound
    ;   system, only. The real voice volume remains unaffected. This is
    ;   necessary after the HARDENVELOPE command has been used which,
    ;   due to hardware limitations, leaves the sound system uninformed about
    ;   the real hardware volume.
    ;   Use this command in conjunction with carefully timed macros, to tell the
    ;   soundsystem your idea of the current hardware volume (usually after a
    ;   full fade-in or fade-out).
    ; Parameters:
    ;   volume = MIDI volume (0~127) to set as a fake value in the sound system
    ;
    ;   db $28
    ;   db IF volume == 0
    ;           0
    ;      ELSE
    ;           max(1,volume//8)
    ;
    ;? It seems to reset the true volume. Untested.
    call _snd_MacroOp_09_SETVOLUME
    ld hl, wSnd_Voice_EnvelopeVolume
    add hl, bc
    ld a, [hl]
    call _snd_CalculateOutputVolume
    ld hl, wSnd_Voice_InternalVolume
    add hl, bc
    ld [hl], a
    ret

    ; $4690
_snd_SetPanningToA:
    ; Sets the panning of the current voice to the value indicated by a
    push af
    ld hl, sp+$01 ;Sneaky: Push a into the stack and make [bc] point to a as a fake MacroReadAddress
    ld c, l
    ld b, h
    call _snd_MacroOp_0A_PANNING
    pop af
    ret

    ; $469A
_snd_MacroOp_0A_PANNING:
    ;   Sets the panning to be used with the macro
    ; Type: Volume/Panning
    ; Description:
    ;   Sets the position for the current voice channel. Game Boy hardware only
    ;   allows for absolute left, absolute right and center positions.
    ; Parameters:
    ;   pan.pos = 0-41 designates left output, 42-83 center and 84-127 right
    ;
    ;   db $0A
    ;   db switch Panning:
    ;       case 0-$29      0      (represents 0)
    ;       case $2A-$54    1      (represents 64)
    ;       case $54-$FF    2      (represents 127)
    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld d, $00
    ld hl, .NR51Panning_OffBitmap
    add hl, de
    ld a, [hl]
    ld d, a
    ld hl, wSnd_NR51Panning_Mirror
    ld a, [hl]
    and d
    ld [hl], a ;First, turn off the left/right of the target channel
    push hl

    ld a, [bc]
    ld c, a
    ld b, d
    ld a, e
    add a
    add e
    add c
    ld e, a
    ld d, $00
    ld hl, .NR51Panning_OnBitmap
    add hl, de
    ld a, [hl]
    ld c, a  ;Load .NR51Panning_OnBitmap + 3*track + [bc]

    pop hl
    or [hl]
    ld [hl], a ;Apply the bitmap to wSnd_NR51Panning_Mirror

    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld hl, wSnd_Voice_FrequencyStatus
    add hl, de
    bit 7, [hl]
    jr z, .SkipWriteToNR51 ;Jump if voice is off
        ldh a, [rNR51]
        and b
        or c
        ldh [rNR51], a ;Only update NR51 if voice is turned on
    .SkipWriteToNR51:
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    inc [hl]
    ret nz
    inc hl
    inc [hl]
    ret

    ; $46DA
    .NR51Panning_OffBitmap:
    ; Bitmap to disable the panning left/right of channels 0-3
    db $EE, $DD, $BB, $77

    ; $46DE
    .NR51Panning_OnBitmap:
    ; Bitmap to turn on specific channels
    ; Row: Track 0-3
    ; Column: 0 = Left, 1 = Center, 2 = Right
    db $10, $11, $01
    db $20, $22, $02
    db $40, $44, $04
    db $80, $88, $08

    ; $46EA
_snd_MacroOp_0C_VOICE_ON_WAVE_ON:
    ; VOICE_ON
    ;   Starts sound
    ; Type: Voice/Sample
    ; Description:
    ;   This command starts the sound after all initial setups (if the initialization
    ;   phase was not overridden by a SET_VOICE command). The DutyCycle
    ;   parameter is used for the rectangular wave oscillator (voice 1/2), with
    ;   values from 0-3 representing 12.5, 25, 50 and 75 percent pulse-width. A
    ;   duty-cycle of 255 can be used to have the velocity influence the pulse-
    ;   width.
    ;   For voice 3 use the WAVE_ON command.
    ;   For voice 4 the parameter is unimportant.
    ; Parameters:
    ;   DutyCycle = Specify the duty cycle to use (see above). Enter 255 to have the
    ;       velocity modify the duty cycle (0-31=12.5%, 32-63=25%,
    ;       64-95=50%, 96-127=75%)
    ;
    ;   db $0C
    ;   db DutyCycle if (DutyCycle <= 3 or DutyCycle == $FF) else $00
    ;
    ; WAVE_ON
    ;   Loads a looping wave into voice 3 and starts it
    ; Type: Voice/Sample
    ; Description:
    ;   This command loads a short, 32 sample long looping waveform into the
    ;   WaveRAM and starts voice 3 after all initial setups (if the initialization
    ;   phase was not overridden by a SET_VOICE command).
    ; Parameters:
    ;   SampleID = ID of the sample to load into Wave RAM. If the ID is 0 no new
    ;   sample will be loaded and the contents of the Wave RAM remain
    ;   unchanged.
    ;
    ;   db $0C
    ;   db id(SampleID) if SampleID != 0 else $FF
    ld a, [bc]
    ld e, a
    inc bc
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b

    ld a, [wSnd_SelectedVoice_Index]
    cp $02
    ld c, a
    ld a, e
    jp nc, _snd_VoiceOn ;Voice 0-2
    cp $FF
    jp nz, _snd_VoiceOn

    ld b, $00 ;Voice 3
    ld hl, wSnd_Voice_Velocity
    add hl, bc
    ld a, [hl]
    srl a
    srl a
    jp _snd_VoiceOn


    ; $4710
_snd_MacroOp_0D_VOICE_OFF:
    ;   Stops sound
    ; Type: Voice/Sample
    ; Description:
    ;   Stops the sound on the current voice.
    ;
    ;   db $0D
    ld a, [wSnd_SelectedVoice_Index]
    ld c, a
    jp _snd_VoiceOff

    ; $4717
_snd_MacroOp_0B_RNDNOTE:
    ;   Creates a random pitch
    ; Type: Pitch
    ; Description:
    ;   Sets random pitch. Note lo is the lower end of the range, note hi the
    ;   upper end. The detune value will be added after the random pitch is
    ;   calculated and is specified in positive cents. If the free flag is set, the
    ;   pitch will be generated freely inside the range. Otherwise, a random key
    ;   from this range will be generated. If the abs flag is set, the specified
    ;   range is absolute. Otherwise it is relative to the current key.
    ; Parameters:
    ;   note-lo = Specifies the low key for the random range
    ;   detune = Specifies a fraction of a key (0-99) to be added to the random
    ;       result in the end
    ;   note-hi = Specifies the top key for the random range
    ;   fix/free = If OFF, a random key will be generated inside the specified 
    ;       range. If ON a random pitch inside the permissable range is 
    ;       generated without respect to any keys.
    ;   rel/abs = If OFF, the range is relative to the current key. If ON the range is
    ;       fixed by the specified keys
    ;
    ;   IF note-lo == NoteHigh and NoteHigh == 127
    ;       note-lo -= 1
    ;   ELSEIF note-lo == NoteHigh and NoteHigh != 127
    ;       note-hi += 1
    ;   db $0B
    ;   db (fix/free << 7) + rel/abs
    ;   db note-lo
    ;   db note-hi
    ;   db keycents(0,detune) ;Lower 8 bits
    ;
    ;   Note: abs seems to possibly be bugged. To be tested...
        ld a, [wSnd_SelectedVoice_Index]
        cp $03
        jp z, .End ;invalid for voice 3

        ld e, a
        ld d, $00
        ld hl, wSnd_Voice_FrequencyStatus
        ;BUG - there should be add hl, de here to select the correct voice to set
        set 5, [hl]

        sla e
        ld l, c
        ld h, b
        ld a, [hl+]
        bit 7, a
        jr nz, .Free ;fix/free = 1
        and a
        jr nz, .FixAbs ;fix/free = 0, rel/abs = 1
            ld a, [hl+] ;fix/free = 0, rel/abs = 0
            ld c, a
            ld a, [hl+]
            sub c
            ld b, a
            call _snd_rnd_b
            add c
            ld c, [hl]
            ld hl, wSnd_Voice_KeyCents+1
            add hl, de
            ld [hl-], a
            ld [hl], c ;Key = rnd(Hi-Low) + Low ; Cents = Detune
            jp .End
        .FixAbs:
            ld a, [hl+] ;fix/free = 0, rel/abs = 1
            ld c, a
            ld a, [hl+]
            ld b, a
            push hl

            ld hl, wSnd_Voice_KeyCents+1
            add hl, de
            ld a, [hl]
            add c
            bit 7, a
            jr z, .NoUnderflow
                xor a ;NoteLow overflows past 127 so set it to 0
            .NoUnderflow:
            ld c, a ;NoteLow + Voice_Key

            ld a, b
            add [hl] ;NoteHigh + VoiceKey
            sub c
            ld b, a
            call _snd_rnd_b 
            ;Is there a bug here? Shouldn't it be rnd(Hi-Low) + Low - OriginalKey or something similar?
            sub [hl] ; rnd(Hi-Low) - OriginalKey
            pop hl
            ld c, [hl]
            ld hl, wSnd_Voice_AddNote_DeltaKeyCents+1
            add hl, de
            ld [hl-], a
            ld [hl], c ;Detune
            jr .End

    .Free:
        and $7F
        jr nz, .FreeAbs ;fix/free = 1, rel/abs = 1

            ld a, [hl+]  ;fix/free = 1, rel/abs = 0
            cpl
            inc a
            ld b, a
            ld c, $00
            ld a, [hl+]
            ld l, [hl]
            ld h, a
            add hl, bc ; NoteHigh + Cents - NoteLow
            call _snd_rnd_h00 ;n.b. bugged random function
            ld a, b
            cpl
            inc a
            ld b, a ;NoteLow
            add hl, bc ; rnd(NoteHigh+Cents-NoteLow)+NoteLow
            ld a, l
            ld b, h
            ld hl, wSnd_Voice_KeyCents
            add hl, de
            ld [hl+], a
            ld [hl], b
            jr .End

        .FreeAbs:
            ld a, [hl+]
            ld b, a ;NoteLow
            ld c, $00
            push hl
            ld hl, wSnd_Voice_KeyCents
            add hl, de
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            add hl, bc
            ld c, l
            ld b, h  ;Voice_KeyCents + NoteLow

            pop hl
            push bc
            ld a, [hl+]
            ld c, [hl]
            ld b, a
            ld hl, wSnd_Voice_KeyCents
            add hl, de
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            add hl, bc ;Voice_KeyCents + NoteHigh

            pop de ;Voice_KeyCents + NoteLow
            ld c, e
            ld b, d
            ld a, e
            cpl
            add $01
            ld e, a
            ld a, d
            cpl
            adc $00
            ld d, a ;-(Voice_KeyCents + NoteLow)

            add hl, de
            call _snd_rnd_h00 ;rnd(NoteHigh-NoteLow)
            add hl, bc ; rnd(NoteHigh-NoteLow) + Voice_KeyCents + NoteLow
            ld c, l
            ld b, h

            ld a, [wSnd_SelectedVoice_Index]
            sla a
            ld e, a
            ld d, $00
            ld hl, wSnd_Voice_KeyCents
            add hl, de
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            ld a, l
            cpl
            add $01
            ld l, a
            ld a, h
            cpl
            adc $00
            ld h, a ;-Voice_KeyCents

            add hl, bc
            ld a, l
            ld b, h
            ld hl, wSnd_Voice_AddNote_DeltaKeyCents
            add hl, de
            ld [hl+], a
            ld [hl], b ;Delta = rnd(NoteHigh-NoteLow) + NoteLow

    .End:
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        ld a, [hl]
        add $04
        ld [hl+], a
        ld a, [hl]
        adc $00
        ld [hl], a
        ret

    ; $47E7
_snd_MacroOp_08_ADDNOTE:
    ;   Modifies the current key by offset values
    ; Type: Pitch
    ; Description:
    ;   Transposes the current key by a number of keys and a cent fraction.
    ;   The result is temporary when the org.key flag is set, so that further
    ;   ADDNOTE commands will again take the MIDI key as base. If the flag is
    ;   OFF, the result of this command will be considered to be the new base.
    ; Parameters:
    ;   add = Value (-127 – 127) to transpose the current key by
    ;   detune = Key fraction (-99 – 99) to transpose the current key by
    ;   org.key = If set to OFF the result will form a new base key. If set to ON the 
    ;       result is temporary until the next ADDNOTE command.
    ;
    ;   db $08
    ;   db not(org.key)
    ;   db Add
    ;   db keycents(0,Detune) ;Lower 8 bits
        ld a, [wSnd_SelectedVoice_Index]
        cp $03
        jr z, .End ;Voice 4 invalid

        ld e, a
        ld d, $00
        ld hl, wSnd_Voice_FrequencyStatus
        add hl, de
        set 5, [hl] ;Update frequency

        ld l, c
        ld h, b
        ld a, [hl+]
        and a
        ld a, [hl+]
        ld c, [hl]
        ld b, a

        jr nz, .SetNewBaseKey ;org.key = off
            sla e ;;org.key = on
            ld hl, wSnd_Voice_AddNote_DeltaKeyCents
            add hl, de
            ld a, c
            ld [hl+], a
            ld [hl], b
            jr .End

    .SetNewBaseKey:
        sla e
        ld hl, wSnd_Voice_AddNote_DeltaKeyCents
        add hl, de
        xor a
        ld [hl+], a
        ld [hl], a ;Delta is 0

        ld hl, wSnd_Voice_KeyCents
        add hl, de
        ld a, [hl]
        add c
        ld [hl+], a
        ld a, [hl]
        adc b
        ld [hl], a
        bit 7, a
        jr z, .NoUnderflow
            add $54
            ld [hl], a
            jr .End
    .NoUnderflow:
        cp $54
        jr c, .End
        sub $54 ;Handle overflow
        ld [hl], a

    .End:
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        ld a, [hl]
        add $03
        ld [hl+], a
        ld a, [hl]
        adc $00
        ld [hl], a
        ret

    ; $483A
_snd_MacroOp_07_SETNOTE:
    ;   Sets pitch to a fixed note
    ; Type: Pitch
    ; Description:
    ;   Sets the pitch to a fixed note and detune in cents.
    ; Parameters:
    ;   key = The key number to set the pitch to (0-127)
    ;   detune = The fraction of a key (-99 – 99) to add to the pitch
    ;
    ;   db $07
    ;   db Key
    ;   db keycents(0,Detune) ;Lower 8 bits
    ld a, [bc]
    inc bc
    ld d, a
    ld a, [bc]
    inc bc
    ld e, a
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b

    ; $4846
_snd_SetNote:
    ; Sets current voice's key to KeyCents specified by register de
    ld a, [wSnd_SelectedVoice_Index]
    cp $03
    ret z ;Cannot set key for voice 3

    ld c, a
    ld b, $00
    ld hl, wSnd_Voice_FrequencyStatus
    add hl, bc
    set 5, [hl]
    sla c
    ld hl, wSnd_Voice_KeyCents
    add hl, bc
    ld a, e
    sub [hl]
    inc hl
    ld e, a
    ld a, d
    sbc [hl]
    ld hl, wSnd_Voice_AddNote_DeltaKeyCents+1
    add hl, bc
    ld [hl-], a
    ld [hl], e
    ret

    ; $4868
_snd_MacroOp_02_PORTAMENTO:
    ;   Starts a portamento
    ; Type: Pitch
    ; Description:
    ;   Slides the pitch from the current pitch to a target pitch specified in the
    ;   given time.
    ; Parameters:
    ;   Keys = Transpose value for the current key to yield the target key of the
    ;       portamento (-127 ~ 127)
    ;   Cents = Transpose value in fractions of a key to yield the target key of
    ;       the portamento (-99 ~ 99)
    ;   Flag = If OFF, the key and cents specified form the target of the
    ;       portamento. If ON, the key and cents are added to the current
    ;       key (relative mode) to yield the target of the portamento.
    ;   ms = A time in milliseconds for the portamento to be finished
    ;
    ;   db $02
    ;   dw keycents(Key,Cents)
    ;   dw cycles(max(Milliseconds,1))
    ;   db RelBool
    ld l, c
    ld h, b
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    ld a, [hl+]
    ld d, a
    ld a, [hl+]
    ld e, a
    ld a, [wSnd_SelectedVoice_Index]
    cp $03
    jr nc, .End ;Invalid voice
        or [hl] ;bug? RelBool should be placed in bit 7, but it's actually located at bit 0
        call _snd_SetupPortamento
    .End:
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, [hl]
    add $05
    ld [hl+], a
    ld a, [hl]
    adc $00
    ld [hl], a
    ret

    ; $4889
_snd_MacroOp_01_VIBRATO:
    ;   Starts a vibrato effect
    ; Type: Pitch
    ; Description:
    ;   Adds a vibrato effect to the current voice. The intensity of the vibrato is
    ;   specified by a displacement of the current key by a number of keys and
    ;   cents. When the keys parameter is negative, the effect will start to
    ;   decrease in frequency. Otherwise, the frequency will first increase.
    ; Parameters:
    ;   Keys = Intensity of the vibrato in keys relative to the current key
    ;   Cents = Intesity of the vibrato in fractions of a key relative to the current
    ;       key. This value is added to the Keys parameter.
    ;   ms = Time in milliseconds for a full frequency cycle to complete.
    ;
    ;   x = min(cycles(max(Milliseconds//2,1)),$FF)
    ;   y = keycents(Key,Cents)
    ;   db $01
    ;   dw y//x
    ;   db x
    ld l, c
    ld h, b
    ld a, [hl+]
    ld d, a
    ld a, [hl+]
    ld e, a
    ld a, [hl+]
    ld b, a
    ld a, l
    ld [wSnd_SelectedVoice_MacroReadAddress], a
    ld a, h
    ld [wSnd_SelectedVoice_MacroReadAddress+1], a
    ld a, [wSnd_SelectedVoice_Index]
    ld c, a
    cp $03
    jp c, _snd_SetupVibrato
    ret

    ; $48A3
_snd_MacroOp_03_PITCHSWEEP:
    ;   Adds a sweep effect to the pitch
    ; Type: Pitch
    ; Description:
    ;   Adds a sliding effect to the current pitch. After reaching the limit, the pitch
    ;   wraps back and the slide starts again. There are 2 independent
    ;   modulators that can be selected by the select parameter. If the Limit is
    ;   negative the sweep goes downwards, otherwise upwards.
    ; Parameters:
    ;   Limit = Specifies the number of keys to slide up or down relative to the
    ;       current key.
    ;   Limit fine = Specifies the fraction of a key in addition to the full keys to slide
    ;       up or down to.
    ;   Select = If 0 selects sweep effect 1, if 1 selects sweep effect 2. Two
    ;       independent sweep effects which may work against each other
    ;       can be started.
    ;   ms = Time for one sweep cycle to complete in milliseconds
    ;
    ;   x = min(cycles(max(Milliseconds,1)),$FF)
    ;   y = keycents(NoteLimit,CentLimit)
    ;   db $03
    ;   dw y//x
    ;   dw y
    ;   db SweepBool*$80
    ld l, c
    ld h, b
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    ld a, [hl+]
    ld d, a
    ld a, [hl+]
    ld e, a
    ld a, [wSnd_SelectedVoice_Index]
    cp $03
    jr nc, .InvalidVoice
        or [hl]
        call _snd_SetupPitchsweep
    .InvalidVoice:
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, [hl]
    add $05
    ld [hl+], a
    ld a, [hl]
    adc $00
    ld [hl], a
    ret

    ; $48C4
_snd_MacroOp_10_SETNOISE:
    ;   Sets parameters for the noise generator
    ; Type: Voice/Sample
    ; Description:
    ;   Sets up the polynomial clock counter for the white noise generator (voice
    ;   4).
    ; Parameters:
    ;   Clock = Values from 0-13 select the ratio of frequencies
    ;   Step = If 0 selects 15 steps for the counter, 1 selects 7 steps
    ;   Freq. = Values from 0-7 select the frequency ratio
    ;
    ;   db $10
    ;   db Clock*%10000 + Step*%1000 + Freq
    ld a, [bc]
    inc bc
    ld e, a
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b
    ld a, e
    ldh [rNR43], a
    ret

    ; $48D1
_snd_MacroOp_12_KEYOFF:
    ;   Sends keyoff to voice
    ; Type: Control
    ; Description:
    ;   Sends a keyoff to the specified voice. Specify 255 to send a keyoff to the
    ;   current voice.
    ; Parameters:
    ;   Voice = Specifies the voice to send a keyoff to (0-3). Enter 255 to send a
    ;       keyoff to this voice.
    ;
    ;   db $12
    ;   db Voice if Voice <= 3 else $FF
    ld a, [bc]
    inc bc
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld [hl], c
    inc hl
    ld [hl], b
    cp $FF
    jr nz, _snd_KeyOff_TrackA

    ; $48DD
_snd_KeyOff_SelectedTrack:
    ld a, [wSnd_SelectedVoice_Index]

    ; $48E0
_snd_KeyOff_TrackA:
    sla a
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_TrapKeyOffAddress
    add hl, de
    ld a, [hl+]
    ld c, a
    or [hl]
    jr z, .NoKeyOff_JumpAddress
        ld a, [hl]
        ld hl, wSnd_SelectedVoice_MacroReadAddress+1
        ld [hl-], a
        ld [hl], c
        ld hl, wSnd_Voice_MacroReadAddress+1
        add hl, de
        ld [hl-], a
        ld [hl], c
        ld hl, wSnd_Voice_WaitIndex
        xor a
        ld [hl+], a
        ld [hl], a
    .NoKeyOff_JumpAddress:
        srl e
        ld hl, wSnd_Voice_VolumeStatus
        add hl, de
        ld a, [hl]
        or $80
        ld [hl], a ;Send keyoff to WAIT
        ld c, a

        and $17 ;Check for Envelope bits
        ret z
        bit 4, a ;Return if Envelope_Ascending
        ret nz

        bit 2, a
        jr z, .ADSRAttackOrDecay
            ;ADSR_Sustain
            ld a, b
            and $F0
            or $08 ;Bit 3 (ADSR_Release)
            ld [hl], a
            ret
        .ADSRAttackOrDecay:
            ;We will keep the currently saved volume and decay from there
            ld a, c
            and $F0
            or $08 ;Set bit 3 ADSR_Release
            ld [hl], a

            bit 0, c
            ld bc, $0001 ;From ADSR_Decay, offset the pointer by 1
            jr z, .FoundADSRPointerOffset
                ld bc, $0003 ;From ADSR_Attack, offset the pointer by 3
            .FoundADSRPointerOffset:
            ld hl, wSnd_Voice_Envelope_ADSRPointer
            add hl, de
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            add hl, bc
            ld a, [hl+]
            ld b, [hl]

            ld hl, wSnd_Voice_Envelope_DeltaVolumeCents
            add hl, de
            ld [hl+], a
            ld [hl], b ;Save Decay
            ret

    ; $493D
_snd_MacroOp_11_ENVELOPE_DESCENDING:
    ;   Starts a software envelope
    ; Type: Volume/Panning
    ; Description:
    ;   Starts a software envelope. The volume will be faded out/in to mute level
    ;   or full volume in the time specified. Due to Game Boy hardware
    ;   restrictions, this may be of lower quality than the hardware envelope.
    ; Parameters:
    ;   Flag = If OFF, fades out to mute level, if ON fades in to maximum level
    ;   ms = Time to fade out to zero in milliseconds
    ;
    ;   x = cycles(max(ms,1))
    ;   db $11 (flag is OFF)
    ;   dw max(1,$0F00//x)*(-1)
    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_VolumeStatus
    add hl, de
    ld a, [hl]
    and $F0
    or $28
    ld [hl], a

    ld hl, wSnd_Voice_EnvelopeVolume
    add hl, de
    ld [hl], $0F

    sla e
    ld hl, wSnd_Voice_Envelope_VolumeCents
    add hl, de
    xor a
    ld [hl+], a
    ld [hl], $0F

    ld l, c
    ld h, b
    ld a, [hl+]
    ld c, [hl]
    ld hl, wSnd_Voice_Envelope_DeltaVolumeCents+1
    add hl, de
    ld [hl-], a
    ld [hl], c

    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, [hl]
    add $02
    ld [hl+], a
    ld a, [hl]
    adc $00
    ld [hl], a
    ret

    ; $4973
_snd_MacroOp_13_HARDENVELOPE:
    ;   Starts hardware envelope
    ; Type: Volume/Panning
    ; Description:
    ;   Starts a hardware envelope. The volume will be faded out/in in the
    ;   specified time, which cannot be longer than 1640ms and is dependent on
    ;   the current volume. By employing the hardware feature for the envelope,
    ;   the sound might be slightly less distorted in comparison with the software
    ;   envelope. The software envelope, however, can span longer fading
    ;   times and also works on voice 3.
    ; Parameters:
    ;   Flag = If OFF, fades the voice down to mute level. If ON, fades the 
    ;       voice in from the current volume to maximum.
    ;   ms = Time in milliseconds (highly approximate!) for the fade to
    ;       complete. Due to hardware restrictions this value cannot be
    ;       larger than 1640 ms for a fade from maximum volume.
    ;
    ;   db $13
    ;   db Flag*0b1000 + max(1,min(7,floor(ms*0.0042666667141020298004150390625 + 0.5)))
    ld a, [bc]
    ld e, a
    inc bc
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b
    ld a, [wSnd_SelectedVoice_Index]
    cp $02
    ret z ;Cannot hardware envelope on Voice 2

    ld c, e
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_VolumeStatus
    add hl, de
    ld a, [hl]
    and $F0
    or $20
    ld [hl], a ;Ensure register's volume is updated during snd_Handle

    ld hl, wSnd_Voice_InternalVolume
    add hl, de
    ld a, [hl]
    swap a
    or c
    bit 3, a ;Envelope direction
    jr z, .SkipSetVelocity
        ld hl, wSnd_Voice_Velocity ;Do this if Ascending envelope
        add hl, de
        ld [hl], $00
    .SkipSetVelocity: ;Jump here if Descending envelope
    ld hl, .Registers_N02VolumeEnvelopeTable
    add hl, de
    ld c, [hl]
    ld [$FF00+c], a
    ret

    ; $49A9
    .Registers_N02VolumeEnvelopeTable:
    ; Points to NR12, NR22, null, NR42, which are the Channel X Volume Envelope registers
    db $12, $17, $00, $21

    ; $49AD
_snd_MacroOp_1D_PWN_START:
    ;   Starts the Pulse-Width-Modulation effect on voice 3
    ; Type: Voice/Sample
    ; Description:
    ;   Starts a software generated pulse-width effect on voice 3. The width of
    ;   the pulse will change over time (in a ping-pong-like fashion) between the
    ;   specified low and high limits.
    ; Parameters:
    ;   LimitLo = Low limit of the pulse width (0-15)
    ;   LimitHi = High limit of the pulse width (0-15)
    ;   ms = Time in milliseconds it takes to complete one pulse cycle
    ;
    ;   If LimitLo > LimitHi the values are swapped to fix the error
    ;   db $1E
    ;   db LimitLo
    ;   db LimitHi
    ;   dw 0 if cycles(ms) == 0, else ((LimitHi - LimitLo) << 8)//cycles(ms)
    ld l, c
    ld h, b
    ld a, [hl+]
    ld [wSnd_PWN_LimitLow], a
    ld [wSnd_PWN_CurrentDuty+1], a
    xor a
    ld [wSnd_PWN_CurrentDuty], a
    ld a, [hl+]
    ld [wSnd_PWN_LimitHigh], a
    ld a, [hl+]
    ld [wSnd_PWN_DutyDelta+1], a
    ld a, [hl+]
    ld [wSnd_PWN_DutyDelta], a
    ld a, l
    ld [wSnd_SelectedVoice_MacroReadAddress], a
    ld a, h
    ld [wSnd_SelectedVoice_MacroReadAddress+1], a
    ld a, $FF
    ld [wSnd_Sample_Bank], a
    ld [wSnd_Sample_BlockRepetitions], a
    call _snd_PWN_ClearLast7BytesOfSoundRAM
    ld a, $01
    ld [wSnd_Voice_SampleStatus], a
    call _snd_HandlePWN
    ld a, $FF
    ld c, $02
    jp _snd_VoiceOn

    ; $49E8
_snd_PWN_ClearLast7BytesOfSoundRAM:
    xor a
    ldh [rNR30], a
    ld hl, _AUD3WAVERAM + 9
    ld [hl+], a
    ld [hl+], a
    ld [hl+], a
    ld [hl+], a
    ld [hl+], a
    ld [hl+], a
    ld [hl], a
    ld a, $80
    ldh [rNR30], a
    ret

    ; $49FA
_snd_MacroOp_1E_PWN_UPDATE:
    ;   Updates the Pulse-Width-Modulation effect on voice 3
    ; Type: Voice/Sample
    ; Description:
    ;   This is basically the same as the PWM_START command. It modifies the
    ;   pulse width limits and the pulse cycle time, without restarting the effect.
    ; Parameters:
    ;   LimitLo = Low limit of the pulse width (0-15)
    ;   LimitHi = High limit of the pulse width (0-15)
    ;   ms = Time in milliseconds it takes to complete one pulse cycle
    ;
    ;   If LimitLo > LimitHi the values are swapped to fix the error
    ;   db $1E
    ;   db LimitLo
    ;   db LimitHi
    ;   dw 0 if cycles(ms) == 0, else ((LimitHi - LimitLo) << 8)//cycles(ms)
    ld a, [wSnd_Voice_SampleStatus]
    bit 0, a
    jr z, .NotInPWNMode
        ld l, c
        ld h, b
        ld a, [hl+]
        ld [wSnd_PWN_LimitLow], a
        ld a, [hl+]
        ld [wSnd_PWN_LimitHigh], a
        ld a, [hl+]
        ld [wSnd_PWN_DutyDelta+1], a
        ld a, [hl+]
        ld [wSnd_PWN_DutyDelta], a
        ld a, l
        ld [wSnd_SelectedVoice_MacroReadAddress], a
        ld a, h
        ld [wSnd_SelectedVoice_MacroReadAddress+1], a
        jp _snd_HandlePWN
    .NotInPWNMode:
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        ld a, [hl]
        add $04
        ld [hl+], a
        ld a, [hl]
        adc $00
        ld [hl], a
        ret

    ; $4A2A
_snd_MacroOp_24_PWN_FIXED:
    ;   Starts a generated pulse wave of fixed width
    ; Type: Voice/Sample
    ; Description:
    ;   Starts a fixed generated rectangular pulse wave on voice 3. The duty
    ;   parameter specifies the pulse-width from 0-15, which is equivalent to a
    ;   0-50% duty cycle.
    ; Parameters:
    ;   Duty = Width of the rectangular pulse (0-15)
    ;
    ;   db $24
    ;   db Duty
    ld a, [bc]
    inc bc
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld [hl], c
    inc hl
    ld [hl], b

    ; $4A32
_snd_PWN_FixedPulseA:
    ld [wSnd_PWN_CurrentDuty+1], a
    xor a
    ld [wSnd_PWN_CurrentDuty], a
    ld [wSnd_PWN_LimitLow], a
    ld [wSnd_PWN_DutyDelta+1], a
    ld [wSnd_PWN_DutyDelta], a
    ld a, $0F
    ld [wSnd_PWN_LimitHigh], a
    ld a, $FF
    ld [wSnd_Sample_Bank], a
    ld [wSnd_Sample_BlockRepetitions], a
    call _snd_PWN_ClearLast7BytesOfSoundRAM
    ld a, $01
    ld [wSnd_Voice_SampleStatus], a
    call _snd_HandlePWN
    ld a, $FF
    ld c, $02
    jp _snd_VoiceOn

    ; $4A61
_snd_MacroOp_25_PWN_VELOCITY:
    ;   Starts a generated pulse wave of velocity-dependent width
    ; Type: Voice/Sample
    ; Description:
    ;   Starts a fixed generated pulse wave on voice 3. The pulse-width is set
    ;   according to the current key velocity.
    ;
    ;   db $25
    ld a, [wSnd_Voice_Velocity+2]
    jr _snd_PWN_FixedPulseA

    ; $4A66
_snd_MacroOp_18_TRAP_KEYOFF:
    ;   Sets a trap on reception of a keyoff
    ; Type: Structure
    ; Description:
    ;   This command sets a so-called trap for a keyoff. This means that as
    ;   soon as the macro receives a keyoff by either the MIDI sequencer or the
    ;   KEYOFF command, the macro will jump to the macrostep where the trap
    ;   was set. As long as no keyoff is received the macro proceeds its
    ;   execution in normal fashion.
    ;   This command is used to escape an infinite loop or wait.
    ; Parameters:
    ;   macro = ID of the macro to jump to as soon as a keyoff is received
    ;   step = Step number in the macro to jump into as soon as a keyoff is
    ;   received
    ;
    ;   db $18
    ;   dw address(macro,step)
    ld l, c
    ld h, b
    ld a, [hl+]
    ld e, [hl]
    ld d, a
    ld hl, sdp_SoundMacroLookupTable
    add hl, de
    ld c, l
    ld b, h
    ld a, [wSnd_SelectedVoice_Index]
    sla a
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_TrapKeyOffAddress
    add hl, de
    ld a, c
    ld [hl+], a
    ld [hl], b
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, [hl]
    add $02
    ld [hl+], a
    ld a, [hl]
    adc $00
    ld [hl], a
    ret

    ; $4A8C
_snd_MacroOp_19_UNTRAP_KEYOFF:
    ; Removes a trap set for keyoff
    ;   Type: Structure
    ; Description:
    ;   Remove a previously set TRAP_KEYOFF.
    ;
    ;   db $19
    ld a, [wSnd_SelectedVoice_Index]
    sla a
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_TrapKeyOffAddress
    add hl, de
    xor a
    ld [hl+], a
    ld [hl], a
    ret

    ; $4A9C
_snd_MacroOp_1F_STOP_MOD:
;Stops any pitch modulation
    ; Type: Pitch
    ; Description:
    ;   Stops any pitch modulation on the current voice channel, but does not
    ;   reset the current values.
    ;   db $1F
    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld d, $00
    cp $02
    jr nz, .SkipVoice2
        ld a, [wSnd_Voice_SampleStatus]
        and $FE
        ld [wSnd_Voice_SampleStatus], a ;Disable PWN
    .SkipVoice2:
    ld hl, wSnd_Voice_FrequencyStatus
    add hl, de
    ld a, [hl]
    and $E2 ;Disable all effects
    or $20  ;Update frequency
    ld [hl], a
    ret

    ; $4AB9
_snd_MacroOp_20_RESET_MOD:
    ;   Reset all pitch modulations
    ; Description:
    ;   This command will stop and reset any active pitch modulation on the
    ;   current voice channel.
    ;
    ;   db $20
    call _snd_MacroOp_1F_STOP_MOD
    ld a, e
    cp $03 ;No modulation for voice 4
        ret z
    sla e
    xor a
    ld hl, wSnd_Voice_Modulation_DeltaKeyCents
    add hl, de
    ld [hl+], a
    ld [hl], a
    ret

    ; $4ACA
_snd_MacroOp_1A_SENDFLAG:
    ;   Raises a flag the application can evaluate
    ;   Type: Special
    ; Description:
    ;   Raises one of 8 user flags the game application can evaluate.
    ;   This feature is mainly used to signal certain events to the game program.
    ;   A raised flag remains raised until the application has read its status, at
    ;   which point the flag will be cleared again.
    ; Parameters:
    ;   num = Number of the flag to raise (0~7)
    ;
    ;   db $1A
    ;   db num & 7
    ld a, [bc]
    inc bc
    ld e, a
    ld d, $00
    ld hl, _snd_FlagLookupTable
    add hl, de
    ld a, [wSnd_Flags]
    or [hl]
    ld [wSnd_Flags], a
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b
    ret

    ; $4AE1
_snd_FlagLookupTable:
    db $01, $02, $04, $08, $10, $20, $40, $80

    ; $4AE9
snd_CheckFlag::
    ; Purpose:
    ;   Checks if a user flag, triggered by a sound, is set. This enables limited
    ;   signaling capabilities from MusyX your application program. When a
    ;   flag is set, it remains set until this function is called. It's state will be
    ;   returned, and the flag will then be cleared.
    ;   
    ; Inputs:
    ;   A   Flag number to test for (0-7)
    ;   
    ; Output:
    ;   A   State of the flag
    ;   ZF  State of the flag (zero or not zero)
    and $07
    ld e, a
    ld d, $00
    ld hl, _snd_FlagLookupTable
    add hl, de
    ld a, [wSnd_Flags]
    ld e, a
    and [hl]
    ret z

    ld a, [hl]
    cpl
    ld d, a
    ld a, e
    and d
    ld [wSnd_Flags], a
    ld a, $01
    and a
    ret

    ; $4B04
_snd_MacroOp_15_SPLITKEY:
    ;   Splits the macro flow depending on the midikey
    ; Type: Structure
    ; Description:
    ;   This command is used to conditionally change the flow of execution in
    ;   the current macro. The macro program will jump to the given macrostep
    ;   inside the specified macro, if the current key is higher or the same as the
    ;   key specified in the parameter.
    ; Parameters:
    ;   key = This parameter specifies a key number to compare against. If the
    ;       key you play is higher or the same as this key, the macro will
    ;       branch, otherwise it resumes.
    ;   macro = The ID of the macro to branch to
    ;   step = The step number inside the macro to branch to
    ;
    ;   db $15
    ;   db key
    ;   dw address(macro,step)
    ld a, [wSnd_SelectedVoice_Index]
    sla a
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_KeyCents+1
    add hl, de

    ; $4B10
_snd_MacroOpJumpIfAboveOrEqual:
    ; Sets wSnd_SelectedVoice_MacroReadAddress to a new address if [hl] is above or equal to [bc]
    ld a, [bc]
    inc bc
    ld e, a
    ld a, [hl]
    cp e
    jp nc, _snd_MacroOp_06_GOTO
        inc bc ;Do not jump
        inc bc
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        ld a, c
        ld [hl+], a
        ld [hl], b
        ret

    ; $4B21
_snd_MacroOp_17_SPLITVEL:
    ;   Splits the macro flow depending on the velocity
    ; Type: Structure
    ; Description:
    ;   This command is used to conditionally change the flow of execution in
    ;   the current macro. The macro program will jump to the given macrostep
    ;   inside the specified macro, if the current velocity is higher or the same as
    ;   the velocity parameter.
    ; Parameters:
    ;   velocity = Specifies the velocity to compare the current velocity against.
    ;       If the current velocity is higher or the same, the macro will
    ;       branch, otherwise it will resume.
    ;   macro = The ID of the macro to branch to
    ;   step = The step number inside the macro to branch to
    ;   db $17
    ;   db Velocity
    ;   dw address(macro,step)
    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_Velocity
    add hl, de
    jr _snd_MacroOpJumpIfAboveOrEqual

    ; $4B2D
_snd_MacroOp_16_SPLITRND:
    ;   Splits the macro flow depending on a random number
    ; Type: Structure
    ; Description:
    ;   This command is used to conditionally change the flow of execution in
    ;   the current macro. The macro program will jump to the given macrostep
    ;   inside the specified macro, if the generated random value is higher or the
    ;   same as the rnd parameter.
    ; Parameters:
    ;   rnd = Value to compare the random number against.
    ;   macro = The ID of the macro to branch to
    ;   step = The step number inside the specified macro to branch to
    ;
    ;   db $16
    ;   db rnd
    ;   dw address(macro,step)
    ld a, [bc]
    ld e, a
    inc bc
    call _snd_rnd_256
    cp e
    jp nc, _snd_MacroOp_06_GOTO
        inc bc ;Do not jump
        inc bc
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        ld a, c
        ld [hl+], a
        ld [hl], b
        ret

    ; $4B40
_snd_MacroOp_21_STARTSAMPLE:
    ;   Plays a sample
    ; Type: Voice/Sample
    ; Description:
    ;   Plays back the specified sample.
    ; Parameters:
    ;   Sample ID = The ID of the sample to play back.
    ;
    ;   db $21
    ;   db id(SampleID)
    ld a, [wSnd_SelectedVoice_Index]
    cp $02
    jr nz, .FixReadAddressPosition ;Abort if not track 2

    xor a
    ld [wSnd_Voice_SampleStatus], a
    ld a, [bc]
    call _snd_GetSampleTableRow
    bit 0, e
    jr nz, .NormalQuality
    ;LowQuality
        call _snd_StartSample_SaveDataAndVoiceOn
        call _snd_SetLowQSampleFrequency
        ld hl, wSnd_Voice_SampleStatus
        set 7, [hl]

        .FixReadAddressPosition:
            ld hl, wSnd_SelectedVoice_MacroReadAddress
            ld a, [hl]
            add $01
            ld [hl+], a
            ld a, [hl]
            adc $00
            ld [hl], a
            ret

    .NormalQuality:
        call _snd_StartSample_SaveDataAndVoiceOn
        xor a
        ld [wSnd_Sample_CallbackAddress], a
        ld [wSnd_Sample_CallbackAddress+1], a
        call _snd_EnableSampleTimer
        ld hl, wSnd_Voice_SampleStatus
        set 6, [hl]
        jr .FixReadAddressPosition

    ; 4B7E
_snd_StartSample_SaveDataAndVoiceOn:
    ld [wSnd_Sample_Bank], a
    call snd_InitSampleBlock

    bit 7, h
    jr z, .SkipSampleCrossesBankBoundary
        ld h, $40
        ld a, [wSnd_Sample_Bank]
        inc a
        ld [wSnd_Sample_Bank], a
    .SkipSampleCrossesBankBoundary:

    push hl
    ld hl, wSnd_Sample_Length
    ld a, c
    ld [hl+], a
    ld a, b
    ld [hl+], a
    ld [hl], $00

    ld hl, wSnd_Sample_Address
    pop bc
    ld a, c
    ld [hl+], a
    ld [hl], b

    ld a, $FF
    ld c, $02
    jp _snd_VoiceOn

    ; $4BA9
_snd_SetLowQSampleFrequency:
    ; Note that, on average, only the first 26 nibbles will be played, and the last
    ;   6 nibbles will be skipped every 60 Hz
    ; This is either a bug, or a mechanism to prevent bad sound effects
    ld hl, wSnd_Voice_FrequencyStatus+2
    set 5, [hl] ;Update frequency

    ld hl, wSnd_Voice_KeyCents+4
    ld a, [wSnd_GameboyModel]
    cp $81
    jr nz, .NormalSpeed
    ldh a, [rKEY1]
    bit 7, a
    jr z, .NormalSpeed
    ;DoubleSpeed     ; See table in _snd_SendFrequencyToRegister
        ld de, $2A7B ; 48.489 Hz - 25.86 nibbles/32 will be processed
        jr .Finally
    .NormalSpeed:
        ld de, $2A88 ; 47.628 Hz - 25.40 nibbles/32 will be processed
    .Finally: ;Bug? Why isn't it ld de, $2E87 , which would run it at 60 Hz
    ld a, e   ;Experimentally, sound quality is ++ improved when making this change
    sub [hl]
    inc hl
    ld e, a
    ld a, d
    sbc [hl]
    ld hl, wSnd_Voice_AddNote_DeltaKeyCents+4+1
    ld [hl-], a
    ld [hl], e
    ret

    ; $4BD2
_snd_EnableSampleTimer:
    ; Enables the timer interrupt to playback a sample at normal quality
    ; It sets the Voice 2 frequency to 256 Hz (i.e. runs the 32 nibbles of RAM every 1/256 of a second)
    ;   and sets the timer interrupt at 256 Hz (to load the next nibbles)
        ld hl, wSnd_Voice_FrequencyStatus+2
        set 5, [hl]

        ld hl, wSnd_Voice_KeyCents+4
        ld de, $47B4 ;In _snd_SendFrequencyToRegister, this value will map to 256 Hz
        ld a, e
        sub [hl] ;Instead of directly modifying wSnd_Voice_KeyCents,
        inc hl   ;  we calculate the delta and store it in wSnd_Voice_AddNote_DeltaKeyCents
        ld e, a  ;This means that the original KeyCents value is preserved
        ld a, d  ;  but the actual frequency will be wSnd_Voice_KeyCents+wSnd_Voice_AddNote_DeltaKeyCents
        sbc [hl] ;  i.e. 256 Hz
        ld hl, wSnd_Voice_AddNote_DeltaKeyCents+5
        ld [hl-], a ;Since the RAM contains 32 nibbles, the effective sample playback rate is 8192 Hz
        ld [hl], e 

        ld a, [wSnd_GameboyModel]
        cp $81
        jr nz, .NormalSpeed
        ldh a, [rKEY1]
        bit 7, a
        jr z, .NormalSpeed
            ld a, $07 ;double speed
            ldh [rTAC], a ;16384 Hz -> doubled to 32768 Hz
            ld a, $80     ;32768 Hz/128 -> 256 Hz
            jr .DoneDoubleSpeed
        .NormalSpeed:
            ld a, $06
            ldh [rTAC], a ;65536 Hz
            ld a, $00     ;65536 Hz/256 => 256 Hz
    .DoneDoubleSpeed:
        ld [wSnd_TMA_Mirror], a
        ldh [rTIMA], a
        ldh [rTMA], a ;set timer interrupt to run at 256 Hz (32 nibbles per interrupt = 8192 Hz)
        ldh a, [rIF]
        and $1B
        ldh [rIF], a
        ldh a, [rIE]
        or $04
        ldh [rIE], a ;enable timer interrupt
        ret

    ; $4C17
_snd_MacroOp_22_PLAYKEYSAMPLE:
    ;   Starts a sample on key index
    ; Type: Voice/Sample
    ; Description:
    ;   Starts a sample on voice 3, using the midikey as an index, into an index
    ;   table called “sample-map”. This index table must be defined with one
    ;   single macro using the SAMPLEMAP command. This “sample-map”
    ;   macro must be placed in the Songgroup on the first Drumlist entry. There
    ;   can be only one index table per project.
    ;   See also SAMPLEMAP.
    ;
    ;   db $22
        ld a, [wSnd_SelectedVoice_Index]
        cp $02
        ret nz ; Abort if not on track 2

        xor a
        ld [wSnd_Voice_SampleStatus], a

        ld a, [sdp_NumberOfSampleMapEntries]
        and a
        jr z, .SampleMapUndefined

            ld b, a
            ld a, [wSnd_Voice_KeyCents+1+4]
            cp b
            ret nc ;Abort if input is out-of-range

            ld c, a
            ld hl, sdp_SampleMapMacro_Address
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            ld de, snd_ProjectData
            add hl, de
            ld b, $00
            add hl, bc ;Find the entry in SampleMap

            ld a, [sdp_NumberOfSamples]
            and a
            ret z ;Abort if 0 samples are defined

            ld b, a
            ld a, [hl]
            cp b
            jr c, .StartSample
            ret ;Abort if input is out-of-range from the sample table

        .SampleMapUndefined: ;Fallback: If samplemap is undefined, just take directly from the samples table
            ld a, [sdp_NumberOfSamples]
            and a
            ret z ;Abort if 0 samples are defined

            ld b, a
            ld a, [wSnd_Voice_KeyCents+1+4]
            cp b
            ret nc  ;Abort if input is out-of-range from the sample table

    .StartSample:
        call _snd_GetSampleTableRow
        bit 0, e
        jr nz, .NormalQuality
        ;LowQuality
            call _snd_StartSample_SaveDataAndVoiceOn
            ld hl, wSnd_Voice_SampleStatus
            set 7, [hl]
            call _snd_SetLowQSampleFrequency
            jr .Finally
        .NormalQuality:
            call _snd_StartSample_SaveDataAndVoiceOn
            xor a
            ld [wSnd_Sample_CallbackAddress], a
            ld [wSnd_Sample_CallbackAddress+1], a
            call _snd_EnableSampleTimer
            ld hl, wSnd_Voice_SampleStatus
            set 6, [hl]
    .Finally:
        ld a, $FF
        ld c, $02
        jp _snd_VoiceOn ;VoiceOn is already called in _snd_StartSample_SaveDataAndVoiceOn - so this is possibly superfluous?

    ; $4C7E
_snd_MacroOp_14_LASTNOTE:
    ;   Retrieves the last note of the current voice
    ; Type: Pitch
    ; Description:
    ;   Recalculates the current key by transposing the last key played on this
    ;   voice.
    ; Parameters:
    ;   add = Number of keys to transpose the last key by (-127 – 127)
    ;   detune = The fraction of a key to transpose the last key by (-99 – 99)
    ;
    ;   db $14
    ;   db Add
    ;   db keycents(0,Detune) ;Lower 8 bits
    ld a, [bc]
    inc bc
    ld d, a ;Add
    ld a, [bc]
    inc bc
    ld e, a ;Cents
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b
    ld a, [wSnd_SelectedVoice_Index]
    cp $03
    ret z ;Abort if voice 3

    ld c, a
    ld b, $00
    ld hl, wSnd_Voice_FrequencyStatus
    add hl, bc
    set 5, [hl]

    ld hl, wSnd_sss_LastKey
    add hl, bc
    ld a, d
    add [hl]
    ld d, a

    sla c
    ld hl, wSnd_Voice_KeyCents
    add hl, bc
    ld a, e
    sub [hl]
    inc hl
    ld e, a
    ld a, d
    sbc [hl]

    ld hl, wSnd_Voice_AddNote_DeltaKeyCents+1
    add hl, bc
    ld [hl-], a
    ld [hl], e ;Store (inputKeyCents + wSnd_sss_LastKey) - wSnd_Voice_KeyCents
    ret

    ; $4CB3
_snd_MacroOp_26_PLAYMACRO:
    ;   Start a macro on another voice
    ; Type: Structure
    ; Description:
    ;   Starts another macro on the specified voice. Can be used to start 2 or
    ;   more macros at the same time from a single note.
    ; Parameters:
    ;   Voice Nr. = Identifies the voice to be associated with the macro to be started.
    ;       (0=Voice1,..., 3=Voice4)
    ;   Macro ID = The ID of the macro to start
    ;   Don’t reset = If this flag is set to ON the voice specified will not be reset
    ;       This is useful to take over the voice as it is at this point.
    ;   db $26
    ;   db IF Voice <= 3
    ;           x = Voice
    ;      ELSE
    ;           x = $FF
    ;      x | (DontResetBool*$80)
    ;   db id(SoundMacroID)
    ld a, [bc]
    ld e, a
    inc bc
    ld a, [bc]
    ld d, a
    inc bc
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, c
    ld [hl+], a
    ld [hl], b

    ld a, [wSnd_SelectedVoice_Index]
    ld c, a
    ld b, $00
    ld hl, wSnd_sss_CurrentKey
    add hl, bc
    ld a, d
    ld d, [hl] ;Get the input key (assuming the SoundMacro comes from a song)

    ld hl, wSnd_Voice_FrequencyStatus
    add hl, bc
    bit 1, [hl]
    push af ;Get the SFX/Song bit

    ld hl, wSnd_Voice_Priority
    add hl, bc
    ld a, [hl] ;Get the priority

    ld hl, wSnd_Voice_Velocity
    add hl, bc
    ld c, e
    ld b, a
    pop af
    ld e, [hl] ;Get the velocity
    jr z, .SongMacro
    ;SFXMacro
        call _snd_GenerateMacro_SFX
        ret z

        ld hl, wSnd_Voice_FrequencyStatus
        add hl, bc
        set 1, [hl] ;Set SFX
        jr .Finally

    .SongMacro:
        call _snd_GenerateMacro_Song
        ret z

    .Finally:
    ld a, [wSnd_SelectedVoice_Index]
    cp c
    ret nz

    sla c ;If the Macro overwrote itself, save the new ReadAddress
    ld hl, wSnd_Voice_MacroReadAddress
    add hl, bc
    ld a, [hl+]
    ld e, [hl]
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld [hl+], a
    ld [hl], e
    ret

    ; $4D04
_snd_MacroOp_1B_PORTLAST:
    ;   Portamento from the last note
    ; Type: Pitch
    ; Description:
    ;   The pitch will slide from the last known value to the pitch of the current
    ;   key, plus the keys and cents parameters, in the specified time.
    ; Parameters:
    ;   Keys = Transposes the current key by this value (-127 ~ 127)
    ;   Cents = Transposes the current key by a fraction of one key (-99 ~ 99)
    ;       Can be used together with the whole key transposed
    ;   ms = A time in milliseconds for the portamento to be finished
    ;
    ;   db $1B
    ;   dw keycents(Key,Cents)
    ;   dw cycles(max(ms,1))
    ld l, c
    ld h, b
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a ;bc = keycents
    ld a, [hl+]
    ld d, a
    ld a, [hl+]
    ld e, a ;de = cycles
    ld a, l
    ld [wSnd_SelectedVoice_MacroReadAddress], a
    ld a, h
    ld [wSnd_SelectedVoice_MacroReadAddress+1], a
    ld a, [wSnd_SelectedVoice_Index]
    cp $03
    ret nc ;Voice 3 invalid

    push de
    push bc
    ld e, a
    sla e
    ld d, $00
    ld hl, wSnd_Voice_KeyCents
    add hl, de
    ld a, [hl+]
    ld b, [hl]
    ld c, a

    pop hl
    add hl, bc
    ld c, l
    ld b, h
    push bc ;Absolute target KeyCents

    ld hl, wSnd_Voice_LastKey
    add hl, de
    ld a, [hl+]
    ld b, [hl]
    ld hl, wSnd_Voice_KeyCents
    add hl, de
    ld [hl+], a
    ld [hl], b
    pop bc
    pop de
    ld a, [wSnd_SelectedVoice_Index]
    jp _snd_SetupPortamento

    ; $4D43
_snd_MacroOp_27_ENVELOPE_ASCENDING:
    ;   Starts a software envelope
    ; Type: Volume/Panning
    ; Description:
    ;   Starts a software envelope. The volume will be faded out/in to mute level
    ;   or full volume in the time specified. Due to Game Boy hardware
    ;   restrictions, this may be of lower quality than the hardware envelope.
    ; Parameters:
    ;   Flag = If OFF, fades out to mute level, if ON fades in to maximum level
    ;   ms = Time to fade out to zero in milliseconds
    ;
    ;   x = cycles(max(ms,1))
    ;   db $27 (flag is ON)
    ;   dw max(1,$0F00//x)
    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_VolumeStatus
    add hl, de
    ld a, [hl]
    and $F0
    or $31 ;bit 5, 4, 0
    ld [hl], a

    xor a
    ld hl, wSnd_Voice_EnvelopeVolume
    add hl, de
    ld [hl], $00

    sla e
    ld hl, wSnd_Voice_Envelope_VolumeCents
    add hl, de
    ld [hl+], a
    ld [hl], a

    ld l, c
    ld h, b
    ld a, [hl+]
    ld c, [hl]
    ld hl, wSnd_Voice_Envelope_DeltaVolumeCents+1
    add hl, de
    ld [hl-], a
    ld [hl], c

    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, [hl]
    add $02
    ld [hl+], a
    ld a, [hl]
    adc $00
    ld [hl], a
    ret

    ; $4D78
_snd_MacroOp_1C_UNDEFINED_29_ADD_SET_PRIO:
    ; OpCode $1C is undefined in MuConv 1.04
    ;
    ; ADD_SET_PRIO
    ;   Changes the priority of a sound effect
    ; Type: Special
    ; Description:
    ;   Modifies the current priority of a sound effect. Depending on the state of
    ;   Flag, the Value is either set directly or added to the current priority.
    ; Parameters:
    ;   Flag = If OFF, adds the Value parameter to the current priority. If ON, 
    ;       sets the Value parameter immediately as the new priority.
    ;   Value = Ranges from –128 to 127. If the Flag parameter is OFF, it will be
    ;       added to the current priority. If the Flag parameter is ON, this 
    ;       value will be set as an absolute priority (using the 2's 
    ;       complement of the value. So –1 becomes 255 and –128 
    ;       becomes 128).
    ;
    ;   db $29
    ;   db Flag
    ;   db Value
    ld a, [wSnd_SelectedVoice_Index]
    ld e, a
    ld d, $00
    ld hl, wSnd_Voice_Priority
    add hl, de
    ld a, [bc]
    inc bc
    and a
    ld a, [bc]
    jr nz, .SetPriority
        ld e, a
        ld a, [hl]
        bit 7, e
        jr nz, .NegativeAddPriority
            add e
            jr nc, .SetPriority
            ld a, $FF ;If exceeds 255, set to 255
            jr .SetPriority
        .NegativeAddPriority:
            sub e ;Is there a bug here? Should you add the original number to the negative number?
            jr nc, .SetPriority ;Does this check work properly with e being a negative number?
            xor a ;If negative number, set priority to zero.
    .SetPriority:
    ld [hl], a
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld a, [hl]
    add $02
    ld [hl+], a
    ld a, [hl]
    adc $00
    ld [hl], a
    ret

    ; $4DA6
_snd_Macro_OpcodeTable:
    dw _snd_MacroOp_00_END_23_STOP
    dw _snd_MacroOp_01_VIBRATO
    dw _snd_MacroOp_02_PORTAMENTO
    dw _snd_MacroOp_03_PITCHSWEEP
    dw _snd_MacroOp_04_WAIT
    dw _snd_MacroOp_05_LOOP
    dw _snd_MacroOp_06_GOTO
    dw _snd_MacroOp_07_SETNOTE
    dw _snd_MacroOp_08_ADDNOTE
    dw _snd_MacroOp_09_SETVOLUME
    dw _snd_MacroOp_0A_PANNING
    dw _snd_MacroOp_0B_RNDNOTE
    dw _snd_MacroOp_0C_VOICE_ON_WAVE_ON
    dw _snd_MacroOp_0D_VOICE_OFF
    dw _snd_MacroOp_0E_SETVOICE
    dw _snd_MacroOp_0F_SETADSR
    dw _snd_MacroOp_10_SETNOISE
    dw _snd_MacroOp_11_ENVELOPE_DESCENDING
    dw _snd_MacroOp_12_KEYOFF
    dw _snd_MacroOp_13_HARDENVELOPE
    dw _snd_MacroOp_14_LASTNOTE
    dw _snd_MacroOp_15_SPLITKEY
    dw _snd_MacroOp_16_SPLITRND
    dw _snd_MacroOp_17_SPLITVEL
    dw _snd_MacroOp_18_TRAP_KEYOFF
    dw _snd_MacroOp_19_UNTRAP_KEYOFF
    dw _snd_MacroOp_1A_SENDFLAG
    dw _snd_MacroOp_1B_PORTLAST
    dw _snd_MacroOp_1C_UNDEFINED_29_ADD_SET_PRIO
    dw _snd_MacroOp_1D_PWN_START
    dw _snd_MacroOp_1E_PWN_UPDATE
    dw _snd_MacroOp_1F_STOP_MOD
    dw _snd_MacroOp_20_RESET_MOD
    dw _snd_MacroOp_21_STARTSAMPLE
    dw _snd_MacroOp_22_PLAYKEYSAMPLE
    dw _snd_MacroOp_00_END_23_STOP
    dw _snd_MacroOp_24_PWN_FIXED
    dw _snd_MacroOp_25_PWN_VELOCITY
    dw _snd_MacroOp_26_PLAYMACRO
    dw _snd_MacroOp_27_ENVELOPE_ASCENDING
    dw _snd_MacroOp_28_CURRENTVOL
    dw _snd_MacroOp_1C_UNDEFINED_29_ADD_SET_PRIO

    ; $4DFA
_snd_TryChooseVoice_andAssignSoundMacro:
    ; Determine target voice based on SETVOICE, or c if SETVOICE is not defined
    ; Assign SoundMacro if higher priority
    ; Reset Voice automatically unless disabled by SETVOICE
    ;
    ; Input:
    ;   a = SoundMacroID
    ;   b = Priority
    ;   c = Target voice (0 for SFX - gets overwritten by SETVOICE)
    ;   d = 1 for SFX
    ld e, a
    ld a, d
    ld [wSnd_Temp_E], a
    ld d, $00
    sla e
    rl d
    ld hl, sdp_SoundMacroLookupTable
    add hl, de
    ld a, [hl+]
    ld e, a
    ld a, [hl-]
    ld d, a
    ld hl, sdp_SoundMacroLookupTable
    add hl, de            ;Address of the SoundMacro
    ld a, [hl]
    cp $0E ;SETVOICE
    jr nz, _snd_TryAssignSoundMacroToVoiceC_andResetVoiceUnlessDisabled ;No SETVOICE
        inc hl
        ld a, [hl+]
        bit 4, a  ;ToggleBool
        jr z, .SkipToggleBool
                and $80
                ld e, a ;Store DontResetBool flag
                ld a, [wSnd_sss_Status]
                bit 6, a
                jr nz, .SkipInitializeToggleBit
                    set 6, a
                    ld d, a
                    ld a, c
                    and $01
                    sla a
                    swap a
                    or d
                    ld [wSnd_sss_Status], a ;Set bit 6, and load c%2 into bit 5
            .SkipInitializeToggleBit:
                ld d, a
                swap a
                srl a
                and $01 ;Get bit 5
                or e ;Or with DontResetBool flag
                ld c, a
                ld a, d
                xor $20                  ;Return c (bit 7 = DontResetBool, bit 0 = voice from bit5 of wSnd_sss_Status)
                ld [wSnd_sss_Status], a  ;Flip bit 5
                jr _snd_TryAssignSoundMacroToVoiceC_andResetVoiceUnlessDisabled
        .SkipToggleBool:
            cp $FF
            jr nz, .SpecificVoice
                ld a, $80 ;Never happens because $FF goes through the ToggleBool code instead
                or c
            .SpecificVoice:
                ld c, a ;bit 7 = DontResetBool, bit 0-1 = target voice

    ; $4E4D
_snd_TryAssignSoundMacroToVoiceC_andResetVoiceUnlessDisabled:
    ; Assign SoundMacro if higher priority
    ; Reset Voice automatically unless disabled by SETVOICE
    ; Input:
    ;   b = priority
    ;   c = bit 7 is DontResetBool, bit 0-1 is target voice
    ;   hl = SoundMacro read address
    bit 7, c
    jp nz, _snd_AssignMacroToVoiceC_IfPriority ;Skip Reset voice

        ld a, c
        cp $02
        jr nz, .SkipVoice2 ;Desired voice is not 2, so don't need to do special Sample stuff
            ld a, [wSnd_Voice_Priority+2]
            ld e, a
            ld a, b
            cp e
            jr c, .SkipVoice2 ;Priority check fail, so don't erase voice
                ld a, [wSnd_Voice_SampleStatus]
                bit 6, a
                jr z, .NotNormalQuality
                    xor a
                    ldh [rTAC], a ;Disable timer
                    ldh a, [rIE]
                    and $1B
                    ldh [rIE], a ;Disable timer interrupt
                    ldh a, [rIF]
                    and $1B
                    ldh [rIF], a ;Disable timer interrupt flag
            .NotNormalQuality:
                xor a
                ld [wSnd_Voice_SampleStatus], a

    .SkipVoice2:
        call _snd_AssignMacroToVoiceC_IfPriority
        ret z ;Priority check fail

        ld hl, wSnd_Voice_EnvelopeVolume
        add hl, bc
        ld [hl], $0F
        ld hl, wSnd_Voice_FrequencyStatus
        add hl, bc
        ld [hl], a
        ld hl, wSnd_Voice_VolumeStatus
        add hl, bc
        ld [hl], a
        sla c
        ld a, c
        cp $06
        jr z, .SkipVoice012 ;Skip if voice 3

            ld a, [wSnd_Temp_E]
            and a
            jr nz, .SFX
            ;Song
                ld hl, wSnd_Voice_KeyCents
                add hl, bc
                ld a, [hl+]
                ld d, [hl]
                ld e, a
                ld hl, wSnd_Voice_AddNote_DeltaKeyCents
                add hl, bc
                ld a, [hl+]
                ld h, [hl]
                ld l, a
                add hl, de
                ld e, l
                ld d, h
                ld hl, wSnd_Voice_LastKey
                add hl, bc
                ld a, e
                ld [hl+], a
                ld [hl], d
            .SFX:
                xor a
                ld hl, wSnd_Voice_Modulation_DeltaKeyCents
                add hl, bc
                ld [hl+], a
                ld [hl], a ;Reset wSnd_Voice_Modulation_DeltaKeyCents

                ld hl, wSnd_Voice_AddNote_DeltaKeyCents
                add hl, bc
                ld [hl+], a
                ld [hl], a  ;Reset wSnd_Voice_AddNote_DeltaKeyCents

    .SkipVoice012:
        xor a
        ld hl, wSnd_Voice_KeyCents
        add hl, bc
        ld [hl+], a
        ld [hl], a
        srl c
        ld hl, _snd_NR51Panning_OnBilateralBitmap
        add hl, bc
        ld a, [wSnd_NR51Panning_Mirror]
        or [hl]
        ld [wSnd_NR51Panning_Mirror], a
        push bc
        call _snd_VoiceOff
        pop bc
        rlca
        ret

    ; $4EDA
_snd_NR51Panning_OnBilateralBitmap:
    db $11, $22, $44, $88

    ; $4EDE
_snd_AssignMacroToVoiceC_IfPriority:
    ; Assigns a SoundMacro to voice c if priority b is higher than the current priority
    ; Inputs:
    ;   c = bit 7 is DontResetBool, bit 0-1 is target voice
    ;   b = Priority of new SoundMacro
    ;   hl = SoundMacro reading address
    ; Returns nz if success (i.e. higher priority)
        push hl
        ld a, b
        res 7, c
        ld b, $00
        ld hl, wSnd_Voice_Priority
        add hl, bc
        cp [hl]
        jr nc, .NewSoundMacroIsHigherPriority
            pop hl
            xor a
            ret
    .NewSoundMacroIsHigherPriority:
        ld [hl], a ;Store new priority
        pop hl
        ld a, l
        ld d, h
        sla c
        ld hl, wSnd_Voice_MacroReadAddress
        add hl, bc
        ld [hl+], a
        ld [hl], d ;Store read address
        xor a
        ld hl, wSnd_Voice_TrapKeyOffAddress
        add hl, bc
        ld [hl+], a
        ld [hl], a ;Store null KeyOff address
        ld hl, wSnd_Voice_WaitIndex
        add hl, bc
        ld [hl+], a
        ld [hl], a ;Disable WAIT counter
        srl c
        ld hl, wSnd_Voice_LoopIndex
        add hl, bc
        ld [hl], a ;Disable LOOP counter
        ld hl, wSnd_Voice_UniqueID
        add hl, bc
        ld [hl], a ;Reset UniqueID
        rlca ;Return nz
        ret

    ; $4F15
_snd_GenerateMacro_SFX:
    ; Inputs:
    ;   c = Voice
    ;   a = SoundMacroID
    ;   d = Song Key -> Set to 1, which indicates SFX
    ;   b = Priority
    ;   e = Velocity
    push de
    ld d, $01
    jr _snd_GenerateMacro

    ; $4F1A
_snd_GenerateMacro_Song:
    ; Inputs:
    ;   c = Voice
    ;   a = SoundMacroID
    ;   d = Song Key
    ;   b = Priority -> Set to 0 as comes from song
    ;   e = Velocity
    push de
    ld b, $00
    ld d, b

    ; $4F1E
_snd_GenerateMacro:
    ; Returns nz if success, or z if no voice available
    ; Returns c as Voice index
    call _snd_TryChooseVoice_andAssignSoundMacro
    pop de
    ret z ;Failed

    ld hl, wSnd_Voice_FrequencyStatus
    add hl, bc
    res 1, [hl] ;Sets as Song-source, so we need to fix this after if running _snd_GenerateMacro_SFX
    set 5, [hl]

    ld hl, wSnd_Voice_VolumeStatus
    add hl, bc
    set 5, [hl]

    ld hl, wSnd_Voice_Velocity
    add hl, bc
    ld [hl], e

    ld hl, wSnd_Voice_KeyCents+1
    sla c
    add hl, bc
    ld a, d
    ld [hl-], a
    ld [hl], $00
    srl c
    rlca
    ret

    ; $4F44
snd_StartSFX::
    ; Purpose:
    ;   This function will start a sound effect.
    ;   
    ; Inputs:
    ;   A   ID of the effect to start
    ;   B   Volume (0-15, or 255 for default)
    ;   C   Position (0=left, 1=center, 2=right)
    ;   
    ; Output:
    ;   A   Active ID, or 0 if effect could not be started
    ;   
    ; Remarks:
    ;   The ID returned by this function, if not zero, is a handle for this sound
    ;   effect. You need to keep it for a later call to snd_StopSFX.
    ld hl, sdp_NumberOfSFXs
    cp [hl]
    jp nc, .InvalidID  ;Abort if ID out of range
        ld e, a
        ld d, $00
        sla e
        rl d
        sla e
        rl d
        ld hl, sdp_SFXTableAddress
        ld a, [hl+]
        ld h, [hl]
        ld l, a
        add hl, de
        ld de, snd_ProjectData
        add hl, de     ;Get sdp_SFXLookupTable + 4*ID
        
        push bc
        ld a, [hl+] ;SoundMacro ID
        ld b, [hl]  ;Priority
        inc hl
        ld c, $00
        ld d, $01
        push hl
        call _snd_TryChooseVoice_andAssignSoundMacro ;Assign SoundMacro to a voice
        pop hl
        jr z, .NewMacroIsTooLowPriority

        ld a, [hl+]
        ld d, a    ;default key
        ld e, [hl] ;default velocity

        ld hl, wSnd_Voice_FrequencyStatus
        add hl, bc
        set 1, [hl]
        set 5, [hl]
        ld hl, wSnd_Voice_VolumeStatus
        add hl, bc
        set 5, [hl]

        ld hl, wSnd_Voice_KeyCents+1
        sla c
        add hl, bc
        ld a, d
        ld [hl-], a
        ld [hl], $00 ;Set the keycents

        srl c
        ld a, e ;Default velocity
        pop de
        bit 7, d ;de <- bc (i.e. velocity = $FF)
        jr nz, .UseDefaultVelocity
            ld a, d ;Assigned velocity
    .UseDefaultVelocity:
        and $0F
        push de
        ld e, a
        ld a, [wSnd_SFXSongVolume]
        swap a
        and $0F
        call _snd_CalculateOutputVolume
        pop de
        ld hl, wSnd_Voice_Velocity
        add hl, bc
        ld [hl], a   ;Store the velocity

        push bc
        ld a, c
        ld [wSnd_SelectedVoice_Index], a
        ld a, e
        cp $FF
        call nz, _snd_SetPanningToA ;If c = $FF, use default panning which is always bilateral

        ld a, [wSnd_Voice_UniqueID_Generator] ;incrementing running count of unique ids
    .FindUniqueIDLoop:
            inc a
            jr z, .FindUniqueIDLoop ;0 is an invalid unique id
            ld hl, wSnd_Voice_UniqueID
            cp [hl]
            jr z, .FindUniqueIDLoop
            inc hl
            cp [hl]
            jr z, .FindUniqueIDLoop
            inc hl
            cp [hl]
            jr z, .FindUniqueIDLoop
            inc hl
            cp [hl]
            jr z, .FindUniqueIDLoop ;If any of the 4 active soundmacros has this id, then the id is invalid
        ld [wSnd_Voice_UniqueID_Generator], a
        ld hl, wSnd_Voice_UniqueID
        pop bc
        add hl, bc
        ld [hl], a
        ret

    .NewMacroIsTooLowPriority:
        pop bc
    .InvalidID:
    xor a
    ret

    ; $4FD8
_snd_SelectVoiceFromUniqueID:
    ; Given unique id a, find the corresponding voice and save it to wSnd_SelectedVoice_Index
    ; Returns nz if found, or z if not found
    ;
    ;(There are 255 unique ids ($01-$FF) and each new id is assigned sequentially)
        and a
        ret z ;0 is an invalid uniqueid

        ld hl, wSnd_Voice_UniqueID
        ld c, $00
        ld b, a
    .Loop4Voices:
            ld a, [hl+]
            cp b
            jr z, .UniqueIDFound ;found the id
            inc c
            bit 2, c
            jr z, .Loop4Voices
                xor a
                ret ;Failed to find id
    .UniqueIDFound:
        ld a, c
        ld [wSnd_SelectedVoice_Index], a
        rlca
        ret


    ; $48DD
snd_StopSFX::
    ; Purpose:
    ;   This function will stop a sound effect, previously started by
    ;   snd_StartSFX.
    ;   
    ; Inputs:
    ;   A   Active ID returned by snd_StartSFX
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   The ID returned by snd_StartSFX, will remain unique, as long as no
    ;   other sound effect cancels this one due to a higher priority. In this
    ;   particular case the ID you kept will become invalid, and might cancel
    ;   another soundeffect that has received this ID assignment, in the
    ;   meantime.
    ;   
    ;   For one-shot sound effects, this is not a problem, since you need not
    ;   stop them, explicitly. For permanent effects however, this could be an
    ;   issue, which is best solved by having the sound designer assign
    ;   appropriate priorities to the sound effects, to minimize these
    ;   occurrences.
    call _snd_SelectVoiceFromUniqueID
    jp nz, _snd_KeyOff_SelectedTrack
    ret

    ; $4FF8
snd_SFXPitch::
    ; Undocumented public function
    ; This function will change the key of a specific sound effect, previously
    ; started by snd_StartSFX.
    ;
    ; Inputs:
    ;   A   Active ID returned by snd_StartSFX
    ;   D   New Key
    ;   E   New Cents (positive detune amount)
    ;   
    ; Output:
    ;   None.
    call _snd_SelectVoiceFromUniqueID
    jp nz, _snd_SetNote
    ret

    ; $4FFF
snd_SFXVolume::
    ; Undocumented public function
    ; This function will change the volume of a specific sound effect, previously
    ; started by snd_StartSFX.
    ;   
    ; Inputs:
    ;   A   Active ID returned by snd_StartSFX
    ;   E   New Volume (0-F)
    ;   
    ; Output:
    ;   None.
    call _snd_SelectVoiceFromUniqueID
    jp nz, _snd_SetSelectedVoiceToVolumeE
    ret

    ; $5006
_snd_HandleMacros:
    ; Loops through the 4 voices and runs each SoundMacro if enabled
    ld de, $0000

    .LoopVoice:
        ld hl, wSnd_Voice_MacroReadAddress
        add hl, de
        ld a, [hl+]
        ld h, [hl]
        ld l, a
        or h
        jr z, .NoMacro ;macro address is $0000 i.e. null so skip
            push de
            ld a, e
            srl a
            ld [wSnd_SelectedVoice_Index], a
            call _snd_ReadMacroOpcode
            pop de
            ld hl, wSnd_Voice_MacroReadAddress
            add hl, de
            ld a, [wSnd_SelectedVoice_MacroReadAddress]
            ld [hl+], a
            ld a, [wSnd_SelectedVoice_MacroReadAddress+1]
            ld [hl], a
    .NoMacro:
        ld a, e
        cp $06
        ret z
        inc e
        inc e
        jr .LoopVoice

    ; $5032
_snd_ReadMacroOpcode:
    ; Processes a SoundMacro
    ; Inputs:
    ;   hl = wSnd_Voice_MacroReadAddress
    ld a, [hl+]
    cp $04 ;WAIT
    push af
    ld c, l
    ld b, h
    ld hl, wSnd_SelectedVoice_MacroReadAddress
    ld [hl], c
    inc hl
    ld [hl], b ;Store the hl pos into wSnd_SelectedVoice_MacroReadAddress
    ld hl, _snd_Macro_OpcodeTable
    sla a
    add l
    ld l, a
    ld a, h
    adc $00
    ld h, a
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    call .JpToHL ;Jump to the opcode via the lookup table
    pop af
    ret z ;Exit if opcode was WAIT
        ld hl, wSnd_SelectedVoice_MacroReadAddress
        ld a, [hl+]
        ld h, [hl]
        ld l, a
        or h
        jr nz, _snd_ReadMacroOpcode ;Keep going until ReadAddress is $0000 (or if WAIT as above)
    ret

    ; $505B
    .JpToHL:
        jp hl

    ; $505C
_snd_VoiceOff:
    ; Sets voice c's panning to 0 and volume to 0
    ; Does not disable the SoundMacro
    ;
    ; Inputs:
    ;   c = voice index
    ld b, $00
    ld hl, wSnd_Voice_FrequencyStatus
    add hl, bc
    res 7, [hl] ;set flag 7 to 0
    ld hl, _snd_VoiceOn.NR51Panning_OffBitmap
    add hl, bc
    ldh a, [rNR51]
    and [hl]
    ldh [rNR51], a ;set panning to 0
    xor a
    ld b, $01 ;set volume to 0 and disable any hardware sweeps

    ; $5070
_snd_SendVolumeToRegister:
    ; Sends the volume to the sound register corresponding to voice index c
    ; Inputs:
    ;   b = if 1, disable any hardware volume sweeps. If 0, preserve any sweeps
    ;   c = voice index
    ;   a = volume (wSnd_Voice_Velocity * wSnd_Voice_EnvelopeVolume)
    ;
    ; The true volume is a multiplication of 4 different volumes:
    ;   (1) wSnd_Voice_Velocity = wSnd_SFXSongVolume * TargetVolume (e.g. SETVOLUME)
    ;   (2) wSnd_Voice_InternalVolume = wSnd_Voice_Velocity * wSnd_Voice_EnvelopeVolume
    ;   (3) wSnd_Voice_InternalVolume is multiplied by hardware volume rNR50 (snd_SetMasterVolume)
    ld e, a
    ld a, c
    cp $02
    ld a, e
    jr z, .Voice2 ;Voice 2 is handled specially

        ld d, $00
        ld e, c
        ld hl, wSnd_Voice_InternalVolume
        add hl, de
        ld [hl], a ;Store volume
        ld c, a
        ld hl, .Registers_NR02VolumeTable
        add hl, de
        bit 0, b
        ld b, c
        ld c, [hl]
        jr nz, .DestroySweepData
            ld a, [$FF00+c]
            and $0F ;Keep volume sweep data
            swap b
            or b
            ld [$FF00+c], a
            jr .DoRestartSound
        .DestroySweepData:
            ld a, b
            swap a ;Volume sweet data is destroyed
            ld [$FF00+c], a

    .DoRestartSound:
        ld hl, .Registers_NR04RestartSoundTable
        add hl, de
        ld c, [hl]
        ld hl, wSnd_NR04FreqUpper_Mirror
        add hl, de
        ld a, e
        cp $03
        jr z, .NR34
            ld a, $80 ;NR14,24,44
            or [hl]
            ld [$FF00+c], a
            ret
        .NR34:
            ld a, $80 ;NR34 doesn't have a frequency mirror with which to OR
            ld [$FF00+c], a
            ret

    .Voice2:
        and $0C
        ld d, a
        ld a, [wSnd_Voice_InternalVolume+2]
        and $0C
        cp d
        ld a, e
        ld [wSnd_Voice_InternalVolume+2], a
        ret z ;ret if there's no change to the volume (upper 2 bits are identical)

        ld e, c
        ld c, a
        ld d, $00
        ld a, [wSnd_Voice_SampleStatus]
        bit 0, a
        ret nz ;If PWN, then don't change the volume control

        ld b, $00
        srl c
        srl c ;Take the upper 2 bits of the volume
        ld hl, .NR32Volume_Bitmap
        add hl, bc
        ld b, [hl] ;Map the 2 upper bits to the correct volume output
        ldh a, [rNR32]
        and $9F ;Keep the 6 unused bits the same (even though they seem unused)
        or b
        ldh [rNR32], a
        jr .DoRestartSound

        ; $50DA
    .Registers_NR02VolumeTable:
        ; Points to NR12, NR22, NR32, NR42
        db $12, $17, $1C, $21

        ; $50DE
    .Registers_NR04RestartSoundTable:
        ; Points to NR14, NR24, NR34, NR44 in order to access bit 7 (restart sound)
        db $14, $19, $1E, $23

        ; 50E2
    .NR32Volume_Bitmap:
        ; Maps to Mute, 25%, 50%, 100% volumes
        db $00, $60, $40, $20

    ; $50E6
_snd_VoiceOn:
    ; Inputs:
    ;   a = Voice 0-1: Duty
    ;       Voice 2:   SampleID ($FF = don't load sample)
    ;       Voice 3:   2-bit volume
    ;   c = Voice
    ld e, a
    ld b, $00
    ld a, c
    cp $03
    jp z, .FinishedStoringTargetKeyCents ;Voice 3
    cp $02
    jr z, .SampleOn ;Voice 2
            ld a, e ;Voice 0-1. Check Duty
            cp $04
            jr nc, .ResetSound ;Duty is >3 ($FF), so don't write the duty to the register here
                ld hl, .Registers_NR01DutyTable
                add hl, bc
                ld l, [hl]
                ld h, $FF
                rrc e
                rrc e
                ld [hl], e ;Write the duty if duty is 0-3
            .ResetSound:
                ld hl, wSnd_NR04FreqUpper_Mirror
                add hl, bc
                ld a, [hl]
                ld hl, .Registers_NR04RestartSoundTable
                add hl, bc
                ld l, [hl]
                or $80
                ld h, $FF
                ld [hl], a  ;Reset sound of NR04
                jr .FinishedStoringTargetKeyCents

        .SampleOn:
            ld a, e
            cp $FF
            jr z, .SkipLoadSampleIntoRAM
                push bc
                call _snd_GetSampleTableRow
                ld [wSnd_Sample_Bank], a
                call snd_TransferSampleBlock
                pop bc
            .SkipLoadSampleIntoRAM:
            ld d, c
            ld hl, wSnd_NR04FreqUpper_Mirror
            add hl, bc
            ld a, [hl]
            ld hl, .Registers_NR04RestartSoundTable
            add hl, bc
            ld c, [hl]
            or $80
            ld [$FF00+c], a ;Reset sound of NR34
            and $7F
            ld [$FF00+c], a
            ld c, d

    .FinishedStoringTargetKeyCents:
        ld hl, wSnd_Voice_FrequencyStatus
        add hl, bc
        set 7, [hl] ;Voice is on

        ld hl, .NR51Panning_OffBitmap
        add hl, bc
        ld b, [hl]
        ldh a, [rNR51]
        and b
        ld c, a
        ld a, b
        cpl
        ld b, a
        ld a, [wSnd_NR51Panning_Mirror]
        and b
        or c
        ldh [rNR51], a ;Turn on the panning for the voice based on wSnd_NR51Panning_Mirror
        ret

    ; $5151
    .Registers_NR01DutyTable:
        ;Points to NR11, NR21 in order to set the duty
        db $11, $16

    ; $5153
    .Registers_NR04RestartSoundTable:
        ; Points to NR14, NR24, NR34 in order to access bit 7 (restart sound)
        db $14, $19, $1E

    ; $5156
    .NR51Panning_OffBitmap:
        ; Bitmap to disable the panning left/right of channels 0-3
        db $EE, $DD, $BB, $77

    ; $515A
_snd_GetSampleTableRow:
    ; Gets a row of data from sdp_SampleTableAddress
    ; a is the desired sample index
    ; Output:
    ;   lh cb e a as the 6 bytes of data
    ;   hl = address
    ;   bc = length/$10
    ;   e = 1 if high quality else 0
    ;   a = bank
    ;       a is modified (BaseBank is added), making it the true bank
    ld hl, sdp_SampleTableAddress
    ld e, [hl]
    inc hl
    ld d, [hl]
    ld hl, snd_ProjectData
    add hl, de
    ld d, $00
    ld e, a
    sla e
    rl d
    ld c, e
    ld b, d
    sla e
    rl d
    add hl, de
    add hl, bc
    ld a, [hl+]
    ld e, a
    ld a, [hl+]
    ld d, a
    push de
    ld a, [hl+]
    ld c, a
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld e, a
    ld a, [wSnd_BaseBank]
    add [hl]
    pop hl
    ret

    ; $5184
_snd_SendFrequencyToRegister:
    ; Sends the frequency associated with the KeyCents to the sound register
    ;   corresponding to voice index c
    ; Inputs:
    ;   bc = Voice index
    ;   hl = KeyCents (wSnd_Voice_KeyCents + wSnd_Voice_Modulation_DeltaKeyCents + wSnd_Voice_AddNote_DeltaKeyCents)

        push bc

        ld a, h
    .AddLoop:
            bit 7, a
            jr z, .SubtractLoop
            add $54
        jr .AddLoop
    .SubtractLoop:
            cp $54
            jr c, .DoneLoop
            sub $54
        jr .SubtractLoop ;First, get key % $54 to use the key in a lookup table.
                         ;It assumes the key is $24-$77

    .DoneLoop:
        ld d, $00
        ld e, a
        ld a, l
        and a
        jr nz, .CentsNonZero
            sla e ;If the Cents is Zero, we can simply use our lookup table
            ld hl, .KeyToFrequencyTable
            add hl, de
            ld e, c
            ld a, [hl+]
            ld b, [hl]
    .SendFrequency:
        ld hl, wSnd_NR04FreqUpper_Mirror
        add hl, de
        ld [hl], b
        ld hl, .Registers_NR03FrequencyLowerTable
        add hl, de
        ld c, [hl]
        ld [$FF00+c], a
        inc c
        ld a, b
        ld [$FF00+c], a
        pop bc
        ret
    .CentsNonZero:
            ld hl, .KeyFrequencyDeltaTable ;If the Cents is Nonzero, we need to do extra math
            add hl, de   ;This table gives the delta between the current and subsequent key
            ld d, c      ;So if you do Delta*Cents/$100, you essentially do a
            ld c, [hl]   ;Linear interpolation between the current and next key
            call _snd_c_Times_a
            ld c, h      ; The interpolated value is stored in c
            ld a, d
            ld b, $00
            ld d, b
            sla e
            ld hl, .KeyToFrequencyTable ;Look up the base key frequency
            add hl, de
            ld e, a
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            add hl, bc  ;Add the base key frequency with the interpolated cent-value
            ld a, l
            ld b, h
            jr .SendFrequency

    ; $51D3
    .Registers_NR03FrequencyLowerTable:
    ; Registers NR13, NR23, NR33
    db $13, $18, $1D

    ; $51D6
    dw $0782      ;53 -1   1040.25 Hz   C6 (UNUSED)
    ; $51D8
    .KeyToFrequencyTable:
    ; Given a key X, look up the X % $54 entry which is what you should
    ;   submit to the frequency registers to produce the desired keys
    ; Note that the labelled frequency and key is what is produced for voices 0 and 1
    ; For voice 2, the frequency is divided by 2 and so the key is produced 1 octave lower
    ;NR03-4 input   key     freq        key
    dw $0782      ;54 00   1040.25 Hz   C6
    dw $0789      ;55 01   1101.45 Hz   C#6
    dw $0790      ;56 02   1170.29 Hz   D6
    dw $0796      ;57 03   1236.53 Hz   D#6
    dw $079C      ;58 04   1310.72 Hz   E6
    dw $07A2      ;59 05   1394.38 Hz   F6
    dw $07A7      ;5A 06   1472.72 Hz   F#6
    dw $07AC      ;5B 07   1560.38 Hz   G6
    dw $07B1      ;5C 08   1659.14 Hz   G#6
    dw $07B5      ;5D 09   1747.63 Hz   A6
    dw $07B9      ;5E 0A   1846.08 Hz   A#6
    dw $07BD      ;5F 0B   1956.30 Hz   B6
    dw $07C1      ;60 0C   2080.51 Hz   C7
    dw $07C4      ;61 0D   2184.53 Hz   C#7
    dw $07C8      ;62 0E   2340.57 Hz   D7
    dw $07CB      ;63 0F   2473.06 Hz   D#7
    dw $07CE      ;64 10   2621.44 Hz   E7
    dw $07D1      ;65 11   2788.77 Hz   F7
    dw $07D3      ;66 12   2912.71 Hz   F#7
    dw $07D6      ;67 13   3120.76 Hz   G7
    dw $07D8      ;68 14   3276.80 Hz   G#7
    dw $07DA      ;69 15   3449.26 Hz   A7
    dw $07DC      ;6A 16   3640.89 Hz   A#7
    dw $07DE      ;6B 17   3855.06 Hz   B7
    dw $07E0      ;6C 18   4096.00 Hz   C8
    dw $07E2      ;6D 19   4369.07 Hz   C#8
    dw $07E4      ;6E 1A   4681.14 Hz   D8
    dw $07E5      ;6F 1B   4854.52 Hz   D#8
    dw $07E7      ;70 1C   5242.88 Hz   E8
    dw $07E8      ;71 1D   5461.33 Hz   F8
    dw $07E9      ;72 1E   5698.78 Hz   F8
    dw $07EB      ;73 1F   6241.52 Hz   G8
    dw $07EC      ;74 20   6553.60 Hz   G#8
    dw $07ED      ;75 21   6898.53 Hz   A8
    dw $07EE      ;76 22   7281.78 Hz   A#8
    dw $07EF      ;77 23   7710.12 Hz   B8
    dw $002C      ;24 24   65.41 Hz     C2
    dw $009C      ;25 25   69.28 Hz     C#2
    dw $0106      ;26 26   73.39 Hz     D2
    dw $016A      ;27 27   77.74 Hz     D#2
    dw $01C9      ;28 28   82.38 Hz     E2
    dw $0222      ;29 29   87.26 Hz     F2
    dw $0276      ;2A 2A   92.43 Hz     F#2
    dw $02C6      ;2B 2B   97.96 Hz     G2
    dw $0311      ;2C 2C   103.78 Hz    G#2
    dw $0358      ;2D 2D   109.96 Hz    A2
    dw $039B      ;2E 2E   116.51 Hz    A#2
    dw $03DA      ;2F 2F   123.42 Hz    B2
    dw $0416      ;30 30   130.81 Hz    C3
    dw $044E      ;31 31   138.55 Hz    C#3
    dw $0483      ;32 32   146.78 Hz    D3
    dw $04B5      ;33 33   155.48 Hz    D#3
    dw $04E4      ;34 34   164.66 Hz    E3
    dw $0511      ;35 35   174.53 Hz    F3
    dw $053B      ;36 36   184.87 Hz    F#3
    dw $0563      ;37 37   195.92 Hz    G3
    dw $0588      ;38 38   207.39 Hz    G#3
    dw $05AC      ;39 39   219.92 Hz    A3
    dw $05CD      ;3A 3A   232.81 Hz    A#3
    dw $05ED      ;3B 3B   246.84 Hz    B3
    dw $060B      ;3C 3C   261.62 Hz    C4
    dw $0627      ;3D 3D   277.11 Hz    C#4
    dw $0641      ;3E 3E   293.23 Hz    D4
    dw $065A      ;3F 3F   310.60 Hz    D#4
    dw $0672      ;40 40   329.33 Hz    E4
    dw $0688      ;41 41   348.60 Hz    F4
    dw $069D      ;42 42   369.22 Hz    F#4
    dw $06B1      ;43 43   391.26 Hz    G4
    dw $06C4      ;44 44   414.78 Hz    G#4
    dw $06D6      ;45 45   439.84 Hz    A4
    dw $06E6      ;46 46   464.79 Hz    A#4
    dw $06F6      ;47 47   492.75 Hz    B4
    dw $0705      ;48 48   522.20 Hz    C5
    dw $0713      ;49 49   553.05 Hz    C#5
    dw $0720      ;4A 4A   585.14 Hz    D5
    dw $072D      ;4B 4B   621.19 Hz    D#5
    dw $0739      ;4C 4C   658.65 Hz    E5
    dw $0744      ;4D 4D   697.19 Hz    F5
    dw $074E      ;4E 4E   736.36 Hz    F#5
    dw $0758      ;4F 4F   780.19 Hz    G5
    dw $0762      ;50 50   829.57 Hz    G#5
    dw $076B      ;51 51   879.68 Hz    A5
    dw $0773      ;52 52   929.59 Hz    A#5
    dw $077B      ;53 53   985.50 Hz    B5

    dw $077B      ;54 54   985.50 Hz    B5 (UNUSED)

    ; $5282
    .KeyFrequencyDeltaTable:
    ; Given a key X, look up the X % $54 entry
    ; The returned value is the delta between the corresponding
    ;   value and the next value in .KeyToFrequencyTable
    ; By looking up the delta, you can multiply it by a fraction to linearly interpolate
    ;   between two notes
    db $07      ;54 00   C5  -> C#5
    db $07      ;55 01   C#5 -> D5 
    db $06      ;56 02   D5  -> D#5
    db $06      ;57 03   D#5 -> E5 
    db $06      ;58 04   E5  -> F5 
    db $05      ;59 05   F5  -> F#5
    db $05      ;5A 06   F#5 -> G5 
    db $05      ;5B 07   G5  -> G#5
    db $04      ;5C 08   G#5 -> A5 
    db $04      ;5D 09   A5  -> A#5
    db $04      ;5E 0A   A#5 -> B5 
    db $04      ;5F 0B   B5  -> C6 
    db $03      ;60 0C   C6  -> C#6
    db $04      ;61 0D   C#6 -> D6 
    db $03      ;62 0E   D6  -> D#6
    db $03      ;63 0F   D#6 -> E6 
    db $03      ;64 10   E6  -> F6 
    db $02      ;65 11   F6  -> F#6
    db $03      ;66 12   F#6 -> G6 
    db $02      ;67 13   G6  -> G#6
    db $02      ;68 14   G#6 -> A6 
    db $02      ;69 15   A6  -> A#6
    db $02      ;6A 16   A#6 -> B6 
    db $02      ;6B 17   B6  -> C7 
    db $02      ;6C 18   C7  -> C#7
    db $02      ;6D 19   C#7 -> D7 
    db $01      ;6E 1A   D7  -> D#7
    db $02      ;6F 1B   D#7 -> E7 
    db $01      ;70 1C   E7  -> F7 
    db $01      ;71 1D   F7  -> F#7
    db $02      ;72 1E   F#7 -> G7 
    db $01      ;73 1F   G7  -> G#7
    db $01      ;74 20   G#7 -> A7 
    db $01      ;75 21   A7  -> A#7
    db $01      ;76 22   A#7 -> B7 
    db $01      ;77 23   B7  -> C8 
    db $70      ;24 24   C1  -> C#1
    db $6A      ;25 25   C#1 -> D1 
    db $64      ;26 26   D1  -> D#1
    db $5F      ;27 27   D#1 -> E1 
    db $59      ;28 28   E1  -> F1 
    db $54      ;29 29   F1  -> F#1
    db $50      ;2A 2A   F#1 -> G1 
    db $4B      ;2B 2B   G1  -> G#1
    db $47      ;2C 2C   G#1 -> A1 
    db $43      ;2D 2D   A1  -> A#1
    db $3F      ;2E 2E   A#1 -> B1 
    db $3C      ;2F 2F   B1  -> C2 
    db $38      ;30 30   C2  -> C#2
    db $35      ;31 31   C#2 -> D2 
    db $32      ;32 32   D2  -> D#2
    db $2F      ;33 33   D#2 -> E2 
    db $2D      ;34 34   E2  -> F2 
    db $2A      ;35 35   F2  -> F#2
    db $28      ;36 36   F#2 -> G2 
    db $25      ;37 37   G2  -> G#2
    db $24      ;38 38   G#2 -> A2 
    db $21      ;39 39   A2  -> A#2
    db $20      ;3A 3A   A#2 -> B2 
    db $1E      ;3B 3B   B2  -> C3 
    db $1C      ;3C 3C   C3  -> C#3
    db $1A      ;3D 3D   C#3 -> D3 
    db $19      ;3E 3E   D3  -> D#3
    db $18      ;3F 3F   D#3 -> E3 
    db $16      ;40 40   E3  -> F3 
    db $15      ;41 41   F3  -> F#3
    db $14      ;42 42   F#3 -> G3 
    db $13      ;43 43   G3  -> G#3
    db $12      ;44 44   G#3 -> A3 
    db $10      ;45 45   A3  -> A#3
    db $10      ;46 46   A#3 -> B3 
    db $0F      ;47 47   B3  -> C4 
    db $0E      ;48 48   C4  -> C#4
    db $0D      ;49 49   C#4 -> D4 
    db $0D      ;4A 4A   D4  -> D#4
    db $0C      ;4B 4B   D#4 -> E4 
    db $0B      ;4C 4C   E4  -> F4 
    db $0A      ;4D 4D   F4  -> F#4
    db $0A      ;4E 4E   F#4 -> G4 
    db $0A      ;4F 4F   G4  -> G#4
    db $09      ;50 50   G#4 -> A4 
    db $08      ;51 51   A4  -> A#4
    db $08      ;52 52   A#4 -> B4 
    db $07      ;53 53   B4  -> C5 

    ; $52D6
_snd_SetupPortamento:
    ; Inputs:
    ;   bc = KeyCents
    ;   de = Cycles
    ;   a = Voice | Abs/Rel flag (?bug - they overlap - I think the flag is supposed to be in bit 7 instead of bit 0 like it is right now)
        sla a ;put bit 7 into the carry flag
        push af
        bit 1, a
        jr z, .Voice0or2 ;A really dumb way of getting wSnd_Voice_FrequencyStatus + Voice
            ld hl, wSnd_Voice_FrequencyStatus+1 ;Voice 1
            jr .VoiceFound
        .Voice0or2:
            bit 2, a
            jr z, .Voice0
                ld hl, wSnd_Voice_FrequencyStatus+2
                jr .VoiceFound
        .Voice0:
            ld hl, wSnd_Voice_FrequencyStatus

    .VoiceFound:
        res 0, [hl]
        res 2, [hl]
        res 3, [hl]
        res 4, [hl]

        ld hl, wSnd_Voice_Port_Cycles
        add l
        ld l, a
        ld a, h
        adc $00
        ld h, a
        ld a, e
        ld [hl+], a
        ld [hl], d ;Store cycles into wSnd_Voice_Port_Cycles

        pop af
        ld d, $00
        ld e, a
        jr nc, .Abs ;bug - this assumes that the flag is placed in bit 7 but it's actually in bit 0...
            push bc ;Rel
            ld hl, wSnd_Voice_KeyCents
            add hl, de
            ld a, c
            add [hl]
            ld c, a
            ld a, b
            inc hl
            adc [hl] ;CurrentKeyCents + InputKeyCents

            ld hl, wSnd_Voice_Port_TargetKeyCents+1
            add hl, de
            ld [hl-], a
            ld [hl], c ;Store the absolute target value into wSnd_Voice_Port_TargetKeyCents
            pop bc
            jr .FinishedStoringTargetKeyCents
        .Abs:
            ld hl, wSnd_Voice_Port_TargetKeyCents
            add hl, de
            ld a, c
            ld [hl+], a
            ld [hl], b

            ld hl, wSnd_Voice_KeyCents
            add hl, de
            ld a, c
            sub [hl]
            ld c, a
            ld a, b
            inc hl
            sbc [hl]
            ld b, a ;Store the delta into bc
    .FinishedStoringTargetKeyCents:
        ; At this point, the delta between the KeyCents and the TargetKeyCents is in bc
        ld hl, wSnd_Voice_Port_Cycles
        add hl, de
        bit 7, b
        jr nz, .NegativeDelta
            ld a, [hl+] ;positive delta
            push bc
            ld b, [hl]
            ld c, a
            pop hl
            call _snd_hl_Div_bc_preserve_de ;Delta/Cycles
            jr .FinishedCalculatingDeltaPerCycle
        .NegativeDelta:
            ld a, c
            cpl
            add $01
            ld c, a
            ld a, b
            cpl
            adc $00
            ld b, a ;-bc

            ld a, [hl+]
            push bc
            ld b, [hl]
            ld c, a

            pop hl
            call _snd_hl_Div_bc_preserve_de
            ld a, l
            cpl
            add $01
            ld l, a
            ld a, h
            cpl
            adc $00
            ld h, a  ;-(-Delta/Cycles)
    .FinishedCalculatingDeltaPerCycle:
        ld a, l
        ld b, h
        ld hl, wSnd_Voice_Port_DeltaKeyCentsPerCycle
        add hl, de
        ld [hl+], a
        ld [hl], b

        srl e
        ld hl, wSnd_Voice_FrequencyStatus
        add hl, de
        set 2, [hl]
        ret

    ; $536D
_snd_SetupVibrato:
    ; Inputs:
    ;   de = DeltaKeyCentsPerCycle
    ;   b = cycles
    ld a, b
    ld b, $00
    sla c
    ld hl, wSnd_Voice_Vibrato_CycleLength
    add hl, bc
    ld [hl], a

    ld hl, wSnd_Voice_Vibrato_CycleCounter
    add hl, bc
    srl a
    jr nz, .NotZero
        inc a ;Make sure the value is at least 1
    .NotZero:
    ld [hl], a ;wSnd_Voice_Vibrato_CycleLength//2 (so that the vibrato effect is centered over 0)

    ld hl, wSnd_Voice_Vibrato_DeltaKeyCentsPerCycle
    add hl, bc
    ld a, e
    ld [hl+], a
    ld [hl], d

    ld hl, wSnd_Voice_Vibrato_CurrentKeyCentOffset
    add hl, bc
    xor a
    ld [hl+], a
    ld [hl], a

    srl c
    ld hl, wSnd_Voice_FrequencyStatus
    add hl, bc
    ld a, [hl]
    and $E2 ;disable 0, 2, 3, 4
    or $01
    ld [hl], a
    ret

    ; $539C
_snd_SetupPitchsweep:
    ; Inputs:
    ;   bc = DeltaKeyCentsPerCycle
    ;   de = KeyCent target
    ;   a = Voice (bit 0-1) | SweepBool (bit 7)
        bit 7, a
        res 7, a
        push af
        push de
        ld hl, wSnd_Voice_FrequencyStatus
        ld d, $00
        ld e, a
        add hl, de
        ld a, [hl]
        jr nz, .FlagTrue
            and $F2 ;FlagFalse - disable bits 0, 2, 3
            ld [hl], a
            jr .DoneResettingStatusA
        .FlagTrue:
            and $EA ; disable bits 0, 2, 4
            ld [hl], a
            ld a, e
            add $03 ;Add 3 to "voice"
            ld e, a
    .DoneResettingStatusA:
        sla e
        ld hl, wSnd_Voice_Sweep_DeltaKeyCentsPerCycle
        add hl, de
        ld a, c
        ld [hl+], a
        ld [hl], b

        pop bc
        ld hl, wSnd_Voice_Sweep_TargetKeyCents
        add hl, de
        ld a, c
        ld [hl+], a
        ld [hl], b

        ld hl, wSnd_Voice_Sweep_CurrentKeyCentOffset
        add hl, de
        xor a
        ld [hl+], a
        ld [hl], a

        pop af
        ld e, a
        ld hl, wSnd_Voice_FrequencyStatus
        add hl, de
        jr nz, .FlagTrue2
            set 3, [hl] ;FlagFalse (Sweep 0)
            ret
        .FlagTrue2:
            set 4, [hl] ;Sweep 1
            ret

    ; $53DE
_snd_HandleModulation:
    ld bc, $0003
    ld hl, wSnd_Voice_FrequencyStatus+2
    .LoopVoices:
        bit 2, [hl]
        jr nz, .HandlePortamento
        bit 0, [hl]
        jr nz, .HandleVibrato
        bit 3, [hl]
        jp nz, .HandlePitchsweep0
    .CheckSweep1:
        bit 4, [hl]
        jp nz, .HandlePitchsweep1
    .SetupNextVoiceCheck:
        dec hl
        dec c
        jr nz, .LoopVoices
    ret

    .HandlePortamento:
        push bc
        push hl
        dec c
        sla c
        set 5, [hl] ;Update freq

        ld hl, wSnd_Voice_Port_Cycles+1
        add hl, bc
        ld a, [hl-]
        or [hl]
        jr nz, .NotLastCycle
            ld hl, wSnd_Voice_Port_TargetKeyCents
            add hl, bc
            ld a, [hl+]
            ld d, [hl]
            ld hl, wSnd_Voice_KeyCents
            add hl, bc
            ld [hl+], a
            ld [hl], d ;wSnd_Voice_KeyCents = wSnd_Voice_Port_TargetKeyCents
            pop hl
            pop bc
            res 2, [hl] ;Disable Portamento
            jr .SetupNextVoiceCheck
        .NotLastCycle:
            dec [hl]
            ld a, [hl+]
            cp $FF
            jr nz, .NoCarry
                dec [hl] ;If lower byte of Cycles carries, then decrease the upper byte

            .NoCarry:
            ld hl, wSnd_Voice_Port_DeltaKeyCentsPerCycle
            add hl, bc
            ld a, [hl+]
            ld d, [hl]
            ld e, a
            ld hl, wSnd_Voice_KeyCents
            add hl, bc
            ld a, [hl]
            add e
            ld [hl+], a
            ld a, [hl]
            adc d
            ld [hl], a ;wSnd_Voice_KeyCents += wSnd_Voice_Port_DeltaKeyCentsPerCycle

            pop hl
            pop bc
            jr .SetupNextVoiceCheck

    .HandleVibrato:
        push bc
        push hl
        set 5, [hl] ;Update freq

        dec c
        sla c
        ld hl, wSnd_Voice_Vibrato_DeltaKeyCentsPerCycle
        add hl, bc
        ld a, [hl+]
        ld d, [hl]
        ld e, a

        ld hl, wSnd_Voice_Vibrato_CurrentKeyCentOffset
        add hl, bc
        ld a, [hl]
        add e
        ld [hl+], a
        ld a, [hl]
        adc d
        ld [hl-], a
        ld d, a
        ld a, [hl] ;wSnd_Voice_Vibrato_CurrentKeyCentOffset += wSnd_Voice_Vibrato_DeltaKeyCentsPerCycle

        ld hl, wSnd_Voice_Modulation_DeltaKeyCents
        add hl, bc
        add [hl]
        ld [hl+], a
        ld a, [hl]
        adc d
        ld [hl], a ;wSnd_Voice_Modulation_DeltaKeyCents += wSnd_Voice_Vibrato_CurrentKeyCentOffset

        ld hl, wSnd_Voice_Vibrato_CycleCounter
        add hl, bc
        dec [hl]
        jr nz, .DontChangeDirection
            ld e, l ;Flip the vibrato effect from positive to negative and vice versa
            ld d, h
            ld hl, wSnd_Voice_Vibrato_CycleLength
            add hl, bc
            ld a, [hl]
            ld [de], a ;wSnd_Voice_Vibrato_CycleCounter = wSnd_Voice_Vibrato_CycleLength

            ld hl, wSnd_Voice_Vibrato_DeltaKeyCentsPerCycle
            add hl, bc
            ld a, [hl]
            cpl
            add $01
            ld [hl+], a
            ld a, [hl]
            cpl
            adc $00
            ld [hl], a ;wSnd_Voice_Vibrato_DeltaKeyCentsPerCycle *= -1

        .DontChangeDirection:
            pop hl
            pop bc
            jp .SetupNextVoiceCheck


    .HandlePitchsweep0:
        push bc
        push hl
        dec c
        sla c
        call _snd_HandlePitchsweep
        ld hl, wSnd_Voice_Modulation_DeltaKeyCents
        add hl, bc
        ld a, [hl]
        add e
        ld [hl+], a
        ld a, [hl]
        adc d
        ld [hl], a

        pop hl
        pop bc
        set 5, [hl]
        jp .CheckSweep1


    .HandlePitchsweep1:
        push bc
        push hl
        ld a, c
        add $03
        sla a
        ld c, a ;Use Voice+3
        call _snd_HandlePitchsweep

        ld a, c
        sub $06
        ld c, a
        ld hl, wSnd_Voice_Modulation_DeltaKeyCents
        add hl, bc
        ld a, [hl]
        add e
        ld [hl+], a
        ld a, [hl]
        adc d
        ld [hl], a

        pop hl
        pop bc
        set 5, [hl]
        jp .SetupNextVoiceCheck

    ; $54B6
_snd_HandlePitchsweep:
    ; Inputs:
    ;   c = voice (0-5), where voices 3-5 indicate Sweep 1
    ; Outputs:
    ;   de = value to put into wSnd_Voice_Modulation_DeltaKeyCents
    ld hl, wSnd_Voice_Sweep_DeltaKeyCentsPerCycle
    add hl, bc
    ld a, [hl+]
    ld d, [hl]
    ld e, a
    ld hl, wSnd_Voice_Sweep_CurrentKeyCentOffset
    add hl, bc
    ld a, [hl]
    add e
    ld [hl+], a
    ld e, a
    ld a, [hl]
    adc d
    ld [hl], a ;de = wSnd_Voice_Sweep_CurrentKeyCentOffset += wSnd_Voice_Sweep_DeltaKeyCentsPerCycle
    ld d, a

    ld hl, wSnd_Voice_Sweep_TargetKeyCents
    add hl, bc
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    bit 7, h
    jr nz, .CheckNegativeSweep
        call _snd_cp_hl_de ;CheckPositiveSweep
        ret nc             ;cp Target, Current
        .ResetSweep:
            ld a, e
            sub l
            ld e, a
            ld a, d
            sbc h
            ; bug - there should be ld d, a here to return de to the parent function
            ld hl, wSnd_Voice_Sweep_CurrentKeyCentOffset+1
            add hl, bc
            ld [hl-], a
            ld [hl], e ;wSnd_Voice_Sweep_CurrentKeyCentOffset -= wSnd_Voice_Sweep_TargetKeyCents
            ret
    .CheckNegativeSweep:
        dec hl
        call _snd_cp_hl_de
        ret c            ;cp Target, Current
        inc hl
        jr .ResetSweep

    ; $54EC
_snd_ADSR_LoadData:
    ; Inputs:
    ;   c = voice
    ;   hl = pointer to ADSR data
    ld a, [hl+]
    ld e, a
    ld a, [hl+]
    ld d, a
    push de

    ld e, l
    ld d, h
    ld b, $00
    sla c
    ld hl, wSnd_Voice_Envelope_ADSRPointer ;pointer to decay
    add hl, bc
    ld a, e
    ld [hl+], a
    ld [hl], d

    pop de
    ld hl, wSnd_Voice_Envelope_DeltaVolumeCents ;attack delta
    add hl, bc
    ld a, e
    ld [hl+], a
    ld [hl], d

    ld hl, wSnd_Voice_Envelope_VolumeCents ;0
    xor a
    ld [hl+], a
    ld [hl], a

    srl c
    ld hl, wSnd_Voice_EnvelopeVolume ;0
    add hl, bc
    ld [hl], a

    ld hl, wSnd_Voice_VolumeStatus
    add hl, bc
    ld a, [hl]
    and $F0
    or $21 ;bit 5 (update volume) and bit 0
    ld [hl], a
    ret

    ; $551E
_snd_HandleEnvelopes:
        ld de, $0004
        ld hl, wSnd_Voice_VolumeStatus+3
    .LoopVoices:
            ld a, [hl]
            and $0B ;bit 3, 1, 0
            jr nz, .HasEnvelope
        .SetupNextLoopCheck:
            dec hl
            dec e
            jr nz, .LoopVoices
        ret

    .HasEnvelope:
        push hl
        push de
        dec e
        sla e
        bit 1, a
        jr nz, .HandleADSRDecay
        bit 3, a
        jp nz, .HandleEnvelopeDescendingADSRRelease
            ;HandleEnvelopeAscending/ADSR_Attack
                ld hl, wSnd_Voice_Envelope_VolumeCents+1
                add hl, de
                ld a, [hl]
                cp $0F
                jr nc, .ReachedF_OrHigher

            .ApplyDelta:
                ld hl, wSnd_Voice_Envelope_DeltaVolumeCents
                add hl, de
                ld a, [hl+]
                ld b, [hl]

                ld hl, wSnd_Voice_Envelope_VolumeCents
                add hl, de
                add [hl]
                ld [hl+], a
                ld a, [hl]
                adc b
                ld [hl], a

                srl e
                ld hl, wSnd_Voice_EnvelopeVolume
                add hl, de
                ld c, [hl]
                ld [hl], a
                cp c
                jr z, .PopAndSetupNextLoopCheck1
                    pop de
                    pop hl
                    set 5, [hl] ;Only set bit 5 if the EnvelopeVolume actually changes
                    jr .SetupNextLoopCheck
                .PopAndSetupNextLoopCheck1:
                    pop de
                    pop hl
                    jr .SetupNextLoopCheck

            .ReachedF_OrHigher:
                ld a, $0F
                ld [hl-], a
                ld [hl], $00 ;Set to max volume

                srl e
                ld hl, wSnd_Voice_EnvelopeVolume
                add hl, de
                ld a, [hl]
                ld [hl], $0F

                ld hl, wSnd_Voice_VolumeStatus
                add hl, de
                cp $0F
                jr z, .SkipSetBit5
                    set 5, [hl] ;Only set if the EnvelopeVolume actually changes
                .SkipSetBit5:
                ld a, [hl]
                and $F0
                bit 4, a
                jr z, .PrepareDecay
                    res 4, a ;Envelope_Ascending: reset and end. Bit 4 is not reset (garbage data)
                    ld [hl], a
                    pop de
                    pop hl
                    jr .SetupNextLoopCheck
                .PrepareDecay:
                    or $02 ;Set bit 1
                    ld [hl], a

                    sla e
                    ld hl, wSnd_Voice_Envelope_ADSRPointer
                    add hl, de
                    ld a, [hl+]
                    ld h, [hl]
                    ld l, a
                    ld a, [hl+]
                    ld b, [hl] ;Decay

                    ld hl, wSnd_Voice_Envelope_DeltaVolumeCents
                    add hl, de
                    ld [hl+], a
                    ld [hl], b ;Load Decay into Delta

                    ld hl, wSnd_Voice_Envelope_ADSRPointer
                    add hl, de
                    ld a, [hl]
                    add $02
                    ld [hl+], a
                    ld a, [hl]
                    adc $00
                    ld [hl], a

    .HandleADSRDecay:
            ld hl, wSnd_Voice_Envelope_ADSRPointer
            add hl, de
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            ld a, [hl]  ;Sustain

            ld hl, wSnd_Voice_Envelope_VolumeCents+1
            add hl, de
            ld b, [hl]
            bit 7, b
            jr nz, .ReachedSustainOrLower ;Negative number
            cp b               ;Jump if NOT lower than Sustain level
                jr c, .ApplyDelta  ;Pass if lower than Sustain level
            .ReachedSustainOrLower:
                ld [hl-], a
                ld [hl], $00 ;Set to Sustain
                ld c, a

                ld hl, wSnd_Voice_Envelope_ADSRPointer
                add hl, de
                ld a, [hl+]
                ld h, [hl]
                ld l, a
                inc hl
                ld a, [hl+]
                ld b, [hl] ;Decay

                ld hl, wSnd_Voice_Envelope_DeltaVolumeCents
                add hl, de
                ld [hl+], a
                ld [hl], b ;Store Decay

                srl e
                ld hl, wSnd_Voice_EnvelopeVolume
                add hl, de
                ld a, [hl]
                ld [hl], c ;Set to Sustain

                ld hl, wSnd_Voice_VolumeStatus
                add hl, de
                cp c
                jr z, .Skip2SetBit5
                    set 5, [hl]
                .Skip2SetBit5:
                ld a, [hl]
                and $F0
                or $04 ;set bit 2
                ld [hl], a
            .PopAndSetupNextLoopCheck2:
                pop de
                pop hl
                jp .SetupNextLoopCheck

    .HandleEnvelopeDescendingADSRRelease: ;Envelope_Descending or ADSR_Release
            ld hl, wSnd_Voice_Envelope_DeltaVolumeCents
            add hl, de
            ld a, [hl+]
            ld b, [hl]
            ld c, a

            ld hl, wSnd_Voice_Envelope_VolumeCents
            add hl, de
            ld a, [hl]
            add c
            ld [hl+], a
            ld c, a
            ld a, [hl]
            adc b
            ld [hl], a
            ld b, a

            srl e
            ld hl, wSnd_Voice_EnvelopeVolume
            add hl, de
            bit 7, a
            jr nz, .ReachedZeroOrLower
            or c ;bc = 0
            jr z, .ReachedZeroOrLower
                ld a, [hl]
                ld [hl], b
                cp b
                jr z, .PopAndSetupNextLoopCheck2 ;Don't set 5 if the EnvelopeVolume hasn't changed
                pop de
                pop hl
                set 5, [hl] ;Only set 5 if the EnvelopeVolume has actually changed
                jp .SetupNextLoopCheck
            .ReachedZeroOrLower:
                ld c, [hl]
                ld [hl], $00
                pop de
                pop hl
                ld a, [hl]
                and $F0
                ld [hl], a
                ld a, c
                and a
                jp z, .SetupNextLoopCheck

                set 5, [hl]
                jp .SetupNextLoopCheck

    ; $5632
_snd_CalculateOutputVolume:
    ; a, e = two volumes
    ;
    ; In theory, these two numbers are multiplied together (see the table below), but
    ;   the multiplication table isn't perfectly symmetrical
    ; In the code, a and e are used interchangeably however.
    ;
    ; The true volume is a multiplication of 4 different volumes:
    ;   (1) wSnd_Voice_Velocity = wSnd_SFXSongVolume * TargetVolume (e.g. SETVOLUME)
    ;   (2) wSnd_Voice_InternalVolume = wSnd_Voice_Velocity * wSnd_Voice_EnvelopeVolume
    ;   (3) wSnd_Voice_InternalVolume is multiplied by hardware volume rNR50 (snd_SetMasterVolume)
    swap a
    or e
    srl a
    ld e, a
    ld d, $00
    ld hl, ._snd_VolumeTable
    jr c, .TakeLowerNibble
        add hl, de
        ld a, [hl]
        swap a
        and $0F
        ret
    .TakeLowerNibble:
        add hl, de
        ld a, [hl]
        and $0F
        ret

        ; $564B
    ._snd_VolumeTable:
        ; <-> e = Target Volume (16 nibbles)
        ; ^v a = Volume from wSnd_SFXSongVolume (16 rows)
        ; It's almost but not perfectly symmetrical
        ; It's almost equivalent to a multiplication table of
        ;  ( (a+1)*15/16 * (e+1)*15/16 ) / 15
        db $00, $00, $00, $00, $00, $00, $00, $00
        db $00, $00, $00, $00, $01, $11, $11, $11
        db $00, $00, $01, $11, $11, $11, $22, $22
        db $00, $01, $11, $11, $22, $22, $33, $33
        db $00, $01, $11, $22, $22, $33, $33, $44
        db $00, $11, $12, $22, $33, $34, $44, $55
        db $00, $11, $22, $23, $34, $44, $55, $66
        db $00, $11, $22, $33, $44, $55, $66, $77
        db $01, $12, $23, $34, $45, $56, $77, $78
        db $01, $12, $23, $44, $55, $67, $78, $89
        db $01, $12, $33, $45, $56, $77, $89, $9A
        db $01, $22, $34, $45, $67, $78, $9A, $AB
        db $01, $22, $34, $56, $67, $89, $AA, $BC
        db $01, $23, $34, $56, $78, $9A, $AB, $CD
        db $01, $23, $45, $67, $78, $9A, $BC, $DE
        db $01, $23, $45, $67, $89, $AB, $CD, $EF

    ; $56CB
_snd_HandleLowQSample:
        ld hl, wSnd_Sample_Length
        ld a, [hl]
        sub $01
        ld [hl+], a
        ld c, a
        ld a, [hl]
        sbc $00
        ld [hl], a
        or c
        jr z, .EndOfSample ;dec by 1 and check if end of sample

            ld hl, wSnd_Sample_Address
            ld a, [hl+]
            ld h, [hl]
            ld l, a
            call snd_TransferSampleBlockRestart
            bit 7, h
            jr z, .SkipSampleCrossesBankBoundary
                ld h, $40
                ld a, [wSnd_Sample_Bank]
                inc a
                ld [wSnd_Sample_Bank], a
        .SkipSampleCrossesBankBoundary:
            ld a, l
            ld b, h
            ld hl, wSnd_Sample_Address
            ld [hl+], a
            ld [hl], b ;Transfer the next 32 nibbles and end
            ret

    .EndOfSample:
        ld hl, wSnd_Voice_SampleStatus
        res 7, [hl]
        ld c, $02
        jp _snd_VoiceOff

    ; $5702
snd_StartSong::
    ; Purpose:
    ;   This function will start song playback.
    ; Inputs:
    ;   a = ID of the song to start
    ; Output:
    ;   None.
    ; Remarks:
    ;   The ID you need to pass to this function was created by MUCONV.exe
    ;   when you converted the project. Please use only the symbolic names
    ;   assigned by MUCONV, as the numeric values behind them are likely to
    ;   change when you convert the project again.
    ld hl, sdp_NumberOfSongs
    cp [hl]
    ret nc ;ret if the ID is out of bounds

    ld d, $00
    ld e, a
    ld h, $00
    ld l, a
    sla l
    rl h
    add hl, de
    ld e, l
    ld d, h
    ld hl, sdp_SongTableAddress
    ld a, [hl+]
    ld b, [hl]
    ld c, a
    ld hl, snd_ProjectData
    add hl, bc
    add hl, de ; Find the address of sdp_SongLookupTable and offset by 3*ID

    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    ld a, [wSnd_BaseBank]
    add b
    ld bc, $0084 ;skip the program header info ($84 bytes)
    add hl, bc
    ld [wSnd_sss_Bank], a
    ld a, h
    ld [wSnd_sss_SongHeaderAddress+1], a
    ld a, l
    ld [wSnd_sss_SongHeaderAddress], a ;Store the bank and address

    xor a
    ld [wSnd_sss_Status], a ;reset

    ld hl, wSnd_sss_Track_HeaderPointer
    ld bc, wSnd_SequencerStateEnd - wSnd_sss_Track_HeaderPointer ;$0045
    .Erase45Bytes:
        xor a
        ld [hl+], a
        dec bc
        ld a, c
        or b
        jr nz, .Erase45Bytes

    call snd_ChangeSongSpeed.SetDefaultSpeed
    call snd_sss_SetupTracks
    ld a, %11100100
    ld [wSnd_sss_TrackToVoiceMap], a
    ld hl, wSnd_sss_Status
    set 7, [hl]
    ret

    ; $5759
snd_SongActive::
    ; Purpose:
    ;   This function will return whether or not a song is currently playing.
    ;   The main use of this function, is to determine the end of a 'one-shot'
    ;   song, like a jingle.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   A 0 = Not playing, 1 = playing
    ;   ZF 0 = Playing, 1 = not playing
    ;   
    ; Remarks:
    ;   The song itself, is certain to be at its end, when this function tells you that
    ;   no song is playing. This does not necessarily mean that no further sound
    ;   can be heard, since the sound macro has control over the sound. If a
    ;   note is fading out, the song will have been finished, and you could
    ;   prematurely end the sound synthesis. The musician should make sure
    ;   that, in case of a 'one-shot' song, the last note is not a note, but a
    ;   dummy program change that allows all notes sufficient time to really fade
    ;   out.
    ;
    ; A stopped or paused song still returns 1. 0 is only returned if the song
    ; is played until the end.
    ld a, [wSnd_sss_Status]
    and $0F ;If any of the 4 tracks are active
    jr nz, .Return1
    ld a, [wSnd_sss_PatternStatus]
    and a
    jr nz, .Return1
        xor a
        ret
    .Return1:
        ld a, $01
        ret

    ; $576B
snd_ChangeSongSpeed::
    ; Purpose:
    ;   This function will change the play back speed of the currently playing
    ;   song. This can be used for instance, in a game to indicate that time is
    ;   running out.
    ;   
    ; Inputs:
    ;   BC Speed scale factor based on $0100 being 1.0.
    ;       If zero is passed, the song's default speed is restored.
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   To speed the song up by 50%, set BC to $0180. To undo the 50% speed
    ;   increase, set BC to $00ab (not $0080).
    ; Example:
    ;   • current speed = $0100
    ;   • snd_ChangeSongSpeed with $0180 yields $0180 as new speed
    ;   • snd_ChangeSongSpeed with $0080 would yield $00c0, which is half
    ;       of $0180. To get back to $0100, you need to specify
    ;       ($0100/$0180)<<8, or $00ab for this example.
    ;
    ; wSnd_sss_Speed by default is bpm*1.7
    ld a, c
    or b
    jr z, .SetDefaultSpeed ;If 0, set default speed
        ld hl, wSnd_sss_Speed
        ld a, [hl+]
        ld h, [hl]
        ld l, a
        call _snd_hl_Times_bc ;Otherwise, multiply hl*bc/$100
        ld a, h
        ld hl, wSnd_sss_Speed
        ld [hl+], a
        ld [hl], c
        ret
    .SetDefaultSpeed:
        call snd_sss_GetBPM
        ld c, l
        ld b, h
        ld a, 102
        call _snd_bc_Times_a
        ld bc, 60
        call _snd_hl_Div_bc_preserve_de ;Speed*102/60, or Speed*1.7.
        ld a, l                         ;The more exact theoretical value would be $1800/60 seconds/59.73 Hz, i.e. 1.71438138289
        ld [wSnd_sss_Speed], a
        ld a, h
        ld [wSnd_sss_Speed+1], a
        ret

    ; $5798
snd_PauseSong::
    ; Purpose:
    ;   This function will pause a song that is currently playing. It can be
    ;   resumed at a later time, by calling snd_ResumeSong. You can also save
    ;   the state of a paused song into a user supplied buffer and play another
    ;   song. Then, restore the buffered state and resume the first song (to play
    ;   a jingle for instance), by calling the state functions snd_GetStateSize,
    ;   snd_SaveState and snd_RestoreState.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   None.
        ld hl, wSnd_sss_Status
        bit 7, [hl]
        ret z
        set 4, [hl]
    ; $57A0
snd_StopSong::
    ; Purpose:
    ;   This function will stop any song that is currently playing.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   None.
    ;   
    ; Remarks:
    ;   The song is stopped immediately, and cannot be resumed. It will need to
    ;   be started again by calling snd_StartSong.
        ld hl, wSnd_sss_Status
        res 7, [hl]
        ld hl, wSnd_Voice_FrequencyStatus
        xor a
        .LoopVoices:
            bit 1, [hl] ;Check if SoundMacro comes from SFX or Song
            inc hl
            call z, .TurnOff ;Turn off if comes from song
            inc a
            cp $04
            jr nz, .LoopVoices
        ret

    ; $57B5
    .TurnOff:
        push af
        push hl
        ld [wSnd_SelectedVoice_Index], a
        ld c, a
        ld b, $00
        sla c
        ld hl, wSnd_Voice_MacroReadAddress
        add hl, bc
        xor a
        ld [hl+], a
        ld [hl], a
        call _snd_KeyOff_SelectedTrack
        call _snd_MacroOp_0D_VOICE_OFF
        pop hl
        pop af
        ret

    ; $57CF
snd_ResumeSong::
    ; Purpose:
    ;   This function will resume a song that was paused. You can also save the
    ;   state of a paused song into a user supplied buffer and play another song.
    ;   Then, restore the buffered state and resume the first song (to play a
    ;   jingle for instance), by calling the state functions snd_GetStateSize,
    ;   snd_SaveState and snd_RestoreState.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   None.
    ;
    ; Actually returns a=1 if success or a=0 if invalid command
    xor a
    ld hl, wSnd_sss_Status
    bit 4, [hl]
    ret z
    res 4, [hl]
    set 7, [hl]
    ld a, $01
    ret

    ; $57DD
snd_GetStateSize::
    ; Purpose:
    ;   This function returns the size of the buffer you need, to provide in calls to
    ;   the functions snd_SaveState and snd_RestoreState.
    ;   
    ; Inputs:
    ;   None.
    ;   
    ; Output:
    ;   A   Size, in bytes, of the state buffer
    ;   
    ; Remarks:
    ;   Call this function just once, during the course of your game development.
    ;   Once you have determined the size of the buffer for this particular
    ;   version of MusyX you do not need to do it again.
    ld a, wSnd_SequencerStateEnd-wSnd_SequencerStateStart ;$52
    ret

    ; $57E0
snd_SaveState::
    ; Purpose:
    ;   This function will backup the current state of the sequencer, for a paused
    ;   song. After the state is secured, you can play another song (i.e., a jingle),
    ;   then restore the state and resume the original song.
    ;   
    ; Inputs:
    ;   C   Size of the user state buffer (for verification purposes)
    ;   HL  Address of the user state buffer to store the state in
    ;   
    ; Output:
    ;   None.
    ld a, wSnd_SequencerStateEnd-wSnd_SequencerStateStart
    cp c
    ret nz
    ld de, wSnd_SequencerStateStart
    .CopyLoop:
        ld a, [de]
        inc de
        ld [hl+], a
        dec c
        jr nz, .CopyLoop
    ret

    ; $57EE
snd_RestoreState::
    ; Purpose:
    ;   This function will restore a previously buffered sequencer song state.
    ; 
    ; Inputs:
    ;   C   Size of the user state buffer (for verification purposes)
    ;   HL  Address of the user state buffer to restore the state from
    ; 
    ; Output:
    ;   None.
    ld a, wSnd_SequencerStateEnd-wSnd_SequencerStateStart
    cp c
    ret nz
    ld de, wSnd_SequencerStateStart
    .CopyLoop:
        ld a, [hl+]
        ld [de], a
        inc de
        dec c
        jr nz, .CopyLoop
    ret

    ; $57FC
_snd_HandleSong:
    ; 
    ; Loads new patterns as needed
        ld a, [wSnd_sss_Status]
        bit 7, a
        ret z ;Abort if song not playing

        ld a, [wSnd_sss_Speed]
        ld c, a
        ld a, [wSnd_sss_Speed+1]
        ld b, a
        call _snd_HandleNoteOffs
        call _snd_HandleNoteOns
        ld hl, wSnd_sss_Track_PatternStartTimer
        xor a

    .LoopTracks: ;Loops, checking for the start of patterns
        ld [wSnd_Temp_9], a ;Track loop
        ld de, _snd_PatternStatusBitmap_PatternEnabled
        add e
        ld e, a
        ld a, d
        adc $00
        ld d, a
        ld a, [de]
        ld e, a
        ld a, [wSnd_sss_Status]
        and e
        jr z, .PrepareNextLoopIncHL ;Track is disabled

            ld a, [hl]
            sub c
            ld [hl+], a
            ld d, a
            ld a, [hl]
            sbc b
            ld [hl+], a
            ld e, a
            ld a, [hl]
            sbc $00
            ld [hl+], a ;wSnd_sss_Track_PatternStartTimer -= SongSpeed

            bit 7, a
            jr nz, .PrepareTrackPattern
            or d
            or e
            jr nz, .PrepareNextLoop

            .PrepareTrackPattern: ;If timer is <= 0
                push hl
                push bc
                ld hl, wSnd_sss_Track_PatternIndex
                ld a, [wSnd_Temp_9]
                ld c, a
                ld b, $00
                add hl, bc
                ld a, [hl] ;Get pattern index

                cp $FE
                jr z, .HandleLoopPattern
                cp $FF
                jr z, .HandleEndOfTrack
                ;HandleNormalBlock
                    ld c, a
                    ld a, [wSnd_Temp_9]
                    call _snd_SetupNewPattern
                    ld a, [wSnd_Temp_9]
                    call snd_sss_SetupNextTrackPattern
                    jr .CheckIfDonePreparingTrackPatterns

                .HandleLoopPattern:
                    ld a, [wSnd_Temp_9]
                    call snd_sss_HandleLoopPattern

                .CheckIfDonePreparingTrackPatterns:
                    pop bc
                    pop hl
                    ld d, h
                    ld e, l
                    dec de
                    ld a, [de]
                    bit 7, a
                    jr nz, .PrepareTrackPattern ;If wSnd_sss_Track_PatternStartTimer is still less than 0, then load another pattern!
                    and a
                    jr nz, .PrepareNextLoop
                    dec de
                    ld a, [de]
                    and a
                    jr nz, .PrepareNextLoop
                    dec de
                    ld a, [de]
                    and a
                    jr nz, .PrepareNextLoop ; If wSnd_sss_Track_PatternStartTimer > 0, then end
                    jr .PrepareTrackPattern ; If wSnd_sss_Track_PatternStartTimer = 0, then  load another pattern!

                .HandleEndOfTrack:
                    ld a, [wSnd_Temp_9]
                    ld c, a
                    ld b, $00
                    ld hl, _snd_PatternStatusBitmap_PatternEnabled
                    add hl, bc
                    ld a, [hl]
                    cpl
                    ld b, a
                    ld a, [wSnd_sss_Status]
                    and b
                    ld [wSnd_sss_Status], a ;Disable status for this track
                    jr .Self
                    .Self:
                    pop bc
                    pop hl
                    jr .PrepareNextLoop

    .PrepareNextLoopIncHL:
        ld de, $0003
        add hl, de

    .PrepareNextLoop:
        ld a, [wSnd_Temp_9]
        inc a
        cp $04
        jp nz, .LoopTracks

        ret

    ; $58A7
_snd_PatternStatusBitmap_PatternEnabled:
    db $01, $02, $04, $08
    ; $58AB
_snd_PatternStatusBitmap_NoteOffTimer:
    db $10, $20, $40, $80

_snd_HandleNoteOffs:
    ; Turns off notes at the end of their play duration, if the voice wasn't stolen by an SFX
    ; Inputs:
    ;   bc = song speed
    xor a

    .TrackLoop:
        ld [wSnd_Temp_9], a ;Input track
        ld e, a
        ld d, $00
        ld hl, _snd_PatternStatusBitmap_NoteOffTimer
        add hl, de
        ld a, [wSnd_sss_PatternStatus]
        and [hl]
        jr z, .SkipTrack ;Skip if the upper nibble bit is reset

            push bc ;Song speed
            ld a, e
            sla e
            add e
            ld e, a
            ld hl, wSnd_sss_Track_NoteEndTimer
            add hl, de
            ld a, [hl]
            sub c
            ld [hl+], a
            ld d, a
            ld a, [hl]
            sbc b
            ld [hl+], a
            ld e, a
            ld a, [hl]
            sbc $00
            ld [hl+], a ;wSnd_sss_Track_NoteEndTimer -= Song speed

            bit 7, a   ;negative number ($FFFFFF)
            jr nz, .TimerNegativeOrZero
            or d
            or e ;24-bit or to check for positive number
            jr nz, .TimerPositiveNumber ;Jump if positive number, pass if zero

            .TimerNegativeOrZero:
                ld a, [wSnd_Temp_9]  ;Input track
                ld e, a
                ld d, $00
                ld hl, _snd_PatternStatusBitmap_NoteOffTimer
                add hl, de
                ld a, [hl]

                cpl
                ld c, a
                ld a, [wSnd_sss_PatternStatus]
                and c
                ld [wSnd_sss_PatternStatus], a ;Reset the NoteOff flag

                ld a, [wSnd_sss_TrackToVoiceMap]
                bit 1, e  ;Input track
                jr z, .TrackCheckA
                    swap a ;Track 2,3
                .TrackCheckA:
                    bit 0, e
                    jr z, .TrackCheckB
                    srl a ;Track 1,3
                    srl a
                .TrackCheckB:
                and %00000011 ;a = Voice = (wSnd_sss_TrackToVoiceMap >> 2*Track) & %00000011

                ld c, a
                ld b, $00
                ld hl, wSnd_Voice_FrequencyStatus
                add hl, bc
                bit 1, [hl]
                jr nz, .TimerPositiveNumber ;Abort if SFX has stolen the voice
                    ld a, c
                    ld [wSnd_SelectedVoice_Index], a
                    call _snd_KeyOff_SelectedTrack ;Send KeyOff

    .TimerPositiveNumber:
        pop bc

    .SkipTrack:
        ld a, [wSnd_Temp_9]  ;Input track
        inc a
        cp $04
        jr nz, .TrackLoop
        ret

    ; $5921
_snd_HandleNoteOns:
    ; Called by handleSong
    xor a

    .TrackLoop:
        ld [wSnd_Temp_9], a ; Track loop
        ld e, a
        ld d, $00
        ld hl, _snd_PatternStatusBitmap_PatternEnabled
        add hl, de
        ld a, [wSnd_sss_PatternStatus]
        and [hl]
        jr z, .SkipTrack ;Skip if pattern is not enabled

            push bc ;SongSpeed
            ld a, e
            sla e
            add e
            ld e, a
            ld hl, wSnd_sss_Track_NoteStartTimer
            add hl, de
            ld a, [hl]
            sub c
            ld [hl+], a
            ld d, a
            ld a, [hl]
            sbc b
            ld [hl+], a
            ld e, a
            ld a, [hl]
            sbc $00
            ld [hl+], a ;wSnd_sss_Track_NoteStartTimer -= SongSpeed

            bit 7, a 
            jr nz, .TryPlayNote   ;negative number ($FFFFFF)
            or d
            or e ;24-bit or to check for positive number
            jr nz, .SkipTrackPop ;Jump if positive number, pass if zero

            .TryPlayNote:
                ld a, [wSnd_Temp_9]
                call _snd_CheckToPlayNote ;Try to play note if possible
                jr nz, .SkipTrackPop ;End if ProgramChangeEvent or End of pattern
                    ld a, [wSnd_Temp_9]
                    call snd_sss_SetupNoteStartDelay ;Try the next note if normal Event
                    jr .TryPlayNote

    .SkipTrackPop:
        pop bc

    .SkipTrack:
        ld a, [wSnd_Temp_9]  ; Input track
        inc a
        cp $04
        jr nz, .TrackLoop

        ret

    ; $596A
_snd_SetupNewPattern:
    ; Enables the pattern
    ; Resets wSnd_sss_Track_NoteStartTimer
    ; Sets up wSnd_sss_Track_PatternReadAddress
    ; Reads the first note(s) if they are at time 0

    ; a = track index
    ; c = pattern index
    ld [wSnd_Temp_B], a
    ld hl, _snd_PatternStatusBitmap_PatternEnabled
    ld e, a
    ld d, $00
    add hl, de
    ld a, [wSnd_sss_PatternStatus]
    or [hl]
    ld [wSnd_sss_PatternStatus], a
    ld a, e
    sla e
    add e
    ld e, a
    ld hl, wSnd_sss_Track_NoteStartTimer
    add hl, de
    xor a
    ld [hl+], a
    ld [hl+], a
    ld [hl], a
    ld a, [wSnd_Temp_B]
    call snd_sss_SetupPattern

    .CheckNextNote:
        ld a, [wSnd_Temp_B]
        call snd_sss_SetupNoteStartDelay
        ld a, [wSnd_Temp_B]
        call _snd_CheckToPlayNote
        jr z, .CheckNextNote ;If program change or note, then see if the next one is immediately played after
    ret

    ; $599D
_snd_CheckToPlayNote:
    ; a = Track
    ; Returns non-zero if no note played or end of pattern
    ; Returns zero if note and/or program change
    ld c, a
    ld e, a
    sla e
    add e
    ld e, a
    ld d, $00
    ld hl, wSnd_sss_Track_NoteStartTimer+2
    add hl, de
    ld a, [hl-]
    bit 7, a
    jr nz, .TimerIsNegativeOrZero ;Negative number
        ld b, a
        ld a, [hl-]
        or b
        ld b, a
        ld a, [hl]
        or b
        ret nz ;Positive number. Abort. Pass if $000000
    .TimerIsNegativeOrZero:
        ld b, $00
        push bc
        ld a, c ;InputTrack
        call snd_sss_SetupPatternNote

        bit 7, d
        jr z, .RegularEvent
        bit 7, a
        jr nz, .ProgramChangeEvent
        ;EndOfPattern
            pop bc ;c=InputTrack
            ld hl, _snd_PatternStatusBitmap_PatternEnabled
            add hl, bc
            ld a, [hl]
            cpl
            ld e, a
            ld a, [wSnd_sss_PatternStatus]
            and e
            ld [wSnd_sss_PatternStatus], a ;Disable pattern
            rlca ;Reset ZF
            ret
        .ProgramChangeEvent:
            and $7F
            cp $02
            jr nc, .SkipResetToggleBitInitialized
                ld hl, wSnd_sss_Status ;Reset SETVOICE toggle bit if the Soundlist entry is 0-1?
                res 6, [hl]            ;I think this is a bug, and it wanted to check if the TrackId was 0-1 (corresponding to voices 0-1)
            .SkipResetToggleBitInitialized:
            call snd_sss_GetProgram
            ld hl, wSnd_sss_Track_CurrentProgram
            pop bc
            add hl, bc
            ld [hl], a
            xor a ;Set ZF
            ret
        .RegularEvent:
            bit 7, a
            jr z, .PlayNote ;No program - skip
            ;HandleProgram
                and $7F
                cp $02
                jr nc, .SkipResetToggleBitInitializedB
                    ld hl, wSnd_sss_Status ;Reset SETVOICE toggle bit if the Soundlist entry is 0-1?
                    res 6, [hl]            ;I think this is a bug, and it wanted to check if the TrackId was 0-1 (corresponding to voices 0-1)
                .SkipResetToggleBitInitializedB:
                call snd_sss_GetProgram
                pop hl
                push hl
                push bc
                ld bc, wSnd_sss_Track_CurrentProgram
                add hl, bc
                ld [hl], a
                pop bc
            .PlayNote:
                pop hl
                push bc
                ld c, l
                ld b, h
                ld a, e
                ld [wSnd_Temp_D], a ;Velocity
                ld hl, wSnd_sss_CurrentKey
                add hl, bc
                ld a, [hl]
                ld [hl], d
                ld hl, wSnd_sss_LastKey
                add hl, bc  ;Move CurrentKey to LastKey
                ld [hl], a  ;Put the new key into CurrentKey

                ld hl, wSnd_sss_Track_NoteEndTimer
                pop de
                push bc
                ld a, c
                sla c
                add c
                ld c, a
                add hl, bc
                xor a
                ld [hl+], a
                ld a, e
                ld [hl+], a
                ld [hl], d ;Set wSnd_sss_Track_NoteEndTimer

                pop bc
                ld hl, _snd_PatternStatusBitmap_NoteOffTimer
                add hl, bc
                ld a, [wSnd_sss_PatternStatus]
                or [hl]
                ld [wSnd_sss_PatternStatus], a ;Enable the note off timer

                ld a, [wSnd_SFXSongVolume]
                and $0F
                ld e, a
                ld a, [wSnd_Temp_D]
                call _snd_CalculateOutputVolume ;velocity * wSnd_SFXSongVolume
                ld e, a ; e = Velocity

                ld hl, wSnd_sss_CurrentKey
                add hl, bc
                ld d, [hl] ; d = Song Key
                ld hl, wSnd_sss_Track_CurrentProgram
                add hl, bc
                ld a, c ;c = Voice = track index
                ld [wSnd_Temp_D], a ;track index
                ld a, [hl] ;a = SoundMacroID

                call _snd_GenerateMacro_Song ;Priority is 0

                ld a, [wSnd_Temp_D]
                ld e, a
                ld d, $00
                ld hl, .TrackToVoiceMap_DisableBitmap
                add hl, de
                ld a, [wSnd_sss_TrackToVoiceMap]
                and [hl] ;First, reset the mapping

                bit 1, e
                jr z, .TrackCheckA
                    swap c
                .TrackCheckA:
                    bit 0, e
                    jr z, .TrackCheckB
                    sla c
                    sla c
                .TrackCheckB: ;c = (Voice << Track*2)

                or c      ;Write the new mapping
                ld [wSnd_sss_TrackToVoiceMap], a
                xor a ;Set ZF
                ret

    ; $5A75
    .TrackToVoiceMap_DisableBitmap:
    db %11111100, %11110011, %11001111, %00111111

    ; $5A79
snd_ProjectData::
sdp_ADSRTableAddress            EQU     snd_ProjectData + $0000
sdp_SFXTableAddress             EQU     snd_ProjectData + $0002
sdp_NumberOfSFXs                EQU     snd_ProjectData + $0004
sdp_SampleTableAddress          EQU     snd_ProjectData + $0005
sdp_NumberOfSamples             EQU     snd_ProjectData + $0007
sdp_GBSlave_Check               EQU     snd_ProjectData + $0008
sdp_SampleMapMacro_Address      EQU     snd_ProjectData + $0009
sdp_NumberOfSampleMapEntries    EQU     snd_ProjectData + $000B
sdp_SongTableAddress            EQU     snd_ProjectData + $000C
sdp_NumberOfSongs               EQU     snd_ProjectData + $000E
sdp_SoundMacroLookupTable       EQU     snd_ProjectData + $000F