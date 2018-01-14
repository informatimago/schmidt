;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               schmidt.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements encoding and decoding of Schmidt Synthesizer bank MIDI SysExs.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-01-14 <PJB> Created.
;;;;BUGS
;;;;
;;;;    - Only Multi Bank sysex are not implemented yet.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT")

;; ###################
;; MIDI IMPLEMENTATION
;; ###################
;;
;;
;; MIDI CONTROLLER ASSIGNMENTS
;; ****************************************
;;
;; CC#
;; Parameter       Send/Receive        Explanation             Comment
;;
;; 0/32            S/R                 Bank-Select MSB/LSB     Single/Multi:
;;                                                             MSB=0
;;                                                             LSB=0...7
;;
;;
;; 1/33            S/R                 Mod-Wheel MSB/LSB       LSB only sent when
;;                                                             Snd-CC14Bit: On
;;
;; 6/38            S/R
;; 7/39            S/R
;; 64              S/R
;; 66              S/R
;; 98              S/R
;; 99              S/R
;; 102             S/R
;; 103             S/R
;; 104             S/R
;; 105             S/R
;; 106             S/R
;; 107             R
;; 108             R
;; 109             R
;; 121             R
;; 122             R
;; 123             R
;;
;; Please note:
;;
;; NRPN:
;;
;; 1.) NRPN-Nr MSB (CC#99)
;; 2.) NRPN-Nr LSB (CC#98)
;; 3.) Data-Entry MSB (CC#38)
;; 4.) Data-Entry LSB (CC#6)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Single Set =  8 Single Banks
Single Bank = 128 Single Presets

Multi Set = 8 Multi Banks
Multi Bank = 32 Multi Presets


Schmidt Synthesizer SysEx Headers
########################################

F0 07 0D 07 07 03 03  firmware (voice, system, panel).
F0 7D 77 33 0b 00 00  single bank dump (bank #b)


SysEx Format for Single Bank
========================================

MIDI Sysex Format
----------------------------------------

    MIDI SysEx header:

        F0              System Exclusive
        7D              Non-Commercial SysEx
        77
        33
        07              bank number 00 - 07 (for bank 1 to 8).
        00 00

    Single Bank Data:

       65536 octets.  Each octet encodes a quad between 0 and 15 (00H…0FH).
       Two successive octets are combined in little-endian order to form a data octet:
       0w 0x 0y 0z …  encode xw zy …, resulting into 32768 octet of bank data block.

    MIDI SysEx trailer:

        05 0E 03 02     check-sum?
        F7              EOX (End of Exclusive)


Bank Data Block Format
----------------------------------------

A Single Bank Block contains 32768 octets: the concatenation of 128
program data blocks of 256 octets each.

The last byte contains a check-sum computed as the arithmetic sum of
all the octets in the bank data block, modulo 256.


Program Data Block Format
----------------------------------------

The program parameters are numbered as documented for the NPRN MIDI
messages sent and received when modifying the parameters of the
current program.  Here is the list of documented parameters:

    Number of parameters: 200
      1: (vcf 1) ENV Depth
      2: (vcf 2) ENV Depth
      3: (vcf 1) Cutoff
      4: (vcf 2) Cutoff
      5: (df 1) Cutoff
      6: (df 2) Cutoff
      7: (osz 3) Main Pitch
      8: (osz 4) Main Tune
     31: (vcf 1/2) VCF12 LFO-Sync-Mode
     32: (master-env/vca) Sound Volume
     33: (master-env/vca) Attack
     34: (master-env/vca) Decay
     35: (master-env/vca) Sustain
     36: (master-env/vca) Release
     37: (master-env/vca) Release-Level
     38: (group 1/2) Group 1 Velocity
     39: (group 1/2) Group 2 Velocity
     40: (vcf 1) Attack
     41: (vcf 1) Decay 1
     42: (vcf 1) Sustain
     43: (vcf 1) Release
     44: (vcf 2) Attack
     45: (vcf 2) Decay 1
     46: (vcf 2) Sustain
     47: (vcf 2) Release
     48: (group 1/2) Min Man/Fade-Controls
     49: (group 1/2) Group 1/2 Man-Mix
     50: (group 1/2) Group 1/2 Fade-Time
     51: (group 1/2) Group 1/2 Fade-Delay
     52: (group 1/2) Panorama-Controls
     53: (group 1/2) Group 1 Pan-Offset
     54: (group 1/2) Group 2 Pan-Offset
     55: (group 1/2) Panorama-LFO-Depth
     56: (group 1/2) Panorama-LFO-Rate
     57: (group 1/2) LFO-Controls
     58: (group 1/2) Mix-LFO-Depth
     59: (group 1/2) Mix-LFO-Rate
     60: (master-env/vca) Soft/VCA-VCF12-Retrigg
     61: (vcf 1/2) VCF12 Decay2
     62: (vcf 1) Decay 2
     63: (vcf 2) Decay 2
     64: (vcf 1) ENV Destination (Velocity)
     65: (vcf 2) ENV Destination (Velocity)
     66: (vcf 1) ENV Velocity-Depth
     67: (vcf 2) ENV Velocity-Depth
     68: (vcf 2) ENV-Trigger-Repeat-Rate
     69: (vcf 1) Trigger Delay
     70: (vcf 2) Trigger Delay
     71: (glide/bend) Glide Depth
     72: (osz 1234) Unisono Tune (Mono)
     73: (osz 1234) Single Fine Tune
     74: (osz 1234) Single Transpose
     75: (vcf 1) Resonance
     76: (vcf 1) Filter Mode (LP-BP-HP)
     77: (vcf 2) Resonance
     78: (vcf 2) Filter Mode (LP-BP-HP)
     79: (vcf 1/2) Input Source
     80: (df 1/2) Input Source
     81: (vcf 1/2) Input B Filter/Level Mod
     82: (vcf 1) Input Level A
     83: (vcf 1) Input Level B
     84: (vcf 2) Input Level A
     85: (vcf 2) Input Level B
     86: (vcf 1) Level B Mod Time Out
     87: (vcf 2) Level B Mod Time Out
     88: (vcf 1) Velocity
     89: (vcf 2) Velocity
     90: (vcf 1) Key Follow
     91: (vcf 2) Key Follow
     92: (df 1) Input Level A
     93: (df 1) Input Level B
     94: (df 2) Input Level A
     95: (df 2) Input Level B
     96: (vcf 1) Input Level DF1 Out
     97: (vcf 2) Input Level DF1 Out
     98: (group 1/2) Group 1 Out DF 1 Level
     99: (group 1/2) Group 2 Out DF 2 Level
    100: (vcf 1/2 df 1/2) LFO Source
    101: (vcf 1) LFO Control/Mode
    102: (vcf 2) LFO Control/Mode
    103: (vcf 1) LFO Depth
    104: (vcf 2) LFO Depth
    105: (vcf 1) LFO Rate
    106: (vcf 2) LFO Rate
    107: (vcf 1) LFO Time
    108: (vcf 2) LFO Time
    109: (group 1/2) Input/Output
    110: (group 1/2) VCF3 Group 1/2 Out
    111: (group 1/2) Group 1 Out VCF3 Level
    112: (group 1/2) Group 2 Out VCF3 Level
    113: (group 1/2) VCF3 Cutoff
    114: (group 1/2) DF 1/2 Group 1/2 Out
    115: (group 1/2) Group 1 Out DF 1 Distortion
    116: (group 1/2) Group 2 Out DF 2 Distortion
    117: (df 2) Assign DF1 Value
    118: (df 1) Space
    119: (df 1) Key Follow
    120: (df 1) Velocity
    121: (df 1) ENV Depth
    122: (df 1) LFO Depth
    123: (df 1) LFO Rate
    124: (df 2) Space
    125: (df 2) Key Follow
    126: (df 2) Velocity
    127: (df 2) ENV Depth
    128: (df 2) LFO Depth
    129: (df 2) LFO Rate
    130: (df 1/2) ENV-Mode/DF1->2 Assign
    131: (df 1/2) Resonance
    132: (df 1) Mode/Mulator-Settings
    133: (df 2) Mode/Mulator-Settings
    134: (df 1) ENV Mode:ADR : ENV Attack; ENV Mode:Ramp : CLK-Rate
    135: (df 2) ENV Mode:ADR : ENV Attack; ENV Mode:Ramp : CLK-Rate
    136: (df 1) ENV Mode:ADR : ENV Decay; ENV Mode:Ramp : Quantize
    137: (df 2) ENV Mode:ADR : ENV Decay; ENV Mode:Ramp : Quantize
    138: (df 1) ENV Mode:ADR : ENV Release; ENV Mode:Ramp : #Repeats
    139: (df 2) ENV Mode:ADR : ENV Release; ENV Mode:Ramp : #Repeats
    140: (df 1) ENV Trigger Delay
    141: (df 2) ENV Trigger Delay
    142: (df 1) Ramp Nr
    143: (df 2) Ramp Nr
    145: (glide/bend) Single Mode/Glide Mode
    146: (glide/bend) Glide Time Filter
    147: (glide/bend) Glide Time OSZ
    148: (glide/bend) Pitch Bend
    149: (osz 1) Detune
    150: (osz 1) Detune Fine/KBD Scale/Wave
    151: (osz 1) Octave/Sub Octave
    152: (osz 1) Noise Modulation
    153: (osz 1) Semitone
    154: (osz 123) PWM Settings
    155: (osz 1) PWM Center
    156: (osz 1) PWM LFO-Rate
    157: (osz 1) PWM LFO-Depth
    158: (osz 2) PWM Center
    159: (osz 2) PWM LFO-Rate
    160: (osz 2) PWM LFO-Depth
    161: (osz 3) PWM Center
    162: (osz 3) PWM LFO-Rate
    163: (osz 3) PWM LFO-Depth
    164: (osz 1) Multi PWM Width Center
    165: (osz 1) Multi PWM Space Center
    166: (osz 1) Multi PWM Width LFO Rate
    167: (osz 1) Multi PWM Width LFO Depth
    168: (osz 1) Multi PWM Space LFO Rate
    169: (osz 1) Multi PWM Space LFO Depth
    170: (osz 1) Multi PWM Diffuse/Mode
    171: (osz 1) Multi PWM LFO Vel/Mode/Kbd
    172: (osz 2) Detune
    173: (osz 2) Detune Fine/KBD Scale/Wave
    174: (osz 2) Octave/Sub Octave
    175: (osz 2) Noise Modulation
    176: (osz 2) Semitone
    177: (osz 3) Detune
    178: (osz 3) Semitone
    179: (osz 3) Noise Modulation
    180: (osz 3) Wave
    181: (osz 3) Detune Fine/KBD Scale/Octave
    182: (osz 3) Subosz Osz3/Sync/Add.Pitch Mod
    183: (osz 3) LFO
    184: (osz 3) FM Depth Osz2
    185: (osz 3) Fine Pitch
    186: (osz 3) Velocity
    187: (osz 3) LFO Depth
    188: (osz 3) LFO Rate
    189: (osz 4) Fine Tune
    190: (osz 4) Velocity
    191: (osz 4) KBD Scale
    192: (osz 4) Noise Modulation
    193: (osz 4) Wave Preset A/B
    194: (osz 4) Octave/Mode
    195: (osz 4) A/B Mix Settings
    196: (osz 4) A/B Mix
    197: (osz 4) A/B Mix Rate
    198: (osz 1) Vibrato Depth
    199: (osz 2) Vibrato Depth
    200: (osz 3) Vibrato Depth
    201: (osz 4) Vibrato Depth
    202: (osz 1) Vibrato Rate
    203: (osz 2) Vibrato Rate
    204: (osz 3) Vibrato Rate
    205: (osz 4) Vibrato Rate
    206: (osz 1) Vibrato Wave
    207: (osz 2) Vibrato Wave
    208: (osz 3) Vibrato Wave
    209: (osz 4) Vibrato Wave
    210: (osz 1) Envelope Depth
    211: (osz 2) Envelope Depth
    212: (osz 3) Envelope Depth
    213: (osz 4) Envelope Depth
    214: (osz 1) Envelope Time
    215: (osz 2) Envelope Time
    216: (osz 3) Envelope Time
    217: (osz 4) Envelope Time
    218: (osz 1) Env Destination/Mode
    219: (osz 2) Env Destination/Mode
    220: (osz 3) Env Destination/Mode
    221: (osz 4) Env Destination/Mode
    222: (osz 1234) Vibrato Assign
    223: (osz 1234) Envelop Assign


The first 8 parameters are encoded over 9 bits; the less-significant 8
bits are stored in the octet at the NPRN index (W).  The 9th bits, the
most significant bits of each of those 8 parameters are collected in a
single octet, stored at the index 9 (H).

The name of the program is stored on the 16 octets from 11 to 26 (N);
the 7 less significant bits of each octets contain a Schmidt character
code; the most significant bits are combined to forms two more 7-bit
Schmidt character codes.  Thus a program name has therefore 18 characters.

The Schmidt character code is indicated in the following table: the
character codes are in the order of the characters as they are scanned
when entering the program name in the Schmidt Synthesizer display.

      0: space
      1: A     27: a     53: 0     79: ;
      2: B     28: b     54: 1     80: <
      3: C     29: c     55: 2     81: =
      4: D     30: d     56: 3     82: >
      5: E     31: e     57: 4     83: ?
      6: F     32: f     58: 5     84: @
      7: G     33: g     59: 6     85: [
      8: H     34: h     60: 7     86: \
      9: I     35: i     61: 8     87: ]
     10: J     36: j     62: 9     88: ^
     11: K     37: k     63: !     89: _
     12: L     38: l     64: "     90: `
     13: M     39: m     65: #     91: {
     14: N     40: n     66: $     92: |
     15: O     41: o     67: %     93: }
     16: P     42: p     68: &     94: ~
     17: Q     43: q     69: '
     18: R     44: r     70: (
     19: S     45: s     71: )
     20: T     46: t     72: *
     21: U     47: u     73: +
     22: V     48: v     74: ,
     23: W     49: w     75: -
     24: X     50: x     76: .
     25: Y     51: y     77: /
     26: Z     52: z     78: :


The other parameters are either continuous parameters (potentiometers)
encoded on an octet, or switch parameters encoded in bitfields packed
into octets (*).

    _WWWWWWWWH_NNNNNNNNNNNNNNNN____*********************************
    ****************************************************************
    ****************_***********************************************
    ********************************________________________________

The remaining bytes (_) are undocumented and not yet
reverse-engineered.

|#



;; The *PARAMETER* list contains specification of the NPRN MIDI
;; messages that are sent and received for each parameter of the
;; Schmidt Synthesizer control panel, along with the encoding of the
;; parameter (as 9-bit word, as 8-bit byte, or as bit fields on 8-bit
;; bytes).

(defparameter *parameters*
  '(

    (:restriction (OSZ 123)
     :parameter "PWM Settings"
     :nrpn 154
     :direction S/R
     :type switch
     :data-format LMKKMVXR
     :explanation ((L "Max Limit"
                    ((1 "On")
                     (0 "Off")))
                   (MM "LFO-Mode"
                    ((#b01 "One-Shot")
                     (#b00 "Triangle")
                     (#b10 "Sine")))
                   (KK "Kbd Scale"
                    ((#b00 "Off")
                     (#b10 "1/4")
                     (#b11 "1/2")))
                   (V "Vel-LFO-Depth"
                    ((1 "On")
                     (0 "Off")))
                   (R "Rate1->Rate23"
                    ((1 "On")
                     (0 "Off")))))


    (:restriction (OSZ 1234)
     :parameter "Vibrato Assign"
     :nrpn 222
     :direction S/R
     :type switch
     :data-format SXXXXXXX
     :explanation ((S "Assign to Osz1234"
                     ((1 "Off")
                      (0 "On")))))

    (:restriction (OSZ 1234)
     :parameter "Envelop Assign"
     :nrpn 223
     :direction S/R
     :type switch
     :data-format SXXXXXXX
     :explanation ((S "Assign to Osz1234"
                     ((1 "Off")
                      (0 "On")))))

    (:restriction (OSZ 1234)
     :parameter "Unisono Tune (Mono)"
     :nrpn 72
     :direction S/R
     :type M
     :data-format byte
     :explanation ((byte "Unisono Tune"
                    ((range 0 255)))))

    (:restriction (OSZ 1234)
     :parameter "Single Fine Tune"
     :nrpn 73
     :direction S/R
     :type M
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "-50Cent…0…+50Cent")))))

    (:restriction (OSZ 1234)
     :parameter "Single Transpose"
     :nrpn 74
     :direction S/R
     :type M
     :data-format byte
     :explanation ((signed-byte "Value"
                    ((range -12 +12 "-12HT…0…+12HT")))))


    ;; OSZ 1

    (:restriction (OSZ 1)
     :parameter "Vibrato Wave"
     :nrpn 206
     :direction S/R
     :type switch
     :data-format byte
     :explanation ((byte "Wave"
                    ((0 "Sine")
                     (1 "Square")
                     (2 "Random")
                     (3 "Sine/Random")))))

    (:restriction (OSZ 1)
     :parameter "Vibrato Depth"
     :nrpn 198
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 1)
     :parameter "Vibrato Rate"
     :nrpn 202
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "0,1Hz…75Hz; 0,2Hz…150Hz (Wave:Random)")))))

    (:restriction (OSZ 1)
     :parameter "Env Destination/Mode"
     :nrpn 218
     :direction S/R
     :type switch
     :data-format DDXXQMVM
     :explanation ((Q "Quant"
                     ((1 "On")
                      (0 "Off")))
                   (V "Vel"
                    ((1 "On")
                     (0 "Off")))
                   (DD "Dest"
                     ((#b00 "Pitch")
                      (#b10 "Vib.Depth")
                      (#b11 "Noise Depth")))
                   (MM "Mode"
                    ((#b00 "Decay Exp")
                     (#b10 "Decay Lin")
                     (#b01 "Attack/Decay")))))

    (:restriction (OSZ 1)
     :parameter "Envelope Depth"
     :nrpn 210
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "-15HT…0…+15HT")))))

    (:restriction (OSZ 1)
     :parameter "Envelope Time"
     :nrpn 214
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "12ms…6,15 (Decay Lin/Exp); 7,5ms…3,85 (Attack/Decay)")))))

    (:restriction (OSZ 1)
     :parameter "Noise Modulation"
     :nrpn 152
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 1)
     :parameter "Detune"
     :nrpn 149
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "fine on: (-15…+15Cent); fine off: (-75…+75Cent)")))))

    (:restriction (OSZ 1)
     :parameter "Semitone"
     :nrpn 153
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((2)
                     (4)
                     (6)
                     (range 8 30 "-7HT…+7HT")))))

    (:restriction (OSZ 1)
     :parameter "Detune Fine/KBD Scale/Wave"
     :nrpn 150
     :direction S/R
     :type switch
     :data-format KKFXXWWW
     :explanation ((KK "Kbd Scale"
                    ((#b00 "Off")
                     (#b10 "1/4")
                     (#b11 "1/2")))
                   (F "Fine"
                    ((0 "On")
                     (1 "Off")))
                   (WWW "Wave"
                    ((#b001 "Square")
                     (#b010 "PW")
                     (#b101 "Saw")
                     (#b111 "Saw/PW")
                     (#b100 "Multi PWM")
                     (#b110 "Multi/PW")
                     (#b011 "Noise")))))

    (:restriction (OSZ 1)
     :parameter "Octave/Sub Octave"
     :nrpn 151
     :direction S/R
     :type switch
     :data-format XSSSXXOO
     :explanation ((SSS "Sub-Oct"
                    ((#b000 "Off")
                     (#b001 "64\"")
                     (#b010 "32\"")
                     (#b011 "16\"")
                     (#b100 "8\"")))
                   (OO "Octave"
                    ((#b00 "32\"")
                     (#b01 "16\"")
                     (#b10 "8\"")
                     (#b11 "4\"")))))

    (:restriction (OSZ 1)
     :parameter "Multi PWM Diffuse/Mode"
     :nrpn 170
     :direction S/R
     :type switch
     :data-format XXXDDXMM
     :explanation ((DD "LFO Diffuse"
                     ((#b00 "Off")
                      (#b10 "Slow")
                      (#b11 "Fast")))
                   (MM "Mode"
                    ((#b00 "(1)")
                     (#b01 "(2)")
                     (#b10 "(3)")
                     (#b11 "(Osz4->RM)")))))

    (:restriction (OSZ 1)
     :parameter "Multi PWM LFO Vel/Mode/Kbd"
     :nrpn 171
     :direction S/R
     :type switch
     :data-format KKKMMMWS
     :explanation ((KKK "Kbd Scale"
                    ((#b000 "Off")
                     (#b110 "1/4")
                     (#b111 "1/2")))
                   (MMM "Mode"
                    ((#b001 "Decay")
                     (#b101 "Attack/Decay")
                     (#b000 "LFO")
                     (#b010 "Step")))
                   (W "Width-LFO-Vel"
                    ((1 "On")
                     (0 "Off")))
                   (S "Space-LFO-Vel"
                     ((1 "On")
                      (0 "Off")))))

    (:restriction (OSZ 1)
     :parameter "Multi PWM Width Center"
     :nrpn 164
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (OSZ 1)
     :parameter "Multi PWM Width LFO Depth"
     :nrpn 167
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (OSZ 1)
     :parameter "Multi PWM Width LFO Rate"
     :nrpn 166
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "0,025Hz…20Hz")))))

    (:restriction (OSZ 1)
     :parameter "Multi PWM Space Center"
     :nrpn 165
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (OSZ 1)
     :parameter "Multi PWM Space LFO Depth"
     :nrpn 169
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (OSZ 1)
     :parameter "Multi PWM Space LFO Rate"
     :nrpn 168
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Mode:LFO : 0,025Hz…20Hz; Mode:Decay : 20s…25ms; Mode:A/D : 40s…50 ms")))))

    (:restriction (OSZ 1)
     :parameter "PWM Center"
     :nrpn 155
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (OSZ 1)
     :parameter "PWM LFO-Rate"
     :nrpn 156
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Mode:Sine/Tri: 0,025Hz…20Hz; Mode:One Shot: 20s…25ms")))))

    (:restriction (OSZ 1)
     :parameter "PWM LFO-Depth"
     :nrpn 157
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))


    ;; OSZ 2

    (:restriction (OSZ 2)
     :parameter "Vibrato Wave"
     :nrpn #.(+ 1 206)
     :direction S/R
     :type switch
     :data-format byte
     :explanation ((byte "Wave"
                    ((0 "Sine")
                     (1 "Square")
                     (2 "Random")
                     (3 "Sine/Random")))))

    (:restriction (OSZ 2)
     :parameter "Vibrato Depth"
     :nrpn #.(+ 1 198)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 2)
     :parameter "Vibrato Rate"
     :nrpn #.(+ 1 202)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "0,1Hz…75Hz; 0,2Hz…150Hz (Wave:Random)")))))

    (:restriction (OSZ 2)
     :parameter "Env Destination/Mode"
     :nrpn #.(+ 1 218)
     :direction S/R
     :type switch
     :data-format DDXXQMVM
     :explanation ((Q "Quant"
                     ((1 "On")
                      (0 "Off")))
                   (V "Vel"
                    ((1 "On")
                     (0 "Off")))
                   (DD "Dest"
                     ((#b00 "Pitch")
                      (#b10 "Vib.Depth")
                      (#b11 "Noise Depth")))
                   (MM "Mode"
                    ((#b00 "Decay Exp")
                     (#b10 "Decay Lin")
                     (#b01 "Attack/Decay")))))

    (:restriction (OSZ 2)
     :parameter "Envelope Depth"
     :nrpn #.(+ 1 210)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "-15HT…0…+15HT")))))

    (:restriction (OSZ 2)
     :parameter "Envelope Time"
     :nrpn #.(+ 1  214)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "12ms…6,15 (Decay Lin/Exp); 7,5ms…3,85 (Attack/Decay)")))))

    (:restriction (OSZ 2)
     :parameter "Noise Modulation"
     :nrpn 175
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 2)
     :parameter "Detune"
     :nrpn 172
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "fine on: (-15…+15Cent); fine off: (-75…+75Cent)")))))

    (:restriction (OSZ 2)
     :parameter "Semitone"
     :nrpn 176
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((2)
                     (4)
                     (6)
                     (range 8 30 "-7HT…+7HT")))))

    (:restriction (OSZ 2)
     :parameter "Detune Fine/KBD Scale/Wave"
     :nrpn 173
     :direction S/R
     :type switch
     :data-format KKFXXWWW
     :explanation ((KK "Kbd Scale"
                    ((#b00 "Off")
                     (#b10 "1/4")
                     (#b11 "1/2")))
                   (F "Fine"
                    ((0 "On")
                     (1 "Off")))
                   (WWW "Wave"
                    ((#b001 "Square")
                     (#b010 "PW")
                     (#b101 "Saw")
                     (#b111 "Saw/PW")
                     (#b100 "Multi PWM")
                     (#b110 "Multi/PW")
                     (#b011 "Noise")))))

    (:restriction (OSZ 2)
     :parameter "Octave/Sub Octave"
     :nrpn 174
     :direction S/R
     :type switch
     :data-format XSSSXXOO
     :explanation ((SSS "Sub-Oct"
                    ((#b000 "Off")
                     (#b001 "64\"")
                     (#b010 "32\"")
                     (#b011 "16\"")
                     (#b100 "8\"")))
                   (OO "Octave"
                    ((#b00 "32\"")
                     (#b01 "16\"")
                     (#b10 "8\"")
                     (#b11 "4\"")))))

    (:restriction (OSZ 2)
     :parameter "PWM Center"
     :nrpn #.(+ 3 155)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (OSZ 2)
     :parameter "PWM LFO-Rate"
     :nrpn #.(+ 3 156)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Mode:Sine/Tri: 0,025Hz…20Hz; Mode:One Shot: 20s…25ms")))))

    (:restriction (OSZ 2)
     :parameter "PWM LFO-Depth"
     :nrpn #.(+ 3 157)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))


    ;; OSZ 3

    (:restriction (OSZ 3)
     :parameter "Vibrato Wave"
     :nrpn #.(+ 2 206)
     :direction S/R
     :type switch
     :data-format byte
     :explanation ((byte "Wave"
                    ((0 "Sine")
                     (1 "Square")
                     (2 "Random")
                     (3 "Sine/Random")))))

    (:restriction (OSZ 3)
     :parameter "Vibrato Depth"
     :nrpn #.(+ 2 198)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 3)
     :parameter "Vibrato Rate"
     :nrpn #.(+ 2 202)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "0,1Hz…75Hz; 0,2Hz…150Hz (Wave:Random)")))))

    (:restriction (OSZ 3)
     :parameter "Env Destination/Mode"
     :nrpn #.(+ 2 218)
     :direction S/R
     :type switch
     :data-format DDXXQMVM
     :explanation ((Q "Quant"
                     ((1 "On")
                      (0 "Off")))
                   (V "Vel"
                    ((1 "On")
                     (0 "Off")))
                   (DD "Dest"
                     ((#b00 "Pitch")
                      (#b10 "Vib.Depth")
                      (#b11 "Noise Depth")))
                   (MM "Mode"
                    ((#b00 "Decay Exp")
                     (#b10 "Decay Lin")
                     (#b01 "Attack/Decay")))))

    (:restriction (OSZ 3)
     :parameter "Envelope Depth"
     :nrpn #.(+ 2 210)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "-15HT…0…+15HT")))))

    (:restriction (OSZ 3)
     :parameter "Envelope Time"
     :nrpn #.(+ 2  214)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "12ms…6,15 (Decay Lin/Exp); 7,5ms…3,85 (Attack/Decay)")))))

    (:restriction (OSZ 3)
     :parameter "Noise Modulation"
     :nrpn 179
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 3)
     :parameter "Detune"
     :nrpn 177
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "fine on: (-15…+15Cent); fine off: (-75…+75Cent)")))))

    (:restriction (OSZ 3)
     :parameter "Semitone"
     :nrpn 178
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((2)
                     (4)
                     (6)
                     (range 8 30 "-7HT…+7HT")))))

    (:restriction (OSZ 3)
     :parameter "Detune Fine/KBD Scale/Octave"
     :nrpn 181
     :direction S/R
     :type switch
     :data-format KKFXXXOO
     :explanation ((KK "Kbd Scale"
                    ((#b00 "Off")
                     (#b10 "1/4")
                     (#b11 "1/2")))
                   (F "Fine"
                    ((0 "On")
                     (1 "Off")))
                   (OO "Octave"
                    ((#b00 "32\"")
                     (#b01 "16\"")
                     (#b10 "8\"")
                     (#b11 "4\"")))))

    (:restriction (OSZ 3)
     :parameter "Wave"
     :nrpn 180
     :direction S/R
     :type switch
     :data-format byte
     :explanation ((byte "value"
                    ((#b00000011 "Square")
                     (#b00000001 "PW (RM Osz2->Osz3; Osz2 Sub+)")
                     (#b00001001 "PW (RM Osz2->Osz3; PWM Osz2)")
                     (#b00000100 "SAW (RM Osz2->Osz3 : Off)")
                     (#b00010100 "SAW (RM Osz2->Osz3 : PWM Osz2)")
                     (#b00100100 "SAW (RM Osz2->Osz3 : Osz2 Sub-)")
                     (#b00000110 "SAW+PW (RM Osz2->Osz3 : Off)")
                     (#b00000000 "Noise")))))

    (:restriction (OSZ 3)
     :parameter "Subosz Osz3/Sync/Add.Pitch Mod"
     :nrpn 182
     :direction S/R
     :type switch
     :data-format YA000SSS
     :explanation ((Y "Sync"
                    ((1 "On")
                     (0 "Off")))
                   (A "Add.Pitch Mod"
                     ((1 "On")
                      (0 "Off")))
                   (SSS "Sub-Oct"
                    ((#b000 "Off")
                     (#b001 "64\"")
                     (#b010 "32\"")
                     (#b011 "16\"")
                     (#b100 "8\"")))))

    (:restriction (OSZ 3)
     :parameter "LFO"
     :nrpn 183
     :direction S/R
     :type switch
     :data-format XXVMMXLX
     :explanation ((V "Vel LFO-Depth"
                    ((0 "On")
                     (1 "Off")))
                   (MM "Mode"
                    ((#b11 "Decay")
                     (#b01 "Attack")
                     (#b00 "LFO")))
                   (L "Pitch->Level Osz3"
                    ((0 "On")
                     (1 "Off")))))

    (:restriction (OSZ 3)
     :parameter "FM Depth Osz2"
     :nrpn 184
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (OSZ 3)
     :parameter "Main Pitch"
     :nrpn 7
     :direction S/R
     :type pot
     :data-format word
     :explanation ((word "Value" ((range 0 511 "0…+4 Octaves")))))

    (:restriction (OSZ 3)
     :parameter "Fine Pitch"
     :nrpn 185
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-50…+50 Cent")))))

    (:restriction (OSZ 3)
     :parameter "Velocity"
     :nrpn 186
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-16HT…0…+16HT")))))

    (:restriction (OSZ 3)
     :parameter "LFO Depth"
     :nrpn 187
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (OSZ 3)
     :parameter "LFO Rate"
     :nrpn 188
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Mode:LFO : 0,025Hz…20Hz; Mode:Attack/Decay : 20s…25ms")))))

    (:restriction (OSZ 3)
     :parameter "PWM Center"
     :nrpn 161
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (OSZ 3)
     :parameter "PWM LFO-Rate"
     :nrpn 162
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Mode:Sine/Tri: 0,025Hz…20Hz; Mode:One Shot: 20s…25ms")))))

    (:restriction (OSZ 3)
     :parameter "PWM LFO-Depth"
     :nrpn 163
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))



    ;; OSZ 4

    (:restriction (OSZ 4)
     :parameter "Vibrato Wave"
     :nrpn #.(+ 3 206)
     :direction S/R
     :type switch
     :data-format byte
     :explanation ((byte "Wave"
                    ((0 "Sine")
                     (1 "Square")
                     (2 "Random")
                     (3 "Sine/Random")))))

    (:restriction (OSZ 4)
     :parameter "Vibrato Depth"
     :nrpn #.(+ 3 198)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 4)
     :parameter "Vibrato Rate"
     :nrpn #.(+ 3 202)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "0,1Hz…75Hz; 0,2Hz…150Hz (Wave:Random)")))))

    (:restriction (OSZ 4)
     :parameter "Env Destination/Mode"
     :nrpn #.(+ 3 218)
     :direction S/R
     :type switch
     :data-format DDXXQMVM
     :explanation ((Q "Quant"
                     ((1 "On")
                      (0 "Off")))
                   (V "Vel"
                    ((1 "On")
                     (0 "Off")))
                   (DD "Dest"
                     ((#b00 "Pitch")
                      (#b10 "Vib.Depth")
                      (#b11 "Noise Depth")))
                   (MM "Mode"
                    ((#b00 "Decay Exp")
                     (#b10 "Decay Lin")
                     (#b01 "Attack/Decay")))))

    (:restriction (OSZ 4)
     :parameter "Envelope Depth"
     :nrpn #.(+ 3 210)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "-15HT…0…+15HT")))))

    (:restriction (OSZ 4)
     :parameter "Envelope Time"
     :nrpn #.(+ 3  214)
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "12ms…6,15 (Decay Lin/Exp); 7,5ms…3,85 (Attack/Decay)")))))

    (:restriction (OSZ 4)
     :parameter "Noise Modulation"
     :nrpn 192
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 4)
     :parameter "Main Tune"
     :nrpn 8
     :direction S/R
     :type pot
     :data-format word
     :explanation ((word "Value" ((range 0 511 "0…+2 Octaves")))))

    (:restriction (OSZ 4)
     :parameter "Wave Preset A/B"
     :nrpn 193
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 38)))))

    (:restriction (OSZ 4)
     :parameter "Fine Tune"
     :nrpn 189
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "0..1HT")))))

    (:restriction (OSZ 4)
     :parameter "Velocity"
     :nrpn 190
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "-30HT…0…+30HT")))))

    (:restriction (OSZ 4)
     :parameter "KBD Scale"
     :nrpn 191
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "-100%…0…+100%")))))

    (:restriction (OSZ 4)
     :parameter "Octave/Mode"
     :nrpn 194
     :direction S/R
     :type switch
     :data-format XXXXXSOO
     :explanation ((S "Ringmod"
                     ((1 "Clean")
                      (0 "Ringmod")))
                   (OO "Dest"
                    ((#b00 "Low")
                     (#b01 "Mid")
                     (#b10 "High")))))

    (:restriction (OSZ 4)
     :parameter "A/B Mix Settings"
     :nrpn 195
     :direction S/R
     :type switch
     :data-format MMMVSXXX
     :explanation ((MMM "Mode"
                    ((#b000 "Off")
                     (#b100 "A->B")
                     (#b101 "A->B->A")
                     (#b111 "LFO")))
                   (V "Mix-Velocity"
                    ((1 "On")
                     (0 "Off")))
                   (S "Mix-Swap"
                     ((1 "On")
                      (0 "Off")))))

    (:restriction (OSZ 4)
     :parameter "A/B Mix"
     :nrpn 196
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255)))))

    (:restriction (OSZ 4)
     :parameter "A/B Mix Rate"
     :nrpn 197
     :direction S/R
     :type pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "Mode:LFO : 100Hz…0,125Hz; Mode:A->B: 5ms…4s; Mode:A->B->A: 10ms…8s")))))


    ;; VCF 1/2 DF1/2

    (:restriction (VCF 1/2 DF 1/2)
     :parameter "LFO Source"
     :nrpn 100
     :direction S/R
     :type switch
     :data-format DDCCBBAA
     :explanation ((DD "DF2-LFO-Source"
                     ((#b11 "LFO-DF1")
                      (#b10 "LFO-VCF1")
                      (#b01 "LFO-DF2")))
                   (CC "DF1-LFO-Source"
                    ((#b11 "LFO-VCF2")
                     (#b10 "LFO-VCF1")
                     (#b00 "LFO-DF1")))
                   (BB "VCF1-LFO-Source"
                    ((#b11 "LFO-VCF1+2")
                     (#b01 "LFO-VCF2")
                     (#b00 "LFO-VCF1")))
                   (AA "VCF2-LFO-Source"
                    ((#b11 "LFO-VCF1+2")
                     (#b01 "LFO-VCF1")
                     (#b00 "LFO-VCF2")))))

    ;; VCF 1/2

    (:restriction (VCF 1/2)
     :parameter "Input Source"
     :nrpn 79
     :direction S/R
     :type switch
     :data-format DDBCCBAA
     :explanation ((DD "VCF2 Input B"
                     ((#b10 "Osz1")
                      (#b01 "Osz2")
                      (#b11 "Osz3")
                      (#b00 "Osz4")))
                   (CC "VCF2 Input A"
                    ((#b10 "Osz1")
                     (#b01 "Osz2")
                     (#b11 "Osz3")
                     (#b00 "Osz4")))
                   (BB "VCF1 Input B"
                    ((#b10 "Osz1")
                     (#b01 "Osz2")
                     (#b11 "Osz3")
                     (#b00 "Osz4")))
                   (AA "VCF1 Input A"
                    ((#b10 "Osz1")
                     (#b01 "Osz2")
                     (#b11 "Osz3")
                     (#b00 "Osz4")))))

    (:restriction (VCF 1/2)
     :parameter "Input B Filter/Level Mod"
     :nrpn 81
     :direction S/R
     :type switch/m
     :data-format ABCDFFMM
     :explanation ((A "VCF2-Filter Mod"
                     ((1 "On")
                      (0 "Off")))
                   (B "VCF1-Filter Mod"
                     ((1 "On")
                      (0 "Off")))
                   (C "VCF2-Lev.Mod Vel."
                    ((1 "On")
                     (0 "Off")))
                   (D "VCF1-Lev.Mod Vel."
                    ((1 "On")
                     (0 "Off")))
                   (FF "VCF2-Lev.Mod ENV"
                    ((#b00 "Off")
                     (#b10 "Attack")
                     (#b01 "Decay")
                     (#b11 "LFO")))
                   (MM "VCF1-Lev.Mod ENV"
                    ((#b00 "Off")
                     (#b10 "Attack")
                     (#b01 "Decay")
                     (#b11 "LFO")))))

    (:restriction (VCF 1/2)
     :parameter "VCF12 Decay2"
     :nrpn 61
     :direction S/R
     :type switch
     :data-format BAXXXXXX
     :explanation ((B "VCF1 Decay2"
                     ((1 "On")
                      (0 "Off")))
                   (A "VCF2 Decay2"
                     ((1 "On")
                      (0 "Off")))))

    (:restriction (VCF 1/2)
     :parameter "VCF12 LFO-Sync-Mode"
     :nrpn 31
     :direction S/R
     :type m
     :data-format BBBBAAAA
     :explanation ((BBBB "VCF2-Sync Mode"
                    ((#b0000 "Intern (LFO Time Pot.)")
                     (#b0001 "Midi-Clock /32")
                     (#b0010 "Midi-Clock /16")
                     (#b0011 "Midi-Clock /16.")
                     (#b0100 "Midi-Clock /8")
                     (#b0101 "Midi-Clock /8.")
                     (#b0110 "Midi-Clock /4")
                     (#b0111 "Midi-Clock /4.")
                     (#b1000 "Midi-Clock /2")
                     (#b1001 "Midi-Clock /2.")
                     (#b1010 "Midi-Clock /1")
                     (#b1011 "Midi-Clock /1.")
                     (#b1100 "Midi-Clock *2")
                     (#b1101 "Midi-Clock *2.")))
                   (BBBB "VCF1-Sync Mode"
                    ((#b0000 "Intern (LFO Time Pot.)")
                     (#b0001 "Midi-Clock /32")
                     (#b0010 "Midi-Clock /16")
                     (#b0011 "Midi-Clock /16.")
                     (#b0100 "Midi-Clock /8")
                     (#b0101 "Midi-Clock /8.")
                     (#b0110 "Midi-Clock /4")
                     (#b0111 "Midi-Clock /4.")
                     (#b1000 "Midi-Clock /2")
                     (#b1001 "Midi-Clock /2.")
                     (#b1010 "Midi-Clock /1")
                     (#b1011 "Midi-Clock /1.")
                     (#b1100 "Midi-Clock *2")
                     (#b1101 "Midi-Clock *2.")))))

    ;; DF 1/2

    (:restriction (DF 1/2)
     :parameter "Input Source"
     :nrpn 80
     :direction S/R
     :type switch
     :data-format DDCCBBAA
     :explanation ((AA "DF1 Input A"
                    ((#b00 "Osz1")
                     (#b01 "Osz2")
                     (#b10 "Osz3")
                     (#b11 "Osz4")))
                   (BB "DF1 Input B"
                    ((#b00 "Osz1")
                     (#b01 "Osz2")
                     (#b10 "Osz3")
                     (#b11 "Osz4")))
                   (CC "DF2 Input A"
                    ((#b00 "Osz1")
                     (#b01 "Osz2")
                     (#b10 "Osz3")
                     (#b11 "Osz4")))
                   (DD "DF2 Input B"
                     ((#b00 "Osz1")
                      (#b01 "Osz2")
                      (#b10 "Osz3")
                      (#b11 "Osz4")))))

    (:restriction (DF 1/2)
     :parameter "Resonance"
     :nrpn 131
     :direction S/R
     :type switch
     :data-format XXBBBAAA
     :explanation ((AAA "Reso-DF1"
                    ((range 0 7 "Min…Max")))
                   (BBB "Reso-DF2"
                    ((range 0 7 "Min…Max")))))

    (:restriction (DF 1/2)
     :parameter "ENV-Mode/DF1->2 Assign"
     :nrpn 130
     :direction S/R
     :type switch
     :data-format XXXXXCBA
     :explanation ((C "Assign DF1->DF2 Settings"
                    ((1 "On")
                     (0 "Off")))
                   (B "ENV-Mode DF2"
                     ((0 "ENV")
                      (1 "Ramp")))
                   (A "ENV-Mode DF1"
                     ((0 "ENV")
                      (1 "Ramp")))))

    ;; VCF 1

    (:restriction (VCF 1)
     :parameter "Input Level A"
     :nrpn 82
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 1)
     :parameter "Input Level B"
     :nrpn 83
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 1)
     :parameter "Input Level DF1 Out"
     :nrpn 96
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 1)
     :parameter "Level B Mod Time Out"
     :nrpn 86
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255
                                   "ENV Attack/Decay: 12,5ms…25s; ENV LFO: 40Hz…0,02Hz")))))

    (:restriction (VCF 1)
     :parameter "Filter Mode (LP-BP-HP)"
     :nrpn 76
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "LP…BP…HP")))))

    (:restriction (VCF 1)
     :parameter "Cutoff"
     :nrpn 3
     :direction S/R
     :type Pot
     :data-format word
     :explanation ((word "Value" ((range 0 511)))))

    (:restriction (VCF 1)
     :parameter "Resonance"
     :nrpn 75
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 1)
     :parameter "Key Follow"
     :nrpn 90
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-216%…Off…+216%")))))

    (:restriction (VCF 1)
     :parameter "Velocity"
     :nrpn 88
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (VCF 1)
     :parameter "ENV Depth"
     :nrpn 1
     :direction S/R
     :type Pot
     :data-format word
     :explanation ((word "Value" ((range 0 511 "-Max…Off…+Max")))))

    (:restriction (VCF 1)
     :parameter "Trigger Delay"
     :nrpn 69
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 1)
     :parameter "Attack"
     :nrpn 40
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "1ms…10s")))))

    (:restriction (VCF 1)
     :parameter "Decay 1"
     :nrpn 41
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Decay2 Off: 2ms…20s; Decay2 On: 1ms…5s")))))

    (:restriction (VCF 1)
     :parameter "Sustain"
     :nrpn 42
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 1)
     :parameter "Decay 2"
     :nrpn 62
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "2ms…21s")))))

    (:restriction (VCF 1)
     :parameter "Release"
     :nrpn 43
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "2ms…20s")))))

    (:restriction (VCF 1)
     :parameter "ENV Velocity-Depth"
     :nrpn 66
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (VCF 1)
     :parameter "ENV Destination (Velocity)"
     :nrpn 64
     :direction S/R
     :type Switch
     :data-format XXXXXDDD
     :explanation ((DDD "Destination"
                    ((#b000 "Off")
                     (#b001 "Depth")
                     (#b010 "Attack")
                     (#b100 "Delay")))))

    (:restriction (VCF 1)
     :parameter "LFO Control/Mode"
     :nrpn 101
     :direction S/R
     :type Switch
     :data-format NXFFVFMM
     :explanation ((N "LFO-Note Reset OnOff"
                    ((1 "On")
                     (0 "Off")))
                   (V "LFO-Depth Vel OnOff"
                    ((1 "On")
                     (0 "Off")))
                   (FFF "Ramp"
                    ((#b000 "Off")
                     (#b110 "Fade In")
                     (#b010 "Fade Out")
                     (#b001 "CLK")))
                   (MM "Mode"
                    ((#b00 "Sine")
                     (#b01 "Triangle")
                     (#b11 "Square")
                     (#b10 "S/H")))))

    (:restriction (VCF 1)
     :parameter "LFO Depth"
     :nrpn 103
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (VCF 1)
     :parameter "LFO Rate"
     :nrpn 105
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255
                      "Sine,Ramp=Off/Fade: 0,01Hz…25Hz; Triangle,Ramp=Off/Fade: 0,01Hz…25Hz; Square,Ramp=Off/Fade: 0,08Hz…200Hz; S/H,Ramp=Off/Fade: 0,04Hz…100Hz")))))

    (:restriction (VCF 1)
     :parameter "LFO Time"
     :nrpn 107
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255
                      "Fade-Time(Ramp=Fade): 60ms…10s; CLK-Rate(Ramp=CLK): 60Hz…0,15Hz")))))




    ;; VCF 2

    (:restriction (VCF 2)
     :parameter "Input Level A"
     :nrpn 84
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 2)
     :parameter "Input Level B"
     :nrpn 85
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 2)
     :parameter "Input Level DF1 Out"
     :nrpn 97
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 2)
     :parameter "Level B Mod Time Out"
     :nrpn 87
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255
                                   "ENV Attack/Decay: 12,5ms…25s; ENV LFO: 40Hz…0,02Hz")))))

    (:restriction (VCF 2)
     :parameter "Filter Mode (LP-BP-HP)"
     :nrpn 78
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "LP…BP…HP")))))

    (:restriction (VCF 2)
     :parameter "Cutoff"
     :nrpn 4
     :direction S/R
     :type Pot
     :data-format word
     :explanation ((word "Value" ((range 0 511)))))

    (:restriction (VCF 2)
     :parameter "Resonance"
     :nrpn 77
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 2)
     :parameter "Key Follow"
     :nrpn 91
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-216%…Off…+216%")))))

    (:restriction (VCF 2)
     :parameter "Velocity"
     :nrpn 89
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (VCF 2)
     :parameter "ENV Depth"
     :nrpn 2
     :direction S/R
     :type Pot
     :data-format word
     :explanation ((word "Value" ((range 0 511 "-Max…Off…+Max")))))

    (:restriction (VCF 2)
     :parameter "Trigger Delay"
     :nrpn 70
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 2)
     :parameter "Attack"
     :nrpn 44
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "1ms…10s")))))

    (:restriction (VCF 2)
     :parameter "Decay 1"
     :nrpn 45
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Decay2 Off: 2ms…20s; Decay2 On: 1ms…5s")))))

    (:restriction (VCF 2)
     :parameter "Sustain"
     :nrpn 46
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (VCF 2)
     :parameter "Decay 2"
     :nrpn 63
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "2ms…21s")))))

    (:restriction (VCF 2)
     :parameter "Release"
     :nrpn 47
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "2ms…20s")))))

    (:restriction (VCF 2)
     :parameter "ENV Velocity-Depth"
     :nrpn 67
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (VCF 2)
     :parameter "ENV Destination (Velocity)"
     :nrpn 65
     :direction S/R
     :type Switch
     :data-format XXXXXDDD
     :explanation ((DDD "Destination"
                    ((#b000 "Off")
                     (#b001 "Depth")
                     (#b010 "Attack")
                     (#b100 "Delay")))))

    (:restriction (VCF 2)
     :parameter "LFO Control/Mode"
     :nrpn 102
     :direction S/R
     :type Switch
     :data-format NXFFVFMM
     :explanation ((N "LFO-Note Reset OnOff"
                    ((1 "On")
                     (0 "Off")))
                   (V "LFO-Depth Vel OnOff"
                    ((1 "On")
                     (0 "Off")))
                   (FFF "Ramp"
                    ((#b000 "Off")
                     (#b110 "Fade In")
                     (#b010 "Fade Out")
                     (#b001 "CLK (Sync OFF)")
                     (#b101 "CLK+Sync VCF1-LFO")))
                   (MM "Mode"
                    ((#b00 "Sine")
                     (#b01 "Triangle")
                     (#b11 "Square")
                     (#b10 "S/H")))))

    (:restriction (VCF 2)
     :parameter "LFO Depth"
     :nrpn 104
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (VCF 2)
     :parameter "LFO Rate"
     :nrpn 106
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255
                      "Sine,Ramp=Off/Fade: 0,01Hz…25Hz; Triangle,Ramp=Off/Fade: 0,01Hz…25Hz; Square,Ramp=Off/Fade: 0,08Hz…200Hz; S/H,Ramp=Off/Fade: 0,04Hz…100Hz")))))

    (:restriction (VCF 2)
     :Parameter "LFO Time"
     :nrpn 108
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255
                      "Fade-Time(Ramp=Fade): 60ms…10s; CLK-Rate(Ramp=CLK): 60Hz…0,15Hz")))))


    (:restriction (VCF 2)
     :Parameter "ENV-Trigger-Repeat-Rate"
     :nrpn 68
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "200Hz…0,4Hz")))))

    ;; DF 1

    (:restriction (DF 1)
     :parameter "Input Level A"
     :nrpn 92
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (DF 1)
     :parameter "Input Level B"
     :nrpn 93
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (DF 1)
     :parameter "Cutoff"
     :nrpn 5
     :direction S/R
     :type Pot
     :data-format word
     :explanation ((word "Value" ((range 0 511)))))

    (:restriction (DF 1)
     :parameter "Space"
     :nrpn 118
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (DF 1)
     :parameter "Key Follow"
     :nrpn 119
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-100%…Off…+100%")))))

    (:restriction (DF 1)
     :parameter "Velocity"
     :nrpn 120
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (DF 1)
     :parameter "ENV Depth"
     :nrpn 121
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (DF 1)
     :parameter "LFO Depth"
     :nrpn 122
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (DF 1)
     :parameter "LFO Rate"
     :nrpn 123
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "0,01Hz…23Hz")))))

    (:restriction (DF 1)
     :Parameter "ENV Trigger Delay"
     :nrpn 140
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Off…2,8s")))))

    (:restriction (DF 1)
     :Parameter "Ramp Nr"
     :nrpn 142
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 31 "1…32")))))

    (:restriction (DF 1)
     :Parameter "ENV Mode:ADR : ENV Attack; ENV Mode:Ramp : CLK-Rate"
     :nrpn 134
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "ENV Mode:ADR : 1ms…21s; ENV Mode:Ramp : Abhängig von Ramp-Nr")))))

    (:restriction (DF 1)
     :Parameter "ENV Mode:ADR : ENV Decay; ENV Mode:Ramp : Quantize"
     :nrpn 136
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "ENV Mode:ADR : 10ms…25s; ENV Mode:Ramp : 0: Sqr. 64:Mix1 128:Mix2 192:Mix3 224:Ramp")))))

    (:restriction (DF 1)
     :Parameter "ENV Mode:ADR : ENV Release; ENV Mode:Ramp : #Repeats"
     :nrpn 138
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "ENV Mode:ADR : 10ms…25s; ENV Mode:Ramp : ((0 1) (32 2) (64 3) (96 4) (128 5) (160 6) (192 7) (200 8) (240 continuous))")))))

    (:restriction (DF 1)
     :Parameter "Mode/Mulator-Settings"
     :nrpn 132
     :direction S/R
     :type Switch
     :data-format DEVLFXMM
     :explanation ((D "ENV-Depth-Vel"
                    ((1 "On")
                     (0 "Off")))
                   (E "ENV-Dest"
                    ((0 "Cutoff")
                     (1 "Space")))
                   (V "Vel-Dest"
                    ((0 "Cutoff")
                     (1 "Space")))
                   (L "LFO-Dest"
                    ((0 "Cutoff")
                     (1 "Space")))
                   (F "Filtermod B->A"
                    ((1 "On")
                     (0 "Off")))
                   (MM "Mode"
                    ((#b00 "LP/LP")
                     (#b01 "LP/HP")
                     (#b10 "BP/BP")
                     (#b11 "HP/HP")))))

    ;; DF2 ???

    (:restriction (DF 2)
     :parameter "Input Level A"
     :nrpn 94
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (DF 2)
     :parameter "Input Level B"
     :nrpn 95
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (DF 2)
     :parameter "Cutoff"
     :nrpn 6
     :direction S/R
     :type Pot
     :data-format word
     :explanation ((word "Value" ((range 0 511)))))

    (:restriction (DF 2)
     :parameter "Space"
     :nrpn 124
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (DF 2)
     :parameter "Key Follow"
     :nrpn 125
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-100%…Off…+100%")))))

    (:restriction (DF 2)
     :parameter "Velocity"
     :nrpn 126
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (DF 2)
     :parameter "ENV Depth"
     :nrpn 127
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (DF 2)
     :parameter "LFO Depth"
     :nrpn 128
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (DF 2)
     :parameter "LFO Rate"
     :nrpn 129
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "0,01Hz…23Hz")))))


    (:restriction (DF 2)                ; ???
     :Parameter "Assign DF1 Value"
     :nrpn 117
     :direction S/R
     :type Switch
     :data-format XXLEVKSC
     :explanation ((L "LFO-Depth"
                    ((1 "On")
                     (0 "Off")))
                   (E "ENV-Depth"
                    ((1 "On")
                     (0 "Off")))
                   (V "Velocty"
                    ((1 "On")
                     (0 "Off")))
                   (K "Key Follow"
                    ((1 "On")
                     (0 "Off")))
                   (S "Space"
                     ((1 "On")
                      (0 "Off")))
                   (C "Cutoff"
                    ((1 "On")
                     (0 "Off")))))

    (:restriction (DF 2)                ; ???
     :parameter "ENV Trigger Delay"
     :nrpn 141
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Off…2,8S")))))

    (:restriction (DF 2)                ; ???
     :parameter "Ramp Nr"
     :nrpn 143
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 31 "1…32")))))

    (:restriction (DF 2)
     :Parameter "ENV Mode:ADR : ENV Attack; ENV Mode:Ramp : CLK-Rate"
     :nrpn 135
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "ENV Mode:ADR : 1ms…21s; ENV Mode:Ramp : Abhängig von Ramp-Nr")))))

    (:restriction (DF 2)
     :Parameter "ENV Mode:ADR : ENV Decay; ENV Mode:Ramp : Quantize"
     :nrpn 137
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "ENV Mode:ADR : 10ms…25s; ENV Mode:Ramp : 0: Sqr. 64:Mix1 128:Mix2 192:Mix3 224:Ramp")))))

    (:restriction (DF 2)
     :Parameter "ENV Mode:ADR : ENV Release; ENV Mode:Ramp : #Repeats"
     :nrpn 139
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "ENV Mode:ADR : 10ms…25s; ENV Mode:Ramp : ((0 1) (32 2) (64 3) (96 4) (128 5) (160 6) (192 7) (200 8) (240 continuous))")))))

    (:restriction (DF 2)
     :Parameter "Mode/Mulator-Settings"
     :nrpn 133
     :direction S/R
     :type Switch
     :data-format DEVLFXMM
     :explanation ((D "ENV-Depth-Vel"
                    ((1 "On")
                     (0 "Off")))
                   (E "ENV-Dest"
                    ((0 "Cutoff")
                     (1 "Space")))
                   (V "Vel-Dest"
                    ((0 "Cutoff")
                     (1 "Space")))
                   (L "LFO-Dest"
                    ((0 "Cutoff")
                     (1 "Space")))
                   (F "Filtermod B->A"
                    ((1 "On")
                     (0 "Off")))
                   (MM "Mode"
                    ((#b00 "LP/LP")
                     (#b01 "LP/HP")
                     (#b10 "BP/BP")
                     (#b11 "HP/HP")))))

    ;; Group12

    (:restriction (group 1/2)
     :Parameter "Input/Output"
     :nrpn 109
     :direction S/R
     :type Switch
     :data-format GFEDCCBA
     :explanation ((G "Input DF2-Mix A+B"
                    ((1 "On")
                     (0 "Off")))
                   (F "Input DF1-Mix A+B"
                    ((1 "On")
                     (0 "Off")))
                   (E "VCF2 Out-Invert"
                    ((1 "On")
                     (0 "Off")))
                   (D "VCF2 Out"
                    ((1 "On")
                     (0 "Off")))
                   (CC "VCF3-Input"
                    ((#b00 "Osz1")
                     (#b01 "Osz2")
                     (#b10 "Osz3")
                     (#b11 "Osz4")))
                   (B "VCF1 Out-Invert"
                     ((1 "On")
                      (0 "Off")))
                   (A "VCF1 Out"
                     ((1 "On")
                      (0 "Off")))))

    (:restriction (group 1/2)
     :Parameter "VCF3 Group 1/2 Out"
     :nrpn 110
     :direction S/R
     :type Switch
     :data-format BAXXXXX1
     :explanation ((B "Group 2 VCF3"
                     ((1 "On")
                      (0 "Off")))
                   (A "Group 1 VCF3"
                     ((1 "On")
                      (0 "Off")))))

    (:restriction (group 1/2)
     :Parameter "DF 1/2 Group 1/2 Out"
     :nrpn 114
     :direction S/R
     :type Switch
     :data-format XXFEDCBA
     :explanation ((F "Group 2 DF 2 Dist-Vel."
                    ((1 "On")
                     (0 "Off")))
                   (E "Group 1 DF 1 Dist-Vel."
                    ((1 "On")
                     (0 "Off")))
                   (D "Group 2 DF 2 Invert"
                    ((1 "On")
                     (0 "Off")))
                   (C "Group 1 DF 1 Invert"
                    ((1 "On")
                     (0 "Off")))
                   (B "Group 2 DF2 Out"
                     ((1 "On")
                      (0 "Off")))
                   (A "Group 1 DF 1 Out"
                     ((1 "On")
                      (0 "Off")))))

    (:restriction (group 1/2)
     :parameter "Group 1 Velocity"
     :nrpn 38
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (group 1/2)
     :parameter "Group 2 Velocity"
     :nrpn 39
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "-Max…Off…+Max")))))

    (:restriction (group 1/2)
     :parameter "Group 1 Out DF 1 Distortion"
     :nrpn 115
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (group 1/2)
     :parameter "Group 2 Out DF 2 Distortion"
     :nrpn 116
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (group 1/2)
     :parameter "Group 1 Out DF 1 Level"
     :nrpn 98
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (group 1/2)
     :parameter "Group 2 Out DF 2 Level"
     :nrpn 99
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (group 1/2)
     :parameter "Group 1 Out VCF3 Level"
     :nrpn 111
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (group 1/2)
     :parameter "Group 2 Out VCF3 Level"
     :nrpn 112
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (group 1/2)
     :parameter "VCF3 Cutoff"
     :nrpn 113
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    ;; Group 1/2 Level Mod

    (:restriction (group 1/2)
     :parameter "Min Man/Fade-Controls"
     :nrpn 48
     :direction S/R
     :type Switch
     :data-format XXFFMSXX
     :explanation ((FF "Fade-Mode"
                    ((#b01 "2->1")
                     (#b11 "2->1->2")
                     (#b10 "2->Min->2")
                     (#b00 "2->Min")))
                   (M "Mix-Mode"
                    ((1 "Man")
                     (0 "Fade")))
                   (S "Fade-Swap 1/2"
                     ((1 "On")
                      (0 "Off")))))

    (:restriction (group 1/2)
     :parameter "Group 1/2 Man-Mix"
     :nrpn 49
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Group1…Group2")))))

    (:restriction (group 1/2)
     :parameter "Group 1/2 Fade-Time"
     :nrpn 50
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value"
                    ((range 0 255 "Fade-Mode 2->1,2->Min : 5ms…4s; Fade-Mode 2->1->2 : 10ms…8s; Fade-Mode 2->Min->2 : 10ms…8s")))))

    (:restriction (group 1/2)
     :parameter "Group 1/2 Fade-Delay"
     :nrpn 51
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "0…4,5s")))))

    (:restriction (group 1/2)
     :parameter "LFO-Controls"
     :nrpn 57
     :direction S/R
     :type Switch
     :data-format DDDRVXXX
     :explanation ((DDD "Group-Dest"
                    ((#b000 "Off")
                     (#b100 "1")
                     (#b010 "2")
                     (#b110 "1+2")
                     (#b111 "1+ 2-")))
                   (R "LFO-Note-Reset"
                    ((1 "On")
                     (0 "Off")))
                   (V "LFO-Depth-Velocity"
                    ((1 "On")
                     (0 "Off")))))

    (:restriction (group 1/2)
     :parameter "Mix-LFO-Depth"
     :nrpn 58
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (group 1/2)
     :parameter "Mix-LFO-Rate"
     :nrpn 59
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "0…4,5s")))))

    (:restriction (group 1/2)
     :parameter "Panorama-Controls"
     :nrpn 52
     :direction S/R
     :type Switch
     :data-format SDDVMMXD
     :explanation ((S "Voice-Spread"
                     ((1 "On")
                      (0 "Off")))
                   (DDD "Group-Dest"
                    ((#b000 "Off")
                     (#b100 "1")
                     (#b010 "2")
                     (#b110 "1+2")
                     (#b111 "1+ 2-")))
                   (V "LFO-Depth-Velocity"
                    ((1 "On")
                     (0 "Off")))
                   (MM "LFO-Mode"
                    ((#b01 "L>R")
                     (#b10 "L>R>L")
                     (#b00 "Tri-Continuous")))))

    (:restriction (group 1/2)
     :parameter "Panorama-LFO-Depth"
     :nrpn 55
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (group 1/2)
     :parameter "Panorama-LFO-Rate"
     :nrpn 56
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "LFO-Mode:Continuous : 0,025Hz…10Hz; LFO-Mode:L>R : 20s…40ms; LFO-Mode:L>R>L : 40s…80ms")))))

    (:restriction (group 1/2)
     :parameter "Group 1 Pan-Offset"
     :nrpn 53
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Left…Right")))))

    (:restriction (group 1/2)
     :parameter "Group 2 Pan-Offset"
     :nrpn 54
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "Left…Right")))))

    ;; Master ENV/VCA-VCF12 Retrigg

    (:restriction (master-env/vca)
     :parameter "Soft/VCA-VCF12-Retrigg"
     :nrpn 60
     :direction S/R
     :type Switch
     :data-format XXXXDCBA
     :explanation ((D "VCF1-Retrigg"
                    ((1 "On")
                     (0 "Off")))
                   (C "VCF2-Retrigg"
                    ((1 "On")
                     (0 "Off")))
                   (B "VCA-Soft"
                     ((1 "On")
                      (0 "Off")))
                   (A "VCA-Retrigg"
                     ((1 "On")
                      (0 "Off")))))

    (:restriction (master-env/vca)
     :parameter "Sound Volume"
     :nrpn 32
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (master-env/vca)
     :parameter "Attack"
     :nrpn 33
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "1ms…10ms")))))

    (:restriction (master-env/vca)
     :parameter "Decay"
     :nrpn 34
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "2ms…20s")))))

    (:restriction (master-env/vca)
     :parameter "Sustain"
     :nrpn 35
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    (:restriction (master-env/vca)
     :parameter "Release"
     :nrpn 36
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255 "2ms…20s")))))

    (:restriction (master-env/vca)
     :parameter "Release-Level"
     :nrpn 37
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 255)))))

    ;; Glide/Bend

    (:restriction (glide/bend)
     :parameter "Pitch Bend"
     :nrpn 148
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 2 "-12HT")
                                  (range 3 13 "-11HT…-1HT")
                                  (range 14 14 "Off")
                                  (range 15 28 "+1HT…+11HT")
                                  (range 29 31 "+12HT")))))

    (:restriction (glide/bend)
     :parameter "Glide Time OSZ"
     :nrpn 147
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 111 "1,2s/Octave…12ms/Octave")
                                  (range 112 143 "Off")
                                  (range 144 255 "16ms…4s Fix")))))

    (:restriction (glide/bend)
     :parameter "Glide Time Filter"
     :nrpn 146
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 111 "1,2s/Octave…12ms/Octave")
                                  (range 112 143 "Off")
                                  (range 144 255 "16ms…4s Fix")))))

    (:restriction (glide/bend)
     :parameter "Glide Depth"
     :nrpn 71
     :direction S/R
     :type Pot
     :data-format byte
     :explanation ((byte "Value" ((range 0 127 "32HT…1HT")
                                  (range 128 255 "10%…100%")))))

    (:restriction (glide/bend)
     :parameter "Single Mode/Glide Mode"
     :nrpn 145
     :direction S/R
     :type Switch
     :data-format PTMXUXXF
     :explanation ((P "Single Mode"
                     ((1 "Mono")
                      (0 "Poly")))
                   (T "Env Trigg"
                    ((1 "On")
                     (0 "Off")))
                   (M "Glide Mode"
                    ((0 "Norm")
                     (1 "Legato")))
                   (U "Mono-Unisono"
                     ((1 "On")
                      (0 "Off")))
                   (F "Indiv.Filter Glide"
                    ((1 "On")
                     (0 "Off")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun little-quad (a b)
  (check-type a (integer 0 15))
  (check-type b (integer 0 15))
  (dpb b (byte 4 4) a))

(defparameter *schmidt-code* " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

(defun schmidt-char-code (c)
  (or (position c *schmidt-code*)
      (error "Character ~C cannot be encoded for Schmidt synthesizer." c)))

(defun schmidt-code-char (b)
  (let ((code *schmidt-code*)
        (b (mod b 128)))
    (if (< -1 b (length code))
        (aref code b)
        #\?)))


(defparameter *data-offset* 7)

(defun bank-from-sysex (sysex)
  (let ((bank (coerce (loop
                        :for i :from *data-offset* :below (- (length sysex) 5) :by 2
                        :collect (little-quad (aref sysex i) (aref sysex (+ i 1))))
                      '(vector (unsigned-byte 8)))))
    (unless (valid-bank-checksum-p bank)
      (cerror "Ignore"
              "Invalid Bank Checksum: Expected ~D, got ~D"
              (bank-compute-checksum bank) (bank-checksum bank)))
    bank))

(defun bank-checksum (bank)
  (check-type bank vector)
  (assert (= 32768 (length bank)))
  (aref bank (1- (length bank))))

(defun bank-compute-checksum (bank)
  (loop
    :for i :below (1- (length bank))
    :for sum := (aref bank i) :then (logand #xff (+ (aref bank i) sum))
    :finally (return sum)))

(defun valid-bank-checksum-p (bank)
  (= (bank-compute-checksum bank) (bank-checksum bank)))

(defun update-bank-checksum (bank)
  (check-type bank vector)
  (assert (= 32768 (length bank)))
  (setf (aref bank (1- (length bank))) (bank-compute-checksum bank)))



(defun program (bank index)
  (check-type bank        (vector 32768))
  (check-type index       (integer 0 127))
  (subseq bank (* 256 index) (* 256 (1+ index))))

(defun (setf program) (new-program bank index)
  (check-type new-program (vector 256))
  (check-type bank        (vector 32768))
  (check-type index       (integer 0 127))
  (replace bank new-program :start1 (* 256 index))
  (update-bank-checksum banke)
  new-program)


(defun program-name (program)
  (let ((bytes (nsubseq program 11 27)))
    (concatenate 'string
                 (map 'string 'schmidt-code-char bytes)
                 (loop :repeat 2
                       :for s :from 0 :by 8
                       :collect (loop
                                  :for i :from 0
                                  :for j :from 7 :downto 0
                                  :for b := (ldb (byte 1 7) (aref bytes (+ s i)))
                                  :for val := (dpb b (byte 1 j) 0) :then (dpb b (byte 1 j) val)
                                  :finally (return (schmidt-code-char val)))))))

(defun (setf program-name) (new-name program)
  (let ((new-name (map 'vector (function schmidt-char-code)
                    (format nil "~18A" (subseq new-name 0 (min (length new-name) 18))))))
    (loop
      :for i :from 0 :to 1
      :for b := (aref new-name (+ 16 i))
      :do (loop :for j :from (* i 8)
                :for k :from 7 :downto 0
                :do (setf (aref new-name j)
                          (dpb (ldb (byte 1 k) b) (byte 1 7) (aref new-name j)))))
    (replace program new-name :start1 11 :end1 27 :start2 0)
    new-name))


;;----------------------------------------------------------------------

(defun decode-pot-word (program-bytes specification)
  (destructuring-bind (&key restriction parameter nrpn direction type data-format explanation)
      specification
    (declare (ignore restriction parameter direction type explanation))
    (assert (eq 'word data-format))
    (dpb (ldb (byte 1 (1- nrpn)) (aref program-bytes 8)) (byte 1 8) (aref program-bytes (1- nrpn)))))

(defun decode-pot-byte (program-bytes specification)
  (destructuring-bind (&key restriction parameter nrpn direction type data-format explanation)
      specification
    (declare (ignore restriction parameter direction type explanation))
    (assert (eq 'byte data-format))
    (aref program-bytes (1- nrpn))))

(defun decode-switch-fields (byte data-format explanation)
  (check-type data-format symbol)
  (let ((bits (loop :for bit :from 7 :downto 0
                    :for chr :in (explode data-format)
                    :for key := (intern (string chr))
                    :collect (cons key bit))))
    (assert (= 8 (length bits)))
    (let ((fields (sort (remove 'x bits :key (function car))
                        (function char<=) :key (lambda (k) (character (car k))))))
      (loop
        :for (key name encoding) :in explanation
        :for bits := (remove (intern (string (aref (string key) 0))) fields :key (function car) :test-not (function eql))
        :collect (loop
                   :for (nil . bit) :in (sort bits (function <) :key (function cdr))
                   :for e :from 0
                   :for val := (ldb (byte 1 bit) byte)
                     :then (dpb (ldb (byte 1 bit) byte) (byte 1 e) val)
                   :finally (return (list name (or (second (assoc val encoding)) val))))))))

(defun decode-switch (program-bytes specification)
  (destructuring-bind (&key restriction parameter nrpn direction type data-format explanation)
      specification
    (declare (ignore restriction parameter direction type))
    (decode-switch-fields (aref program-bytes (1- nrpn))
                          data-format explanation)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dump-parameters (&optional (parameters *parameters*))
  (loop
    :with ps := (make-string 256 :initial-element #\_)
    :for parameter :in (sort (copy-list parameters) (function <) :key (lambda (parameter) (getf parameter :nrpn)))
    :for restriction := (getf parameter :restriction)
    :for name := (getf parameter :parameter)
    :for nrpn := (getf parameter :nrpn)
    :for type := (getf parameter :data-format)
      :initially (format t "~&Number of parameters: ~A~%" (length parameters))
    :do (format t "~3D: ~A ~A~%" nrpn restriction name)
        (setf (aref ps nrpn) (cond
                               ((char/= (aref ps nrpn) #\_) #\!)
                               ((eq type 'word)             #\W)
                               (t                           #\*)))
    :finally (loop :for i :from 11 :below 27
                   :do (setf (aref ps i) (if (char/= (aref ps i) #\_)
                                             #\!
                                             #\N)))
             (format t "~{~A~%~}~%" (loop :for i :from 0 :by 64 :below 256
                                          :collect (subseq ps i (+ i 64))))
             (return ps)))

(defun dump-program (bank prog program)
  (when bank (format t "Bank ~D " bank))
  (when prog (format t "Program ~D " prog))
  (format t "~A~%" (program-name program))
  (loop
    :with last-restriction := nil
    :for specification :in *parameters*
    :do (destructuring-bind (&key restriction parameter nrpn direction type data-format explanation)
            specification
          (declare (ignore nrpn direction type))
          (unless (equal last-restriction restriction)
            (setf last-restriction restriction)
            (format t "~S~%" restriction))
          (format t "    ~32A: ~A ~A~%" parameter
                  (case data-format
                    (byte      (decode-pot-byte program specification))
                    (word      (decode-pot-word program specification))
                    (otherwise (decode-switch program specification)))
                  (case data-format
                    ((byte word)      (or (remove nil (mapcar (lambda (item) (fourth item))
                                                              (third (first explanation))))
                                          ""))
                    (otherwise        ""))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#|

(loop :for path :in (directory #P"~/Documents/SysEx Librarian/Schmidt-Single-bank-*.syx")
      :for b :from 0
      :collect (loop
                 :with bank := (bank-from-sysex (com.informatimago.common-lisp.cesarum.file:binary-file-contents
                                            path))
                 :for i :below 128
                 :do (dump-program b i (program bank i))))







(format t "~{ ~2,'0X~}"(list (schmidt-char-code #\K) (schmidt-char-code #\]))) 0B 57

"PJB Simple Sound 1"
(mapcar '-
        (map 'list 'char-code "PJB Simple Sound 1")
        '(16 10 2 0 19 35 39 42 38 31 128 147 41 175 168 30 0 183 55 0))

(map 'string 'schmidt-code-char '(16 10 2 0 19 35 39 42 38 31 128 147 41 175 168 30 0 183 55 0))
"PJB Simple Sound 22 "

(map 'string 'schmidt-code-char '(9 40 35 46 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
"Init                "



(length " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz 0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
95
(length "PJB Simple Sound 1")

(remove-duplicates (map 'list (lambda (parameter) (getf parameter :restriction))
                     *parameters*) :test (function equal))
((osz 123) (osz 1234) (osz 1) (osz 2) (osz 3) (osz 4) (vcf 1/2 df 1/2) (vcf 1/2) (df 1/2) (vcf 1) (vcf 2) (df 1) (df 2) (group 1/2) (master-env/vca) (glide/bend))


(dump-parameters)

(com.informatimago.common-lisp.cesarum.array:positions #\_
 (block nil
   (with-output-to-string (*standard-output*)
     (return (dump-parameters)))))
(0 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 144 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255)


(mapcar (lambda (path)
          (decode-pot-word
           (program (bank-from-sysex (com.informatimago.common-lisp.cesarum.file:binary-file-contents path))
                    8)
           '(:restriction (VCF 1)
             :parameter "Cutoff"
             :nrpn 3
             :direction S/R
             :type Pot
             :data-format word
             :explanation ((word "Value" ((range 0 511)))))))
        '(#P"~/Documents/SysEx Librarian/Schmidt-8-9-VCF1=0.syx"
          #P"~/Documents/SysEx Librarian/Schmidt-8-9-VCF1=255.syx"
          #P"~/Documents/SysEx Librarian/Schmidt-8-9-VCF1=511.syx"))



(loop
  :with bank := (bank-from-sysex (com.informatimago.common-lisp.cesarum.file:binary-file-contents
                             #P"~/Documents/SysEx Librarian/Schmidt-8-1-Vol=165.syx"
                             #-(and) #P"~/Documents/SysEx Librarian/Schmidt-8-9-VCF1=511.syx"
                             #-(and) #P"~/Documents/SysEx Librarian/Schmidt-Single-bank-1.syx"))
  :for i :below 128
  :for program := (program bank i)
  :collect (decode-pot-byte
            program
            '(:restriction (master-env/vca)
              :parameter "Sound Volume"
              :nrpn 32
              :direction S/R
              :type Pot
              :data-format byte
              :explanation ((byte "Value" ((range 0 255)))))))


(loop
  :with bank := (bank-from-sysex (com.informatimago.common-lisp.cesarum.file:binary-file-contents
                             #P"~/Documents/SysEx Librarian/Schmidt-Single-bank-1.syx"))
  :for i :below 128
  :for program := (program bank i)
  :do (print (decode-switch program
                            '(:restriction (OSZ 1)
     :parameter "Detune Fine/KBD Scale/Wave"
     :nrpn 150
     :direction S/R
     :type switch
     :data-format KKFXXWWW
     :explanation ((KK "Kbd Scale"
                    ((#b00 "Off")
                     (#b10 "1/4")
                     (#b11 "1/2")))
                   (F "Fine"
                    ((0 "On")
                     (1 "Off")))
                   (WWW "Wave"
                    ((#b001 "Square")
                     (#b010 "PW")
                     (#b101 "Saw")
                     (#b111 "Saw/PW")
                     (#b100 "Multi PWM")
                     (#b110 "Multi/PW")
                     (#b011 "Noise"))))))))

|#
