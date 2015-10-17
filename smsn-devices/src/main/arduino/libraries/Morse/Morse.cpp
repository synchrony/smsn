/*
  Morse.cpp
  Created by Joshua Shinavier, 2013-2014
  Released into the public domain.
*/

#include "Morse.h"

int morsePitch = 1760;
long timeUnit = 50;

char Morse::bits[50] = {
    0xd8,0xce,0xb7,0x79,0x0e,0xa5,0x34,0x57,0xc6,0xab,0x2c,0x08,0xe3,0xbc,0xef,0x39,0x86,0xb8,0x3a,0xbb,0x79,0x4e,0x73,
    0xf5,0xe5,0x1f,0x6a,0x44,0x52,0x6f,0x17,0x13,0x96,0xb3,0x34,0x4f,0x73,0xae,0xbe,0xfc,0x43,0x8d,0x48,0xea,0xed,0x62,
    0xc2,0xf3,0x3c,0xcf};
/*
int Morse::offsets[95] = {
    0,4,10,16,23,29,35,41,46,52,58,63,69,75,81,86,91,96,101,106,111,116,121,126,131,136,142,148,154,159,165,171,177,
    179,183,187,190,191,195,198,202,204,208,211,215,217,219,222,226,230,233,236,237,240,244,247,251,255,259,264,270,
    276,282,288,294,296,300,304,307,308,312,315,319,321,325,328,332,334,336,339,343,347,350,353,354,357,361,364,368,
    372,376,382,388,394,400};
*/

Morse::Morse(int morsePin, int (*stopTest)())
{
    _morsePin = morsePin;
    _stopTest = stopTest;
}

void Morse::morseOn() {
    tone(_morsePin, morsePitch);
}

void Morse::morseOff() {
    noTone(_morsePin);
}

void Morse::dit() {
    morseOn();
    delay(timeUnit);
}

void Morse::dah() {
    morseOn();
    delay(3 * timeUnit);
}

void Morse::interElementGap() {
    morseOff();
    delay(timeUnit);
}

void Morse::shortGap() {
    morseOff();
    delay(3 * timeUnit);
}

void Morse::mediumGap() {
    morseOff();
    delay(7 * timeUnit);
}

void Morse::beginSequence() {
    // insert a short gap at the beginning of the message, to separate it from any (non-Morse) tones which came before
    interElementGap();
}

void Morse::endSequence() {
    // end the last tone and insert a gap after the message, in case another message follows it
    mediumGap();
}

// TODO: use stopTest
void Morse::playMorseString(const char* message) {
    int firstLetterInWord = true;

    const char *cur = message;
    while (*cur) {
        char c = *cur;

        if (32 == c) {
            mediumGap();
            firstLetterInWord = true;
            cur++;
            continue;
        } else if (c < 32 || c > 127) {
            c = '?';
        }

        int offset = getOffset(c - 33);
        int nextOffset = getOffset(c + 1 - 33);

        if (firstLetterInWord) {
            firstLetterInWord = false;
        } else {
            shortGap();
        }

        int firstElementInLetter = true;
        for (int i = offset; i < nextOffset; i++) {
            if (firstElementInLetter) {
                firstElementInLetter = false;
            } else {
                interElementGap();
            }

            if (1 & (bits[i / 8] >> (i % 8))) {
                dit();
            } else {
                dah();
            }
        }

        cur++;
    }

    endSequence();
}

void Morse::playMorseInt(int d) {
    char printStr[64];
    sprintf(printStr, "%d", d);
    playMorseString(printStr);
}

// The following absurdity was made necessary by the difficulty of keeping the offsets array in RAM without
// confusing the running program.  Even though it is not a very large array, strange behavior would result even
// when Morse subroutines were not called.  Keeping the "array" in program space is effective as a workaround,
// although it could be done more elegantly than the below using PROGMEM.
int Morse::getOffset(int c) {
    switch (c) {
        case 0:
            return 0;
        case 1:
            return 4;
        case 2:
            return 10;
        case 3:
            return 16;
        case 4:
            return 23;
        case 5:
            return 29;
        case 6:
            return 35;
        case 7:
            return 41;
        case 8:
            return 46;
        case 9:
            return 52;
        case 10:
            return 58;
        case 11:
            return 63;
        case 12:
            return 69;
        case 13:
            return 75;
        case 14:
            return 81;
        case 15:
            return 86;
        case 16:
            return 91;
        case 17:
            return 96;
        case 18:
            return 101;
        case 19:
            return 106;
        case 20:
            return 111;
        case 21:
            return 116;
        case 22:
            return 121;
        case 23:
            return 126;
        case 24:
            return 131;
        case 25:
            return 136;
        case 26:
            return 142;
        case 27:
            return 148;
        case 28:
            return 154;
        case 29:
            return 159;
        case 30:
            return 165;
        case 31:
            return 171;
        case 32:
            return 177;
        case 33:
            return 179;
        case 34:
            return 183;
        case 35:
            return 187;
        case 36:
            return 190;
        case 37:
            return 191;
        case 38:
            return 195;
        case 39:
            return 198;
        case 40:
            return 202;
        case 41:
            return 204;
        case 42:
            return 208;
        case 43:
            return 211;
        case 44:
            return 215;
        case 45:
            return 217;
        case 46:
            return 219;
        case 47:
            return 222;
        case 48:
            return 226;
        case 49:
            return 230;
        case 50:
            return 233;
        case 51:
            return 236;
        case 52:
            return 237;
        case 53:
            return 240;
        case 54:
            return 244;
        case 55:
            return 247;
        case 56:
            return 251;
        case 57:
            return 255;
        case 58:
            return 259;
        case 59:
            return 264;
        case 60:
            return 270;
        case 61:
            return 276;
        case 62:
            return 282;
        case 63:
            return 288;
        case 64:
            return 294;
        case 65:
            return 296;
        case 66:
            return 300;
        case 67:
            return 304;
        case 68:
            return 307;
        case 69:
            return 308;
        case 70:
            return 312;
        case 71:
            return 315;
        case 72:
            return 319;
        case 73:
            return 321;
        case 74:
            return 325;
        case 75:
            return 328;
        case 76:
            return 332;
        case 77:
            return 334;
        case 78:
            return 336;
        case 79:
            return 339;
        case 80:
            return 343;
        case 81:
            return 347;
        case 82:
            return 350;
        case 83:
            return 353;
        case 84:
            return 354;
        case 85:
            return 357;
        case 86:
            return 361;
        case 87:
            return 364;
        case 88:
            return 368;
        case 89:
            return 372;
        case 90:
            return 376;
        case 91:
            return 382;
        case 92:
            return 388;
        case 93:
            return 394;
        case 94:
            return 400;
    }
}
