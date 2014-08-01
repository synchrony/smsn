/*
  Morse.cpp
  Created by Joshua Shinavier, 2013-2014
  Released into the public domain.
*/

#include "Morse.h"

Morse::Morse(int morsePin, int (*stopTest)(), void (*sendError)(const char*))
{
    _morsePin = morsePin;
    _stopTest = stopTest;
    _sendError = sendError;
}

// Codes for punctuation taken from: http://home.windstream.net/johnshan/cw_ss_list_punc.html
// Not used: the paragraph symbol .-.-..
const char* morseCodes[96] = {
/* ! */ "---.",
/* " */ ".-..-.",
/* # */ 0,
/* $ */ "...-..-",
/* % */ 0,
/* & */ 0,
/* ' */ ".----.",
/* ( */ "-.--.",
/* ) */ "-.--.-",
/* * */ 0,
/* + */ ".-.-.",
/* , */ "--..--",
/* - */ "-....-",
/* . */ ".-.-.-",
/* / */ "-..-.",
/* 0 */ "-----",
/* 1 */ ".----",
/* 2 */ "..---",
/* 3 */ "...--",
/* 4 */ "....-",
/* 5 */ ".....",
/* 6 */ "-....",
/* 7 */ "--...",
/* 8 */ "---..",
/* 9 */ "----.",
/* : */ "---...",
/* ; */ "-.-.-.",
/* < */ 0,
/* = */ "-...-",
/* > */ 0,
/* ? */ "..--..",
/* @ */ ".--.-.",
/* A */ ".-",
/* B */ "-...",
/* C */ "-.-.",
/* D */ "-..",
/* E */ ".",
/* F */ "..-.",
/* G */ "--.",
/* H */ "....",
/* I */ "..",
/* J */ ".---",
/* K */ "-.-",
/* L */ ".-..",
/* M */ "--",
/* N */ "-.",
/* O */ "---",
/* P */ ".--.",
/* Q */ "--.-",
/* R */ ".-.",
/* S */ "...",
/* T */ "-",
/* U */ "..-",
/* V */ "...-",
/* W */ ".--",
/* X */ "-..-",
/* Y */ "-.--",
/* Z */ "--..",
/* [ */ "-.--.",
/* \ */ 0,
/* ] */ "-.--.-",
/* ^ */ 0,
/* _ */ "..--.-",
/* ` */ 0,
/* a */ ".-",
/* b */ "-...",
/* c */ "-.-.",
/* d */ "-..",
/* e */ ".",
/* f */ "..-.",
/* g */ "--.",
/* h */ "....",
/* i */ "..",
/* j */ ".---",
/* k */ "-.-",
/* l */ ".-..",
/* m */ "--",
/* n */ "-.",
/* o */ "---",
/* p */ ".--.",
/* q */ "--.-",
/* r */ ".-.",
/* s */ "...",
/* t */ "-",
/* u */ "..-",
/* v */ "...-",
/* w */ ".--",
/* x */ "-..-",
/* y */ "-.--",
/* z */ "--..",
/* { */ 0,
/* | */ 0,
/* } */ 0,
/* ~ */ 0,
/* <del> */ 0,
};

int morsePitch = 1760;
long timeUnit = 50;

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

        const char *sequence = morseCodes[c - 33];
        if (!sequence) {
            sequence = morseCodes['?' - 33];
        }

        if (firstLetterInWord) {
            firstLetterInWord = false;
        } else {
            shortGap();
        }

        int firstElementInLetter = true;
        const char *cur2 = sequence;
        while (*cur2) {
            if (firstElementInLetter) {
                firstElementInLetter = false;
            } else {
                interElementGap();
            }
            char c2 = *cur2;
            if ('.' == c2) {
                dit();
            } else if ('-' == c2) {
                dah();
            } else {
                _sendError("element in morse sequence is neither a dit '.' nor a dah '-'");
            }

            cur2++;
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

