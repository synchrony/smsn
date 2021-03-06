/*
  Morse.h - utility to play Morse code sequences via Arduino outputs
  Created by Joshua Shinavier, 2013-2016
  Released into the public domain.
*/

#ifndef Morse_h
#define Morse_h

#include "Arduino.h"

class Morse
{
  public:
    Morse(int morsePin, int (*stopTest)());

    void morseOn();
    void morseOff();

    void dit();
    void dah();

    void interElementGap();

    // gap between letters
    void shortGap();

    // gap between words
    void mediumGap();

    void beginSequence();
    void endSequence();

    void playMorseString(const char* message);

    void playMorseInt(int d);

  private:
    int _morsePin;
    int (*_stopTest)();

    int getOffset(int c);

    static char bits[50];
    //static int offsets[67];
};

#endif // Morse_h
