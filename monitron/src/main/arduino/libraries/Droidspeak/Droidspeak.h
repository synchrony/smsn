/*
  Droidspeak.h - a simple vocabulary of beeps and whistles for symbolic auditory feedback without speech
  Created by Joshua Shinavier, 2012-2014
  Released into the public domain.
*/

#ifndef Droidspeak_h
#define Droidspeak_h

#include "Arduino.h"

class Droidspeak
{
  public:
    Droidspeak(uint8_t speakerPin);

    void glideLinear(unsigned long duration, long startFrequency, long endFrequency);
    void glideLog(unsigned long duration, long startFrequency, long endFrequency);
    void speakRandomSequence();
    void speakOK();
    void speakPowerUpPhrase();
    void speakSerialOpenPhrase();
    void speakShockPhrase();
    void speakWarningPhrase();
    void tick();

  private:
    uint8_t _speakerPin;
};

#endif // Droidspeak_h

