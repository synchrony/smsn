/*
  Droidspeak.cpp
  Created by Joshua Shinavier, 2012-2016
  Released into the public domain.
*/

#include <math.h>

#include "Droidspeak.h"


Droidspeak::Droidspeak(uint8_t speakerPin)
{
    _speakerPin = speakerPin;
}

void Droidspeak::glideLinear(unsigned long duration,
                             unsigned int startFrequency,
                             unsigned int endFrequency)
{
    unsigned int diff = endFrequency - startFrequency;
    long steps = 100;
    unsigned long inc = (duration * 1000) / steps;
    
    for (long i = 0; i < steps; i++) {
      unsigned int t = startFrequency + (diff * i) / steps;
      tone(_speakerPin, t);
      delayMicroseconds(inc);
    }  
    
    noTone(_speakerPin);
}

void Droidspeak::glideLog(unsigned long duration,
                          unsigned int startFrequency,
                          unsigned int endFrequency)
{
    long steps = 100;
    unsigned long inc = (duration * 1000) / steps;
    double base = pow((double) endFrequency / (double) startFrequency, 1 / (double) (duration * 1000));
  
    for (long i = 0; i < steps; i++)
    {
        double t = i * inc;
        double f = startFrequency * pow(base, t);
        tone(_speakerPin, (int) f);
        delayMicroseconds(inc);
    }
    
    noTone(_speakerPin);
}

void Droidspeak::speakRandomSequence()
{
  int base = 440;
  
  // Using a pentatonic scale makes these random sequences more melodious
  double fact = 1.122462;
  
  for (int i = 0; i < 5; i++)
  {
    int x = random(0, 12);
    double t = base;
    for (int i = 0; i < x; i++)
    {
      t *= fact;
    }
  
    tone(_speakerPin, (int) t);
    delay(90); 
    noTone(_speakerPin);
    delay(10);
  }
}

void Droidspeak::speakOK()
{
/*
    glideLog(105, 220, 313);
    delay(57);
    glideLog(132, 466, 313);
*/
    glideLog(53, 440, 626);
    delay(29);
    glideLog(66, 932, 626);

    delay(50);
}

void Droidspeak::speakPowerUpPhrase()
{
    glideLog(800, 55, 14080);
    delay(50);
}

void Droidspeak::speakSerialOpenPhrase()
{
    speakRandomSequence();
    delay(50);
}

void Droidspeak::speakShockPhrase()
{
    glideLog(100, 14080, 55);
}

void Droidspeak::speakWarningPhrase()
{
    //tone(_speakerPin, 220);
    glideLog(20, 220, 880);
    //delay(100);
    glideLog(20, 880, 220);
    //delay(50);
    //glideLog(100, 220, 880);
}

// Note: a single-cycle tick is barely audible (using the Sanco EMB-3008A speaker),
// just enough to hear with your ear next to the device.
// Ten cycles produces a more noticeable, though still quiet, click.
// Even ten cycles takes practically no time (compared to a serial write operation).
void Droidspeak::tick()
{
    for (int i = 0; i < 10; i++) {
        digitalWrite(_speakerPin, HIGH);
        digitalWrite(_speakerPin, LOW);
    }
    
    //tone(_speakerPin, 440);
    //delayMicroseconds(10);
    //noTone(_speakerPin);
}

void Droidspeak::analogTone(unsigned long duration,
                            unsigned int frequency,
                            double volume) {
    int level = (int) (volume * 255);
    unsigned long c = (duration * frequency)/1000;

    // note: small inaccuracies of timing will creep in thus without correction,
    // e.g. up to 2ms / s for a=440, 4ms / s for a=880
    unsigned int delay = 1000000 / (2 * frequency);
    //unsigned int delay = 1136;
    for (unsigned long i = 0; i < c; i++) {
        analogWrite(_speakerPin, level);
        delayMicroseconds(delay);
        analogWrite(_speakerPin, 0);
        delayMicroseconds(delay);
    }
}
