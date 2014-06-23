#include "math.h"

// Note: a single-cycle tick is barely audible (using the Sanco EMB-3008A speaker),
// just enough to hear with your ear next to the device.
// Ten cycles produces a more noticeable, though still quiet, click.
// Even ten cycles takes practically no time (compared to a serial write operation).
void tick()
{
    for (int i = 0; i < 10; i++) {
        digitalWrite(SPEAKER_PIN, HIGH);
        digitalWrite(SPEAKER_PIN, LOW);
    }
    
    //tone(SPEAKER_PIN, 440);
    //delayMicroseconds(10);
    //noTone(SPEAKER_PIN);
}

void glideLinear(unsigned long duration, long startFrequency, long endFrequency)
{
    long diff = endFrequency - startFrequency;
    long steps = 100;
    unsigned long inc = (duration * 1000) / steps;
    
    for (long i = 0; i < steps; i++) {
      int t = startFrequency + (diff * i) / steps;
      tone(SPEAKER_PIN, t);
      delayMicroseconds(inc);
    }  
    
    noTone(SPEAKER_PIN);
}

void glideLog(unsigned long duration, long startFrequency, long endFrequency)
{
    long steps = 100;
    unsigned long inc = (duration * 1000) / steps;
    double base = pow((double) endFrequency / (double) startFrequency, 1 / (double) (duration * 1000));
  
    for (long i = 0; i < steps; i++)
    {
        double t = i * inc;
        double f = startFrequency * pow(base, t);
        tone(SPEAKER_PIN, (int) f);
        delayMicroseconds(inc);
    }
    
    noTone(SPEAKER_PIN);
}

void speakRandomSequence()
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
  
    tone(SPEAKER_PIN, (int) t);
    delay(90); 
    noTone(SPEAKER_PIN);
    delay(10);
  }
}

void speakOK()
{
  /*
    tone(SPEAKER_PIN, 330);
    delay(100);
    tone(SPEAKER_PIN, 262);
    delay(300);
    noTone(SPEAKER_PIN);
    delay(50);
    */
    
    glideLog(50, 196, 784);
    delay(20);
    glideLog(300, 1568, 49);
    delay(50);
}

void speakPowerUpPhrase()
{
    glideLog(1000, 55, 14080);
    delay(50);
}

void speakSetupCompletedPhrase()
{
    //delay(100);
    //speakOK();

    speakRandomSequence();
    delay(50);
}

void speakShockPhrase()
{
    glideLog(100, 14080, 55);
}

void speakWarningPhrase()
{
    //tone(SPEAKER_PIN, 220);
    glideLog(20, 220, 880);
    //delay(100);
    glideLog(20, 880, 220);
    //delay(50);
    //glideLog(100, 220, 880);
}

