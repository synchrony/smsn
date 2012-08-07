void speakGlide(unsigned long duration, long startFrequency, long endFrequency)
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
    delay(100);  
  }
  noTone(SPEAKER_PIN);
}

void speakStartupPhrase()
{
    speakGlide(200, 1760, 440);
    delay(100);
    speakGlide(400, 440, 880);
    
    delay(100);
    speakRandomSequence();
}

