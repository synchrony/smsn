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

void speakStartupPhrase()
{
    speakGlide(100, 1760, 440);
    delay(100);
    speakGlide(400, 440, 880);
}

