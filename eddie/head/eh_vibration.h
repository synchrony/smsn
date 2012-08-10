int vibro_min, vibro_max;

void vibro_sample()
{
    unsigned int v = analogRead(VIBRO_PIN);
    
    if (v < vibro_min)
    {
        vibro_min = v;
    }
    
    else if (v > vibro_max)
    {
        vibro_max = v;
    }  
}

void vibro_reset()
{
    vibro_min = 1023;
    vibro_max = 0;
}

void vibro_reportAndReset()
{
    double relMin = vibro_min / 1024.0; 
    double relMax = vibro_max / 1024.0;
    
    tick();
    Serial.print(VIBRO_OSC_PREFIX);
    Serial.print("/data "); 
    Serial.print(relMin);
    Serial.print(" "); 
    Serial.println(relMax); 
    
    vibro_reset();
}
