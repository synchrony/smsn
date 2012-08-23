
const unsigned long maxLong = 0xffffffff;
unsigned long startTime;
unsigned long timerHighBits = 0;

// TODO: the other functions which print times must also deal with the high bits

void startCycle()
{
    startTime = millis();

    Serial.print(OM_SYSTEM_TIME);
    Serial.print(" 0x");
    if (timerHighBits > 0)
    {
        Serial.print(timerHighBits, HEX);
    }
    Serial.println(startTime, HEX);    
}

void endCycle()
{
    unsigned long duration;
    unsigned long cur = millis();
    if (cur < startTime)
    {
        timerHighBits++;  
        duration = maxLong - startTime + cur;      
    }       
    else
    {
        duration = cur - startTime;      
    }
    
    if (duration < CYCLE_MILLIS_MIN)
    {
        delay(CYCLE_MILLIS_MIN - duration);
    }
    else if (duration > CYCLE_MILLIS_MAX)
    {
        Serial.print(OM_SYSTEM_ERROR);
        Serial.println(" cycle-too-long");            
    }
}

