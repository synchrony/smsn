
const unsigned long maxLong = 0xffffffff;
unsigned long timerStartTime;
unsigned long timerHighBits = 0;

// TODO: the other functions which print times must also deal with the high bits

void startCycle()
{
    timerStartTime = millis();

    Serial.print(OM_SYSTEM_TIME);
    Serial.print(" ");
    if (timerHighBits > 0)
    {
        Serial.print(timerHighBits, HEX);
    }
    Serial.println(timerStartTime, HEX);    
}

unsigned long errorReportIndex = 0;
unsigned long cyclesBetweenErrorReports = 1;

void error(char *message)
{
    beginOSCErrorMessage();
    Serial.print(OM_SYSTEM_ERROR);
    Serial.print(" ");
    Serial.println(message);
    
    errorReportIndex++;
    if (errorReportIndex == cyclesBetweenErrorReports)
    {
        droidspeak.speakWarningPhrase();
        cyclesBetweenErrorReports *= 2;
        errorReportIndex = 0;
    }
    endOSCErrorMessage();     
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
        error("cycle-too-long");            
    }
}


