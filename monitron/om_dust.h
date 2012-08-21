 // This dust sensor code was adapted from Paul Deng's tutorial:
//    http://sensorapp.net/?p=479

const int dustSensorDelay1=280;
const int dustSensorDelay2=40;
const int dustSensorDelay3=9680;

int firstReading = true;

void sampleDustSensor() {
    digitalWrite(DUST_LED_PIN, LOW); // power on the LED
    delayMicroseconds(dustSensorDelay1);
    
    int dustVal=analogRead(DUST_PIN); // read the dust value via pin 5 on the sensor
    delayMicroseconds(dustSensorDelay2);  // what is the purpose of the second delay?
    
    digitalWrite(DUST_LED_PIN, HIGH); // turn the LED off
    delayMicroseconds(dustSensorDelay3);  // just to make it an even 10ms, apparently
        
    // Don't output the first reading.  For some reason, in this application, the
    // first reading is not reliable.
    if (firstReading) {
        firstReading = false;
    } else {
        tick();
        Serial.print(DUST_OSC_PREFIX);
        Serial.print("/data "); 
        Serial.println(dustVal);
    }
    
    // Note: it is assumed that the rest of the sampling cycle will take at
    // least one second, before which the dust sensor will not be sampled again.
}
