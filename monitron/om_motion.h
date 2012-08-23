int motionDetected = 0;
unsigned long firstSampleTime;
unsigned long lastSampleTime;

void resetMotionDetector() {
    motionDetected = 0;
    firstSampleTime = 0;
    lastSampleTime = 0;
}

void sampleMotionDetector() {
    beginSample();
  
    lastSampleTime = millis();
    if (0 == firstSampleTime) {
        firstSampleTime = lastSampleTime;
    }
    
    int v = digitalRead(MOTION_PIN);
    if (v == LOW) {
        motionDetected++;
    }
    
    endSample();
}

void reportMotionDetectorResults() {
    beginOSCWrite();
 
    Serial.print(OM_SENSOR_SE10_MOTION);
    Serial.print(" "); 
    Serial.print(firstSampleTime, HEX);
    Serial.print(" ");
    Serial.print(lastSampleTime, HEX);
    Serial.print(" 1 ");
    Serial.println(motionDetected);
        
    endOSCWrite();
}
