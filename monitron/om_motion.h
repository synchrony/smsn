int motionDetected = 0;
unsigned long startTime;
unsigned long endTime;

void resetMotionDetector() {
    motionDetected = 0;
    startTime = 0;
    endTime = 0;
}

void sampleMotionDetector() {
    beginSample();  
    if (0 == startTime) {
        startTime = millis();
    }
    
    int v = digitalRead(MOTION_PIN);
    if (v == LOW) {
        motionDetected++;
    }
    
    endTime = millis();   
    endSample();
}

void reportMotionDetectorResults() {
    beginOSCWrite();
 
    Serial.print(OM_SENSOR_SE10_MOTION);
    Serial.print(" "); 
    Serial.print(startTime, HEX);
    Serial.print(" ");
    Serial.print(endTime, HEX);
    Serial.print(" 1 ");
    Serial.println(motionDetected);
        
    endOSCWrite();
}
