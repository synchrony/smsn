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

    OSCMessage m(EXO_OM_SENSOR_SE10_MOTION);
    m.add((uint64_t) startTime);
    m.add((uint64_t) endTime);
    m.add(1);
    m.add(motionDetected);
    osc.sendOSC(m);
        
    endOSCWrite();
}
