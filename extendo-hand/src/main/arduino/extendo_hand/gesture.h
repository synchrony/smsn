#include <math.h>
#include <Arduino.h>  // for millis(), if nothing else

double mag(const double *vin) {
    return sqrt(pow(vin[0], 2) + pow(vin[1], 2) + pow(vin[2], 2));  
}

void normalize(const double *vin, double *vout) {
    double m = mag(vin);
    vout[0] = vin[0] / m;
    vout[1] = vin[1] / m;
    vout[2] = vin[2] / m;
}

double distance(const double *v1, const double *v2) {
    return sqrt(pow(v1[0] - v2[0], 2) + pow(v1[1] - v2[1], 2) + pow(v1[2] - v2[2], 2));  
}

const double openPalmCenter[] = {0.3093581, -0.6281864, -0.7139183};
const double openPalmTolerance = 0.3;

const double waveCenter1[] = {-0.8356163, 0.4055072, 0.3705529};
const double waveTolerance1 = 0.5;

const double waveCenter2[] = {0.9125032, -0.1936484, -0.3603307};
const double waveTolerance2 = 0.5;

// at most 300ms may pass between one extremum of a wave gesture and the other
const unsigned long waveMaxDelay = 300;

const char
    *openPalm = "open-palm",
    *wave1 = "wave-1",
    *wave2 = "wave-2",
    *wave1Complete = "wave-1-complete",
    *wave2Complete = "wave-2-complete",
    *none = "none";
    
unsigned long lastWave1 = 0, lastWave2 = 0;

const char *classifyGestureVector(double *v) {
    double normed[3];
    normalize(v, normed);
    
    // note: overflow of millis() during a wave gesture may interfere with its recognition.  However, this is infrequent and unlikely.
    unsigned long now = millis();
    
    double d;
    d = distance(normed, openPalmCenter);
    if (d <= openPalmTolerance) {
        return openPalm;
    }
    d = distance(normed, waveCenter1);
    if (d <= waveTolerance1) {
        lastWave1 = now;
        
        return (now - lastWave2 <= waveMaxDelay) ? wave1Complete : wave1;
    }
    d = distance(normed, waveCenter2);
    if (d <= waveTolerance2) {
        lastWave2 = now;
        
        return (now - lastWave1 <= waveMaxDelay) ? wave2Complete : wave2;
    }    
    
    return none;    
}
