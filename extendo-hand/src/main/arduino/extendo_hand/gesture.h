#include <math.h>
#include <Arduino.h>  // for millis(), if nothing else

int gestureTone = 0;
int gestureToneLength = 0;

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

const double openPalmCenter[] = {0.31, -0.63, -0.71};
const double openPalmTolerance = 0.3;

const double waveCenter1[] = {-0.84, 0.41, 0.37};
const double waveTolerance1 = 0.5;

const double waveCenter2[] = {0.91, -0.19, -0.36};
const double waveTolerance2 = 0.5;

const double tapCenter[] = {0.16, -0.04, 0.98};
const double tapTolerance = 0.5;  // generous; 0.15 *should* be sufficient

// at most 300ms may pass between one extremum of a wave gesture and the other
const unsigned long waveMaxDelay = 300000;

// 500ms was found to be typical in the large-format keypad application
const unsigned long tapMinDelay = 200000;
const unsigned long tapMaxDelay = 800000;

const unsigned long tapMaxWidth = 100000;

const char
    *openPalm = "open-palm",
    *wave1 = "wave-1",
    *wave2 = "wave-2",
    *wave1Complete = "wave-1-complete",
    *wave2Complete = "wave-2-complete",
    *tap1 = "tap-1",
    *tap2 = "tap-2",
    *none = "none";
    
int32_t lastWave1 = 0, lastWave2 = 0;
int32_t lastTap1 = 0, lastTap2 = 0;

// note: overflow of millis() during a wave gesture may interfere with its recognition.  However, this is infrequent and unlikely.
const char *classifyGestureVector(double *v, int32_t tmax, int32_t now) {
    double normed[3];
    normalize(v, normed);
        
    int32_t width = now - tmax;
    
    double d;
    
#ifdef GESTURE_MODE
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
#else
#ifdef KEYBOARD_MODE
    d = distance(normed, tapCenter);
    if (d <= tapTolerance && width < tapMaxWidth) {        
        if (now - lastTap1 >= tapMinDelay && now - lastTap2 >= tapMinDelay) {
            if (now - lastTap1 <= tapMaxDelay) {
                lastTap1 = 0;
                lastTap2 = now;
                gestureTone = 1600;
                gestureToneLength = 50;
                return tap2;
            } else {
                lastTap1 = now;
                gestureTone = 400;
                gestureToneLength = 50;
                return tap1;
            }
        }
    }
#endif
#endif

    return none;    
}
