#include <math.h>
#include <Arduino.h>  // for millis(), if nothing else

// don't output the "none" gesture in keyboard mode
#ifdef GESTURE_MODE
#define OUTPUT_NULL_GESTURE
#endif

unsigned int gestureTone = 0;
unsigned int gestureToneLength = 0;
double gestureToneVolume = 1.0;

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

/* of a large (n=344), heterogeneous set of taps gathered on 2014-08-21 and normalized
   to unit vectors, 95% were within 0.7 of their normalized mean, (0.04, -0.22, 0.98) */
const double tapCenter[] = {0.04, -0.22, 0.98};
const double tapTolerance = 0.6;

const double flipCenter[] = {0.9266487, 0.1260582, -0.303386};
const double flipTolerance = 0.5;

const double twitchCenter[] = {0.5662381, 0.1734404, 0.7668633};
const double twitchTolerance = 0.30;

const unsigned long flipMinDelay = 800000;
const unsigned long twitchMinDelay = 800000;

// at most 300ms may pass between one extremum of a wave gesture and the other
const unsigned long waveMaxDelay = 300000;

// 500ms was found to be typical in the large-format keypad application
// however, even 300ms clearly excludes some quick taps, and is frustrating as a threshold
const unsigned long pairedTapMinDelay = 150000;
const unsigned long pairedTapMaxDelay = 800000;

const unsigned long tripleTapMaxDelay = 200000;

const unsigned long tapMaxWidth = 100000;

const char
    *flip = "flip",
    *openPalm = "open-palm",
    *tap1 = "tap-1",
    *tap2 = "tap-2",
    *tripleTap = "triple-tap",
    *wave1 = "wave-1",
    *wave2 = "wave-2",
    *wave1Complete = "wave-1-complete",
    *wave2Complete = "wave-2-complete",
    *twitch = "twitch",
    *none = "none";
    
unsigned long lastWave1 = 0, lastWave2 = 0;
unsigned long lastPairedTap1 = 0, lastPairedTap2 = 0, lastTripleTap1 = 0, lastTripleTap2 = 0;
unsigned long lastFlip = 0;
unsigned long lastTwitch = 0;

double varyVolumeWithDistance(double dist, double threshold) {
    if (dist > threshold) dist = threshold;

    const double minVol = 0.25, maxVol = 1.0;
    return minVol + (threshold - dist) * ((maxVol - minVol)/threshold);
}

unsigned int varyPitchWithDistance(double basePitch, double dist, double threshold) {
    if (dist > threshold) dist = threshold;
   
    // 5 octaves (6th is a limit which never occurs) makes the highest registers sound
    // "brightest" without being too high and thin, while the lower registers
    // are less audible on a piezo speaker
    unsigned int p = (unsigned int) ((threshold - dist) * (6/threshold));
    // only allow whole octaves
    unsigned int f = (unsigned int) (basePitch * (int) pow(2, p));
    return f;
}

unsigned int varyDurationWithDistance(unsigned int minDuration, unsigned int maxDuration, double dist, double threshold) {
    if (dist > threshold) dist = threshold;
 
    return (unsigned int) (minDuration + (maxDuration - minDuration) * (threshold - dist) / (1.0 * threshold));
}

// note: overflow of millis() during a wave gesture may interfere with its recognition.  However, this is infrequent and unlikely.
const char *classifyGestureVector(double *v, unsigned long tmax, unsigned long now) {
    double normed[3];
    normalize(v, normed);

    // delay between peak and fall-off point (where acceleration sinks below a threshold)
    unsigned long width = now - tmax;
    
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
    d = distance(normed, flipCenter);
    if (d <= flipTolerance && now - lastFlip >= flipMinDelay) {
        gestureTone = 1109;
        //gestureToneLength = 100;
        gestureToneLength = varyDurationWithDistance(25, 150, d, flipTolerance);
        lastFlip = now;
        return flip;      
    }
    
    /* exclude "twitch" for now
    d = distance(normed, twitchCenter);
    if (d <= twitchTolerance && now - lastTwitch >= twitchMinDelay && now - lastFlip >= twitchMinDelay) {
        gestureTone = 635;
        gestureToneLength = 100;
        lastTwitch = now;
        return twitch;  
    }
    */
    
    d = distance(normed, tapCenter);
    if (d <= tapTolerance && width < tapMaxWidth) {
        //gestureToneVolume = varyVolumeWithDistance(d, tapTolerance);
        if (now - lastTripleTap2 <= tripleTapMaxDelay) {
            gestureTone = 3520;
            gestureToneLength = 200;
            lastTripleTap1 = 0;
            lastTripleTap2 = 0;
            return tripleTap;
        } else if (now - lastTripleTap1 <= tripleTapMaxDelay) {
            lastTripleTap2 = now;
            lastPairedTap1 = 0;
            lastPairedTap2 = 0;
#ifdef OUTPUT_NULL_GESTURE
            return none;    
#else
            return NULL;
#endif
        } else {
            // this tap can double as a paired tap
            lastTripleTap1 = now;
        }

        if (now - lastPairedTap1 >= pairedTapMinDelay && now - lastPairedTap2 >= pairedTapMinDelay) {
            if (now - lastPairedTap1 <= pairedTapMaxDelay) {
                lastPairedTap1 = 0;
                lastPairedTap2 = now;
                //gestureTone = 1760;
                gestureTone = varyPitchWithDistance(82.5, d, tapTolerance);
                //gestureToneLength = 50;
                gestureToneLength = varyDurationWithDistance(25, 150, d, tapTolerance);
                return tap2;
            } else {
                lastPairedTap1 = now;
                //gestureTone = 440;
                gestureTone = varyPitchWithDistance(55, d, tapTolerance);
                //gestureToneLength = 50;
                gestureToneLength = varyDurationWithDistance(25, 150, d, tapTolerance);
                return tap1;
            }
        }
    }
#endif // ifdef KEYBOARD_MODE
#endif // ifdef GESTURE_MODE

#ifdef OUTPUT_NULL_GESTURE
    gestureToneLength = 0;
    return none;    
#else
    return NULL;
#endif
}
