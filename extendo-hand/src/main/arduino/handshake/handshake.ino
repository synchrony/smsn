/*
 * Extend-o-Hand handshake sketch, copyright 2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/extendo
 */

// if true, listen for incoming OSC messages.  Otherwise, do not wait for input,
// which permits a higher sampling and output rate
#define INPUT_ENABLED         1

#define THREEAXIS             0
#define NINEAXIS              1

#include <ExtendoHand.h>

// ExtendoHand dependencies (they are included in ExtendoHand.cpp, but technically they must be included here)
#include <Droidspeak.h>
#include <ExtendOSC.h>
#include <Morse.h>
#include <RGBLED.h>
#include <Adafruit_NeoPixel.h>
#include <OSCMessage.h>
#include <OSCBundle.h>
#include <MMA7361.h>
#include <Wire.h>
#include <I2Cdev.h>
#include <ADXL345.h>
#include <ITG3200.h>
#include <HMC5883L.h>

ExtendoHand exoHand;


////////////////////////////////////////////////////////////////////////////////
// band-pass filtering

// derived on 2014-11-06
const double rcLow = 0.03359582;
const double rcHigh = 0.02259007;

// these "constants" depend on sampling rate
double lpAlpha;
double hpAlpha;

double axLpLast = 0, ayLpLast = 0, azLpLast = 0;
double axHpLast = 0, ayHpLast = 0, azHpLast = 0;

double lowPass(double x, double lpLast) {
    return lpAlpha * x + (1-lpAlpha) * lpLast;
}

double highPass(double x, double xLast, double hpLast) {
    return hpAlpha * (hpLast + x - xLast);
}


////////////////////////////////////////////////////////////////////////////////
// peak detection

unsigned long loopTimeMs;

double minAmp = 0.2;

// twice the expected period, in milliseconds
const unsigned long peakMaxGap = 353;

double sPrev = 0;
boolean risingPrev = true;
double sRef = 0;
unsigned long tRef = 0;
boolean high = false;
unsigned long tLast = 0;
boolean lastAdded = false;

void emitPeak(unsigned long tRef, unsigned long now) {
    OSCMessage m(EXO_HAND_GESTURE);

    m.add(exoHand.getContext());
    
    m.add("handshake");
    
    // note: recognition is delayed by about one time step from the actual peak,
    // so we subtract loopTimeMs
    m.add((uint64_t) (tRef-loopTimeMs));
    
    // also add the time of recognition
    m.add((uint64_t) now);

    exoHand.getOSC()->sendOSC(m);
}

void filterForHandshakePeaks(unsigned long t, double s) {
    boolean rising = s >= sPrev;
    sPrev = s;

    if (rising) {
        if (!risingPrev) { // local minimum
            if (high) {
                if (sRef - s >= minAmp) {
                    if (tLast > 0 && tRef - tLast <= peakMaxGap) {
                        if (!lastAdded) {
                            emitPeak(tLast, t);
                        }
                        emitPeak(tRef, t);
                        ledCueForHandshake(t);
                        
                        lastAdded = true;
                    } else {
                        lastAdded = false;
                    }

                    tLast = tRef;

                    sRef = s;
                    high = false;
                }
            } else if (s < sRef) {
                sRef = s;
            }
        }
    } else if (risingPrev) { // local maximum
        if (high) {
            if (s > sRef) {
                tRef = t;
                sRef = s;
            }
        } else if (s - sRef >= minAmp) {
            tRef = t;
            sRef = s;
            high = true;
        }
    }

    risingPrev = rising;
}


////////////////////////////////////////////////////////////////////////////////
// LED

const unsigned long ledCueLength = 300;

unsigned long ledCueSince = 0;
int ledCueCount = 0;

void ledCueForHandshake(unsigned long now) {
    ledCueCount++;
    exoHand.setColor(ledCueCount > 2 ? RGB_BLUE : ledCueCount > 1 ? RGB_GREEN : RGB_YELLOW);
    ledCueSince = now;
}

void checkLedStatus(unsigned long now) {
    if (ledCueSince > 0 && now - ledCueSince > ledCueLength) {
        exoHand.setColor(0);
        ledCueSince = 0;
        ledCueCount = 0;
    }
}


////////////////////////////////////////////////////////////////////////////////

void loopTimeUpdated(double loopTime) {
    lpAlpha = loopTime / (rcLow + loopTime);
    hpAlpha = rcHigh / (rcHigh + loopTime);

    loopTimeMs = (unsigned long) (loopTime * 1000);

    //exoHand.getOSC()->sendInfo("lpAlpha=%d/10000, hpAlpha=%d/10000", (int)(lpAlpha*10000), (int)(hpAlpha*10000));
}

void setup() {
    exoHand.setLoopTimeHandler(loopTimeUpdated);
    exoHand.setup();
}


////////////////////////////////////////////////////////////////////////////////

void loop() {
    unsigned long now = exoHand.beginLoop();
    checkLedStatus(now);
    
    double ax, ay, az, gx, gy, gz, mx, my, mz;

    exoHand.getAcceleration(&ax, &ay, &az);
    //exoHand.getRotation(&gx, &gy, &gz);
    //exoHand.getHeading(&mx, &my, &mz);
    
    double axLp = lowPass(ax, axLpLast);
    double ayLp = lowPass(ay, ayLpLast);
    double azLp = lowPass(az, azLpLast);
    
    double axHp = highPass(axLp, axLpLast, axHpLast);
    double ayHp = highPass(ayLp, ayLpLast, ayHpLast);
    double azHp = highPass(azLp, azLpLast, azHpLast);
    
    axLpLast = axLp; ayLpLast = ayLp; azLpLast = azLp;
    axHpLast = axHp; ayHpLast = ayHp; azHpLast = azHp;

    double a = sqrt(ax*ax + ay*ay + az*az);
    double aLp = sqrt(axLp*axLp + ayLp*ayLp + azLp*azLp);
    double aHp = sqrt(axHp*axHp + ayHp*ayHp + azHp*azHp);

    /*
    OSCMessage m("/exo/hand/bandpass");
    m.add(exoHand.getLoopTime());
    m.add(ax); m.add(ay); m.add(az);
    m.add(a);
    m.add(aLp);
    m.add(aHp);
    exoHand.getOSC()->sendOSC(m);
    //*/

    filterForHandshakePeaks(now, aHp);
}
