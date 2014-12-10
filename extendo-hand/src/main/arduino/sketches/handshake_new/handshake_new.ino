/*
 * Extend-o-Hand handshake sketch, copyright 2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/extendo
 */


#define THREEAXIS             0
#define NINEAXIS              1

#include <ExtendoHand.h>
#include <HighPassFilter.h>
#include <LowPassFilter.h>
#include <Vector.h>
#include <ScalarFilter.h>
#include <CrestDetector.h>
#include <BooleanFilter.h>
//#include <SequenceDetector.h>

#include <ExtendoDevice.h>
//#include <Droidspeak.h>
#include <ExtendOSC.h>
//#include <Morse.h>
#include <RGBLED.h>
#include <Adafruit_NeoPixel.h>
#include <OSCBundle.h>
//#include <MMA7361.h>
#include <Wire.h>
#include <I2Cdev.h>
#include <ADXL345.h>
//#include <ITG3200.h>
//#include <HMC5883L.h>

ExtendoHand exoHand;


////////////////////////////////////////////////////////////////////////////////
// band-pass filtering

// derived on 2014-11-06
const double rcLow = 0.03359582;
const double rcHigh = 0.02259007;

double minAmp = 0.2;

// twice the expected period, in milliseconds
const unsigned long crestMaxGap = 353;

LowPassFilter handshakeXLowPass(rcLow), handshakeYLowPass(rcLow), handshakeZLowPass(rcLow);
HighPassFilter handshakeXHighPass(rcHigh), handshakeYHighPass(rcHigh), handshakeZHighPass(rcHigh);
//VectorFilter *handshakeBandPass;
CrestDetector handshakeCrests(minAmp, 0);  // TODO: make use of the minPeriod parameter
//SequenceDetector handshakeSequences(&handshakeCrests, 2, crestMaxGap, 0);


////////////////////////////////////////////////////////////////////////////////
// crest detection

unsigned long loopTimeMs;

unsigned long tLast = 0;
boolean lastAdded = false;

void emitCrest(unsigned long tRef, unsigned long now) {
    OSCMessage m(exoHand.address(OSC_GESTURE));

    m.add(exoHand.getContext());
    
    m.add("handshake");
    
    // note: recognition is delayed by about one time step from the actual crest,
    // so we subtract loopTimeMs
    m.add((uint64_t) (tRef-loopTimeMs));
    
    // also add the time of recognition
    m.add((uint64_t) now);

    exoHand.getOSC()->sendOSC(m);
}

void processHandshakeCrest(unsigned long tRef, unsigned long now) {
    if (tLast > 0 && tRef - tLast <= crestMaxGap) {
        if (!lastAdded) {
            emitCrest(tLast, now);
        }
        emitCrest(tRef, now);

        ledCueForHandshake(now);

        lastAdded = true;
    } else {
        lastAdded = false;
    }

    tLast = tRef;
}


////////////////////////////////////////////////////////////////////////////////
// LED

const unsigned long cueThreshold = 300;

unsigned long lastCue = 0;
int ledCueCount = 0;

void ledCueForHandshake(unsigned long now) {
    if (now - lastCue > cueThreshold) {
        ledCueCount = 0;
    }
    ledCueCount++;
    exoHand.setColorFor(ledCueCount > 2 ? RGB_BLUE : ledCueCount > 1 ? RGB_GREEN : RGB_YELLOW, cueThreshold);
    lastCue = now;
}


////////////////////////////////////////////////////////////////////////////////

void loopTimeUpdated(double loopTime) {
    handshakeXLowPass.updateTimestep(loopTime);
    handshakeYLowPass.updateTimestep(loopTime);
    handshakeZLowPass.updateTimestep(loopTime);
    handshakeXHighPass.updateTimestep(loopTime);
    handshakeYHighPass.updateTimestep(loopTime);
    handshakeZHighPass.updateTimestep(loopTime);
    
    loopTimeMs = (unsigned long) (loopTime * 1000);

    //exoHand.getOSC()->sendInfo("lpAlpha=%d/10000, hpAlpha=%d/10000", (int)(lpAlpha*10000), (int)(hpAlpha*10000));
}

void setup() {
    exoHand.setLoopTimeHandler(loopTimeUpdated);
    exoHand.setup();
}


////////////////////////////////////////////////////////////////////////////////

Vector a;
Vector lp, hp;

void loop() {
    unsigned long now = exoHand.beginLoop();

    double ax, ay, az, gx, gy, gz, mx, my, mz;

    exoHand.getAcceleration(&ax, &ay, &az);
    //exoHand.getRotation(&gx, &gy, &gz);
    //exoHand.getHeading(&mx, &my, &mz);

    a.set(ax, ay, az);
    double amag = a.getMagnitude();

    double xlp = handshakeXLowPass.processNext(now, ax);
    double ylp = handshakeYLowPass.processNext(now, ay);
    double zlp = handshakeZLowPass.processNext(now, az);
    lp.set(xlp, ylp, zlp);
    double lpMag = lp.getMagnitude();

    double xhp = handshakeXHighPass.processNext(now, xlp);
    double yhp = handshakeYHighPass.processNext(now, ylp);
    double zhp = handshakeZHighPass.processNext(now, zlp);
    hp.set(xhp, yhp, zhp);
    double hpMag = hp.getMagnitude();

    /*
    OSCMessage m("/exo/hand/bandpass");
    m.add(exoHand.getLoopTime());
    m.add(ax); m.add(ay); m.add(az);
    m.add(a);
    m.add(aLp);
    m.add(aHp);
    exoHand.getOSC()->sendOSC(m);
    //*/

    unsigned long crest = handshakeCrests.processNext(now, hpMag);
    if (crest) {
        processHandshakeCrest(crest, now);
    }
    //unsigned long s = handshakeSequences.processNext(now, hpMag);
}
