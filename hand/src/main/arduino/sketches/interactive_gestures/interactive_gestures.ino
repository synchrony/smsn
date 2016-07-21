/*
 * Extend-o-Hand handshake and handoff combination sketch, copyright 2014-2015 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/smsn
 */

//#define THREE_AXIS
#define NINE_AXIS

#include <Extendo.h>
#include <ExtendoHand.h>
#include <SpikeDetector.h>
#include <Vector3D.h>
#include <ScalarFilter.h>
#include <CircularBufferFilter.h>
#include <HighPassFilter.h>
#include <LowPassFilter.h>
#include <BooleanFilter.h>
#include <DirectionFilter.h>
#include <CrestDetector.h>
#include <VectorFilter.h>

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

//#define AGENT_URI "http://fortytwo.net/josh/things/SBZFumn" // JS
#define AGENT_URI "http://fortytwo.net/josh/things/JdGwZ4n" // XL
//#define AGENT_URI "http://fortytwo.net/josh/things/D4bbQSr" // OH

#define HANDOFF   "/exo/activity/handoff"
#define HANDSHAKE "/exo/activity/handshake"

ExtendoHand exoHand;

unsigned long loopTimeMs;

void emitGesture(const char *address, unsigned long tRef, unsigned long now) {
    OSCMessage m(address);
    m.add(exoHand.getContext());

    // note: recognition is delayed by about one time step from the actual crest,
    // so we subtract loopTimeMs
    m.add((uint64_t) (tRef-loopTimeMs));

    // also add the time of recognition
    m.add((uint64_t) now);

    exoHand.getOSC()->sendOSC(m);
}


////////////////////////////////////////////////////////////////////////////////
// handshake gesture

// derived on 2014-11-06
const double rcLow = 0.03359582;
const double rcHigh = 0.02259007;

LowPassFilter handshakeXLowPass(rcLow), handshakeYLowPass(rcLow), handshakeZLowPass(rcLow);
VectorFilter handshakeLowPass(handshakeXLowPass, handshakeYLowPass, handshakeZLowPass);
HighPassFilter handshakeXHighPass(rcHigh), handshakeYHighPass(rcHigh), handshakeZHighPass(rcHigh);
VectorFilter handshakeHighPass(handshakeXHighPass, handshakeYHighPass, handshakeZHighPass);

double minAmp = 0.2;

// twice the expected period, in milliseconds
const unsigned long crestMaxGap = 353;

CrestDetector handshakeCrests(minAmp, 0);  // TODO: make use of the minPeriod parameter
//SequenceDetector handshakeSequences(handshakeCrests, 2, crestMaxGap, 0);

unsigned long tLast = 0;
boolean lastAdded = false;

const unsigned long cueThreshold = 300;

unsigned long lastCue = 0;
int ledCueCount = 0;

int pulseCount(unsigned long now) {
    if (now - lastCue > cueThreshold) {
        ledCueCount = 0;
    }
    ledCueCount++;
    //exoHand.setColorFor(ledCueCount > 2 ? RGB_BLUE : ledCueCount > 1 ? RGB_GREEN : RGB_YELLOW, cueThreshold);
    lastCue = now;
    return ledCueCount;
}

void processHandshakeCrest(unsigned long tRef, unsigned long now) {
    if (tLast > 0 && tRef - tLast <= crestMaxGap) {
        /* note: previous crests are currently not reported
        if (!lastAdded) {
            //exoHand.playTone(1760, 75);
            emitGesture(HANDSHAKE, tLast, now);
        }
        //exoHand.playTone(1760, 75);
        emitGesture(HANDSHAKE, tRef, now);
        */

        if (pulseCount(now) > 2) {
            emitGesture(HANDSHAKE, tRef, now);
        }

        lastAdded = true;
    } else {
        lastAdded = false;
    }

    tLast = tRef;
}


////////////////////////////////////////////////////////////////////////////////
// handoff gestures (give/take)

// band-pass filtering constants for "buildup", derived on 2014-12-09
const double handoffRcLow = 0.6114599;
const double handoffRcHigh = 0.4090329;

// require 800ms between spikes, which is around one quarter period of a "give"
// This ensures that spikes are relatively isolated before the gesture, cutting down on false positives
// during noisy intervals.  Giver and taker must suddenly spike at the same time, apart from spiking in the
// appropriate directions.
unsigned long interSpikeDelay = 800;

double spikeThreshold = 2.0;

SpikeDetector spikeDetector(spikeThreshold, interSpikeDelay);

/*
LowPassFilter handoffXLowPass(handoffRcLow), handoffYLowPass(handoffRcLow), handoffZLowPass(handoffRcLow);
VectorFilter handoffLowPass(handoffXLowPass, handoffYLowPass, handoffZLowPass);
HighPassFilter handoffXHighPass(handoffRcHigh), handoffYHighPass(handoffRcHigh), handoffZHighPass(handoffRcHigh);
VectorFilter handoffHighPass(handoffXHighPass, handoffYHighPass, handoffZHighPass);
*/

// note: not using CircularBufferFilter's period and timestep as intended; just creating a simple, small buffer
const int smoothingBufferSize = 10;
CircularBufferFilter smoothingX(smoothingBufferSize, 1);
CircularBufferFilter smoothingY(smoothingBufferSize, 1);
CircularBufferFilter smoothingZ(smoothingBufferSize, 1);
VectorFilter smoothing(smoothingX, smoothingY, smoothingZ);

// values derived on 2014-12-09 from the 2014-11-25 handoff data using a circular buffer
Vector3D giveSpikeDirection(0.686863, -0.6251692, 0.3706516);
Vector3D takeSpikeDirection(0.8353023, -0.49748, -0.2340593);
DirectionFilter giveSpikeFilter(giveSpikeDirection, 0.3498809 * 2);
DirectionFilter takeSpikeFilter(takeSpikeDirection, 0.3846736 * 2);


////////////////////////////////////////////////////////////////////////////////

void loopTimeUpdated(double loopTime) {
/*
    handoffLowPass.updateTimestep(loopTime);
    handoffHighPass.updateTimestep(loopTime);
*/
    //smoothing.updateTimestep(loopTime);

    handshakeLowPass.updateTimestep(loopTime);
    handshakeHighPass.updateTimestep(loopTime);

    loopTimeMs = (unsigned long) (loopTime * 1000);

    //exoHand.getOSC()->sendInfo("timestep: %d/1000000", (int) (loopTime * 1000000));
}


////////////////////////////////////////////////////////////////////////////////

void setup() {
    exoHand.setLoopTimeHandler(loopTimeUpdated);
    exoHand.setup();
    exoHand.setContext(AGENT_URI);
    exoHand.playTone(440,100);
}


////////////////////////////////////////////////////////////////////////////////

Vector3D a; // g, m;
//Vector gtlp, gthp;

void loop() {
    unsigned long now = exoHand.beginLoop();

    // raw motion data
    exoHand.getAcceleration(a);
    //exoHand.getRotation(g);
    //exoHand.getHeading(m);

    // smoothing
    Vector3D smooth = smoothing.processNext(now, a);

    // handshake recognition
    Vector3D shakeLp = handshakeLowPass.processNext(now, a);
    Vector3D shakeHp = handshakeHighPass.processNext(now, shakeLp);
    unsigned long crest = handshakeCrests.processNext(now, shakeHp.getMagnitude());
    if (crest) {
        processHandshakeCrest(crest, now);
    }

    // never mind the band-pass filter for handoff; even a small circular buffer works better
    // for smoothing.  The difference in quality is intuitively and immediately obvious
    /*
    Vector3D *gthp = handoffHighPass.processNext(now, &a);
    Vector3D *gtlp = handoffLowPass.processNext(now, gthp);
    */

    // handoff recognition
    unsigned long isSpike = spikeDetector.processNext(now, a.getMagnitude());
    if (isSpike > 0) {
        //exoHand.setColorFor(RGB_WHITE, cueThreshold);

        // "give" and "take" centers are so close that we treat them as one gesture, at least until such time
        // as we take "give" crests (distinct from spikes) into account
        if (giveSpikeFilter.process(smooth) || takeSpikeFilter.process(smooth)) {
//        if (giveSpikeFilter.process(gtlp) || takeSpikeFilter.process(gtlp)) {
            //exoHand.setColorFor(RGB_RED, cueThreshold);
            emitGesture(HANDOFF, isSpike, now);
        }
    }

    /*
    OSCMessage m("/exo/hand/bandpass");
    m.add(exoHand.getLoopTime());
    //m.add(a.getX()); m.add(a.getY()); m.add(a.getZ());
    m.add(a.getMagnitude());
    //m.add(gtlp.getMagnitude());
    m.add(gthp.getMagnitude());
    m.add(isSpike > 0 ? 1 : 0);
    exoHand.getOSC()->sendOSC(m);
    //*/
}
