/*
 * Extend-o-Hand high-five sketch, copyright 2015 by Joshua Shinavier
 * Note: this is a "weak" high-five recognizer for a fun demo;
 * in its current form, it is not precise enough to be combined with other gestures.
 * 
 * See: https://github.com/joshsh/extendo
 */

#define THREEAXIS             0
#define NINEAXIS              1

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

#define HIGHFIVE   "/exo/activity/highfive"

ExtendoHand exoHand;

unsigned long loopTimeMs;

void emitGesture(const char *address, unsigned long tRef, unsigned long now) {
    OSCMessage m(address);
    m.add(exoHand.getContext());

    /* TODO: temporarily disabled to save on flash memory
    // note: recognition is delayed by about one time step from the actual crest,
    // so we subtract loopTimeMs
    m.add((uint64_t) (tRef-loopTimeMs));

    // also add the time of recognition
    m.add((uint64_t) now);
    //*/

    exoHand.getOSC()->sendOSC(m);
}


////////////////////////////////////////////////////////////////////////////////
// high-five gesture

// require a reasonable 1s between high-fives.  Lower intervals are evidence of too much coffee.
unsigned long interSpikeDelay = 1000;

// high-five like you mean it
double spikeThreshold = 3.0;

SpikeDetector spikeDetector(spikeThreshold, interSpikeDelay);

// note: not using CircularBufferFilter's period and timestep as intended; just creating a simple, small buffer
const int smoothingBufferSize = 10;
CircularBufferFilter smoothingX(smoothingBufferSize, 1);
CircularBufferFilter smoothingY(smoothingBufferSize, 1);
CircularBufferFilter smoothingZ(smoothingBufferSize, 1);
VectorFilter smoothing(smoothingX, smoothingY, smoothingZ);


////////////////////////////////////////////////////////////////////////////////

void loopTimeUpdated(double loopTime) {
    //smoothing.updateTimestep(loopTime);

    loopTimeMs = (unsigned long) (loopTime * 1000);

    //exoHand.getOSC()->sendInfo("timestep: %d/1000000", (int) (loopTime * 1000000));
}


////////////////////////////////////////////////////////////////////////////////

void setup() {
    exoHand.setLoopTimeHandler(loopTimeUpdated);
    exoHand.setup();
    exoHand.setContext("http://fortytwo.net/josh/things/SBZFumn");
//    exoHand.setContext("http://fortytwo.net/josh/things/JdGwZ4n");

    exoHand.playTone(440,100);
}


////////////////////////////////////////////////////////////////////////////////

Vector3D a;

void loop() {
    unsigned long now = exoHand.beginLoop();

    // raw motion data
    exoHand.getAcceleration(a);

    // smoothing
    Vector3D smooth = smoothing.processNext(now, a);

    // high-five recognition
    unsigned long isSpike = spikeDetector.processNext(now, a.getMagnitude());
    if (isSpike > 0) {
        // Since this is a very simple demo we do not filter by direction.
        // Simultaneous peaks are sufficient.
        exoHand.setColorFor(RGB_YELLOW, 300);
        //exoHand.playTone(880, 75);
        emitGesture(HIGHFIVE, isSpike, now);
    }
}
