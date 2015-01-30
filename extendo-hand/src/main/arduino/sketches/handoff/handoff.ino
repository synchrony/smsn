/*
 * Extend-o-Hand handshake sketch, copyright 2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/extendo
 */


#define THREEAXIS             0
#define NINEAXIS              1

#include <ExtendoHand.h>
#include <SpikeDetector.h>
#include <Vector3D.h>
//#include <VectorFilter.h>
#include <ScalarFilter.h>
#include <CircularBufferFilter.h>
#include <HighPassFilter.h>
#include <LowPassFilter.h>
#include <BooleanFilter.h>
#include <DirectionFilter.h>

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

const double buildupPeriod = 1.6; // seconds
//const double minTimestep = 0.0025; // seconds; expect to sample no faster than 400Hz
const double minTimestep = 0.25;

// band-pass filtering constants for "buildup", derived on 2014-12-09
const double rcLow = 0.6114599;
const double rcHigh = 0.4090329;

ExtendoHand exoHand;

// require 800ms between spikes, which is around one quarter period of a "give"
// This ensures that spikes are relatively isolated before the gesture, cutting down on false positives
// during noisy intervals.  Giver and taker must suddenly spike at the same time, apart from spiking in the
// appropriate directions.
unsigned long interSpikeDelay = 800;

double spikeThreshold = 2.0;

SpikeDetector *spikeDetector;
LowPassFilter buildupXLowPass(rcLow), buildupYLowPass(rcLow), buildupZLowPass(rcLow);
HighPassFilter buildupXHighPass(rcHigh), buildupYHighPass(rcHigh), buildupZHighPass(rcHigh);

// note: not using CircularBufferFilter's period and timestep as intended; just creating a simple, small buffer
const int smoothingBufferSize = 10;
CircularBufferFilter smoothingX(smoothingBufferSize, 1);
CircularBufferFilter smoothingY(smoothingBufferSize, 1);
CircularBufferFilter smoothingZ(smoothingBufferSize, 1);

// values derived on 2014-12-09 from the 2014-11-25 handoff data using a circular buffer
Vector3D giveSpikeDirection(0.686863, -0.6251692, 0.3706516);
Vector3D takeSpikeDirection(0.8353023, -0.49748, -0.2340593);
DirectionFilter giveSpikeFilter(giveSpikeDirection, 0.3498809 * 1.5);
DirectionFilter takeSpikeFilter(takeSpikeDirection, 0.3846736 * 1.5);

//VectorFilter *buildupFilter;

////////////////////////////////////////////////////////////////////////////////

unsigned long loopTimeMs;

void loopTimeUpdated(double loopTime) {
    //buildupXBufferFilter->updateTimestep(loopTime);
    //buildupYBufferFilter->updateTimestep(loopTime);
    //buildupZBufferFilter->updateTimestep(loopTime);
    buildupXLowPass.updateTimestep(loopTime);
    buildupYLowPass.updateTimestep(loopTime);
    buildupZLowPass.updateTimestep(loopTime);
    buildupXHighPass.updateTimestep(loopTime);
    buildupYHighPass.updateTimestep(loopTime);
    buildupZHighPass.updateTimestep(loopTime);

    loopTimeMs = (unsigned long) (loopTime * 1000);

    //exoHand.getOSC()->sendInfo("timestep: %d/1000000", (int) (loopTime * 1000000));
}

void emitGesture(const char *name, unsigned long tRef, unsigned long now) {
//exoHand.getOSC()->sendInfo("got a gesture");
exoHand.playTone(880,75); exoHand.playTone(440,75);
//*
    OSCMessage m(exoHand.address(OSC_GESTURE));

    m.add(exoHand.getContext());
    
    m.add(name);
    
    // note: recognition is delayed by about one time step from the actual crest,
    // so we subtract loopTimeMs
    m.add(0);
    // TODO: restore
    //m.add((uint64_t) (tRef-loopTimeMs));
    
    // also add the time of recognition
    m.add((uint64_t) now);

    exoHand.getOSC()->sendOSC(m);
    //*/
}


////////////////////////////////////////////////////////////////////////////////

void setup() {
    exoHand.setLoopTimeHandler(loopTimeUpdated);
    exoHand.setup();

    spikeDetector = new SpikeDetector(spikeThreshold, 800);

    //buildupXBufferFilter = new CircularBufferFilter(buildupPeriod, minTimestep, exoHand.getOSC());
    //buildupYBufferFilter = new CircularBufferFilter(buildupPeriod, minTimestep, exoHand.getOSC());
    //buildupZBufferFilter = new CircularBufferFilter(buildupPeriod, minTimestep, exoHand.getOSC());
    //buildupFilter = new VectorFilter(buildupXBufferFilter, buildupYBufferFilter, buildupZBufferFilter);
    
    exoHand.playTone(440,100);
}


////////////////////////////////////////////////////////////////////////////////

Vector3D a;
Vector3D lp, hp;
Vector3D smooth;

void loop() {
    unsigned long now = exoHand.beginLoop();

    double ax, ay, az, gx, gy, gz, mx, my, mz;

    exoHand.getAcceleration(&ax, &ay, &az);
    //exoHand.getRotation(&gx, &gy, &gz);
    //exoHand.getHeading(&mx, &my, &mz);
    
    double sx = smoothingX.processNext(now, ax);
    double sy = smoothingX.processNext(now, ay);
    double sz = smoothingX.processNext(now, az);
    smooth.set(sx, sy, sz);
     
    a.set(ax, ay, az);
    double amag = a.getMagnitude();
    
    double xhp = buildupXHighPass.processNext(now, ax);   
    double yhp = buildupYHighPass.processNext(now, ay);   
    double zhp = buildupZHighPass.processNext(now, az);   
    hp.set(xhp, yhp, zhp);
    double hpMag = hp.getMagnitude();
    
    double xlp = buildupXLowPass.processNext(now, xhp);
    double ylp = buildupYLowPass.processNext(now, yhp);
    double zlp = buildupZLowPass.processNext(now, zhp);
    lp.set(xlp, ylp, zlp);
    double lpMag = lp.getMagnitude();
    
    /*
    double xlp = buildupXLowPass.processNext(now, ax);
    double ylp = buildupYLowPass.processNext(now, ay);
    double zlp = buildupZLowPass.processNext(now, az);
    lp.set(xlp, ylp, zlp);
    double lpMag = lp.getMagnitude();
    double xhp = buildupXHighPass.processNext(now, xlp);   
    double yhp = buildupYHighPass.processNext(now, ylp);   
    double zhp = buildupZHighPass.processNext(now, zlp);   
    hp.set(xhp, yhp, zhp);
    double hpMag = hp.getMagnitude();
    */
    
    //Vector3D *buildup = buildupFilter->processNext(now, &a);
    //double bmag = buildup->getMagnitude();
    
    unsigned long isSpike = spikeDetector->processNext(now, amag);
    if (isSpike > 0) {
        exoHand.warningCue();
        if (giveSpikeFilter.process(smooth) || takeSpikeFilter.process(smooth)) {
            exoHand.okCue();
            emitGesture("give-take", isSpike, now);  
        }
        /*
        if (giveSpikeFilter.process(smooth)) {
            exoHand.infoCue();
            emitGesture("give", isSpike, now);
        }
        // A spike can be both a candidate "give" and a candidate "take" if the filter regions overlap.
        // The distinction will be made in the gestural server
        if (takeSpikeFilter.process(smooth)) {
            exoHand.okCue();  
            emitGesture("take", isSpike, now);
        }
        */
        
        /*
        double give = giveSpikeFilter.processFuzzy(&smooth);
        double take = takeSpikeFilter.processFuzzy(&smooth);
        if (give <= take) {
            if (give >= 0) {
                exoHand.infoCue();        
            }
        } else {
            exoHand.okCue();          
        }
        */
        
        /*
        if (giveSpikeFilter.process(smooth)) {
            exoHand.infoCue();
        } else if (takeSpikeFilter.process(smooth)) {
            exoHand.okCue();
        } else {
            exoHand.warningCue();
        }*/
    }

    /*
    OSCMessage m("/exo/hand/bandpass");
    m.add(exoHand.getLoopTime());
    //m.add(ax); m.add(ay); m.add(az);
    m.add(amag);
    //m.add(lpMag);
    m.add(hpMag);
    m.add(isSpike > 0 ? 1 : 0);
    exoHand.getOSC()->sendOSC(m);
    
    //exoHand.playTone(880,100);
    //*/
}
