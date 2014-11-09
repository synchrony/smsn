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
// constants and variables for band-pass filtering

const double lowThreshold = 0.2;
const double highThreshold = 0.3;

// derived on 2014-11-06
const double rcLow = 0.03359582;
const double rcHigh = 0.02259007;

// twice the period, in milliseconds
const unsigned long peakMaxgap = 353;

double lpAlpha;
double hpAlpha;

void loopTimeUpdated(double loopTime) {
    lpAlpha = loopTime / (rcLow + loopTime);
    hpAlpha = rcHigh / (rcHigh + loopTime);
    //exoHand.getOSC()->sendInfo("lpAlpha=%d/10000, hpAlpha=%d/10000", (int)(lpAlpha*10000), (int)(hpAlpha*10000));    
}


////////////////////////////////////////////////////////////////////////////////
// constants and variables for peak detection

double aPeak = 0;
unsigned long tPeak = 0;
boolean high = false;
unsigned long tLast = 0;
boolean addedLast = false;
//double aLast = 0;
//double lpLast = 0;
//double hpLast = 0;

double axLpLast = 0, ayLpLast = 0, azLpLast = 0;
double axHpLast = 0, ayHpLast = 0, azHpLast = 0;

double lowPass(double x, double lpLast) {
    return lpAlpha * x + (1-lpAlpha) * lpLast;  
}

double highPass(double x, double xLast, double hpLast) {
    return hpAlpha * (hpLast + x - xLast);  
}


////////////////////////////////////////////////////////////////////////////////

void setup() {
    exoHand.setLoopTimeHandler(loopTimeUpdated);
    exoHand.setup();
}


////////////////////////////////////////////////////////////////////////////////

void loop() {
    unsigned long now = exoHand.beginLoop();

    double ax, ay, az, gx, gy, gz, mx, my, mz;

    exoHand.getAcceleration(&ax, &ay, &az);
    //exoHand.getRotation(&gx, &gy, &gz);
    //exoHand.getHeading(&mx, &my, &mz);

    /*
    double a = sqrt(ax*ax + ay*ay + az*az);
    double lp = lpAlpha * a + (1-lpAlpha) * lpLast;
    double hp = hpAlpha * (hpLast + lp - lpLast);
    */
    
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
    aLast = a;
    lpLast = lp;
    hpLast = hp;
    */
    
    OSCMessage m("/exo/hand/bandpass");
    m.add(exoHand.getLoopTime());
    //m.add(ax); m.add(ay); m.add(az);
    m.add(a);
    m.add(aLp);
    m.add(aHp);
    exoHand.getOSC()->sendOSC(m);

/*
    if (high) {


    } else {
        if (hp >= lowThreshold) {
            high = true;
        }
    }
*/
}
