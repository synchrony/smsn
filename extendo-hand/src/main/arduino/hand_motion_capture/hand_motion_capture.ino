/*
 * Extend-o-Hand simple motion capture sketch, copyright 2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/extendo
 */


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

void setup() {
    // output only
    exoHand.enableInput(false);

    exoHand.setup();
}


////////////////////////////////////////////////////////////////////////////////

void loop() {
    unsigned long now = exoHand.beginLoop();

    double ax, ay, az, gx, gy, gz, mx, my, mz;

    exoHand.getAcceleration(&ax, &ay, &az);
    exoHand.getRotation(&gx, &gy, &gz);
    exoHand.getHeading(&mx, &my, &mz);

    OSCMessage m("/exo/hand/motion");
    m.add((uint64_t) now);
    //m.add(exoHand.getLoopTime());
    m.add(ax); m.add(ay); m.add(az);
    m.add(gx); m.add(gy); m.add(gz);
    m.add(mx); m.add(my); m.add(mz);
    exoHand.getOSC()->sendOSC(m);
}
