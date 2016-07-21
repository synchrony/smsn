/*
 * Extend-o-Hand simple motion capture sketch, copyright 2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/smsn
 */

#define THREEAXIS             0
#define NINEAXIS              1

#include <ExtendoHand.h>
#include <Vector3D.h>

// ExtendoHand dependencies (they are included in ExtendoHand.cpp, but technically they must be included here)
#include <ExtendoDevice.h>
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

void setup() {
    // output only
    exoHand.enableInput(false);

    exoHand.setup();
}

Vector3D a, g, m;

void loop() {
    unsigned long now = exoHand.beginLoop();

    exoHand.getAcceleration(a);
    exoHand.getRotation(g);
    exoHand.getHeading(m);

    OSCMessage msg("/exo/hand/motion");
    msg.add((uint64_t) now);
    //m.add(exoHand.getLoopTime());
    msg.add(a.getX()); msg.add(a.getY()); msg.add(a.getZ());
    msg.add(g.getX()); msg.add(g.getY()); msg.add(g.getZ());
    msg.add(m.getX()); msg.add(m.getY()); msg.add(m.getZ());
    exoHand.getOSC()->sendOSC(msg);
}
