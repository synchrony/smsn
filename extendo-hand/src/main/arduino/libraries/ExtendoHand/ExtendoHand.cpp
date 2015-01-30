/*
  ExtendoHand.cpp
  Created by Joshua Shinavier, 2012-2014
  Released into the public domain.
*/

#include "ExtendoHand.h"


////////////////////////////////////////////////////////////////////////////////

#define MOTION_X_PIN A0
#define MOTION_Y_PIN A1
#define MOTION_Z_PIN A2

#define SPEAKER_PIN  9  // PWM preferred
#define RGB_LED_PIN  13
#define VIBRO_PIN    2


////////////////////////////////////////////////////////////////////////////////

ExtendoHand::ExtendoHand(): ExtendoDevice(OSC_EXO_HAND) {
    nineAxis = true;
}

void ExtendoHand::useNineAxisSensors(boolean b) {
    nineAxis = b;
}


////////////////////////////////////////////////////////////////////////////////

// note: enclosing in an ifdef to reduce program size
#ifdef THREE_AXIS
#include <MMA7361.h>

MMA7361 motionSensor(MOTION_X_PIN, MOTION_Y_PIN, MOTION_Z_PIN);
#endif // THREE_AXIS

// nine-axis

#include <Wire.h>
#include <I2Cdev.h>

#include <ADXL345.h>

#ifdef ENABLE_GYRO
#include <ITG3200.h>
ITG3200 gyro;
#endif // ENABLE_GYRO

#ifdef ENABLE_MAGNETOMETER
#include <HMC5883L.h>
HMC5883L magnet;
#endif // ENABLE_MAGNETOMETER

ADXL345 accel;


////////////////////////////////////////////////////////////////////////////////
// output signals

/*
int morseStopTest() {
    // no need to abort
    return 0;
}

Morse *ExtendoHand::createMorse() {
    return new Morse(SPEAKER_PIN, morseStopTest);
}
*/

/*
Droidspeak *ExtendoHand::createDroidspeak() {
    return new Droidspeak(SPEAKER_PIN);
}
*/

#include <Adafruit_NeoPixel.h>

Adafruit_NeoPixel leds = Adafruit_NeoPixel(1, RGB_LED_PIN, NEO_GRB + NEO_KHZ800);

void ExtendoHand::setColor(unsigned long color) {
    leds.setPixelColor(0, (uint32_t) color);
    leds.show();
}

void ExtendoHand::vibrate(unsigned long durationMs) {
    digitalWrite(VIBRO_PIN, HIGH);
    delay(durationMs);
    digitalWrite(VIBRO_PIN, LOW);
}

// note: this is a synchronous/blocking tone for now, to match the Typeatron's behavior
void ExtendoHand::playTone(unsigned int frequency, unsigned long durationMs) {
    if (frequency) {
        tone(SPEAKER_PIN, frequency);
        delay(durationMs);
        noTone(SPEAKER_PIN);
    } else {
        delay(durationMs);
    }
}


////////////////////////////////////////////////////////////////////////////////
// setup

void ExtendoHand::setupPins() {
    pinMode(VIBRO_PIN, OUTPUT);
    leds.begin();
}

void ExtendoHand::setupOther() {
    if (nineAxis) {
        // adjust the power settings after you call this method if you want the accelerometer
        // to enter standby mode, or another less demanding mode of operation
        accel.setRange(1); // 4g
        accel.setFullResolution(1); // maintain 4mg/LSB scale factor (irrespective of range)
        accel.initialize();
        if (!accel.testConnection()) {
            osc.sendError("ADXL345 connection failed");
        } else {
            randomSeed(accel.getAccelerationX() - accel.getAccelerationY() + accel.getAccelerationZ());
        }

#ifdef ENABLE_GYRO
        gyro.initialize();
        if (!gyro.testConnection()) {
            osc.sendError("ITG3200 connection failed");
        }
#endif // ENABLE_GYRO

#ifdef ENABLE_MAGNETOMETER
        magnet.initialize();
        if (!magnet.testConnection()) {
            osc.sendError("HMC5883L connection failed");
        }
#endif // ENABLE_MAGNETOMETER
    } else {
#ifdef THREE_AXIS
        randomSeed(motionSensor.rawX() + motionSensor.rawY() + motionSensor.rawZ());

        // 1.5g constants, sampled 2014-06-21
        motionSensor.calibrateX(272, 794);
        motionSensor.calibrateY(332, 841);
        motionSensor.calibrateZ(175, 700);
#endif // THREE_AXIS
    }
}

////////////////////////////////////////////////////////////////////////////////

void ExtendoHand::getAcceleration(Vector3D &a) {
    if (nineAxis) {
        int16_t ax, ay, az;
        accel.getAcceleration(&ax, &ay, &az);

        // approximate g values, per calibration with a specific sensor
        a.set(
            ax / 230.0 - 0.05,
            ay / 230.0,
            az / 230.0);
    } else {
#ifdef THREE_AXIS
        a.set(
            motionSensor.accelX(),
            motionSensor.accelY(),
            motionSensor.accelZ());
#endif // THREE_AXIS
    }
}

void ExtendoHand::getRotation(Vector3D &g) {
    if (nineAxis) {
#ifdef ENABLE_GYRO
        int16_t gx, gy, gz;
        gyro.getRotation(&gx, &gy, &gz);
        g.set(gx, gy, gz);
#else
        g.set(0, 0, 0);
#endif // ENABLE_GYRO
    } else {
        g.set(0, 0, 0);
    }
}

void ExtendoHand::getHeading(Vector3D &m) {
    if (nineAxis) {
#ifdef ENABLE_MAGNETOMETER
        int16_t mx, my, mz;
        magnet.getHeading(&mx, &my, &mz);
        m.set(mx, my, mz);
#else
        m.set(0, 0, 0);
#endif // ENABLE_MAGNETOMETER
    } else {
        m.set(0, 0, 0);
    }
}

void ExtendoHand::onBeginLoop(unsigned long now) {
    // do nothing
}

void ExtendoHand::onLoopTimeUpdated(double loopTime) {
    if (nineAxis) {
        // the sensor's sampling rate should exceed Extend-o-Hand's loop rate
        uint8_t sampleRate = loopTime <= 0.00125
            ? 0xe  // 1600 Hz
            : loopTime <= 0.0025
            ? 0xd  // 800 Hz
            : 0xc; // 400 Hz

        // uncomment only for development
        //osc.sendInfo("setting sensor sampling rate to %d based on loop time of %d micros",
        //    sampleRate, (int)(loopTime*1000000));

        // adjust the power settings after you call these methods if you want the sensors
        // to enter standby mode, or another less demanding mode of operation
        accel.setRate(sampleRate);
#ifdef ENABLE_GYRO
        gyro.setRate(sampleRate);
#endif // ENABLE_GYRO
#ifdef ENABLE_MAGNETOMETER
        magnet.setDataRate(sampleRate);
#endif // ENABLE_MAGNETOMETER
    }
}


////////////////////////////////////////////////////////////////////////////////

bool ExtendoHand::handleOSCBundle(class OSCBundle &bundle) {
    // no Extend-o-Hand -specific handlers
    return 0;
}
