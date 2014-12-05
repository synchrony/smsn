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

// allows OSC to dispatch messages to non-member functions which call member functions
ExtendoHand *instance;

ExtendoHand::ExtendoHand(): osc(EXO_HAND) {
    instance = this;
    lastHeartbeat = 0;
    loopTimeHandler = NULL;
    nineAxis = true;
    setContext("default");
}

void ExtendoHand::setLoopTimeHandler(void (*handler)(double)) {
    loopTimeHandler = handler;
}

void ExtendoHand::enableInput(boolean b) {
    inputEnabled = b;
}

void ExtendoHand::useNineAxisSensors(boolean b) {
    nineAxis = b;
}


////////////////////////////////////////////////////////////////////////////////

ExtendOSC *ExtendoHand::getOSC() {
    return &osc;
}

double ExtendoHand::getLoopTime() {
    return loopTime;
}


////////////////////////////////////////////////////////////////////////////////

// three-axis

#include <MMA7361.h>

MMA7361 motionSensor(MOTION_X_PIN, MOTION_Y_PIN, MOTION_Z_PIN);

// nine-axis

#include <Wire.h>
#include <I2Cdev.h>
#include <ADXL345.h>
#include <ITG3200.h>
#include <HMC5883L.h>

ADXL345 accel;
ITG3200 gyro;
HMC5883L magnet;


////////////////////////////////////////////////////////////////////////////////

#include <Droidspeak.h>

Droidspeak droidspeak(SPEAKER_PIN);


////////////////////////////////////////////////////////////////////////////////

#include <Morse.h>

int morseStopTest() {
    // no need to abort
    return 0;
}

void error(const char *message) {
    instance->getOSC()->sendError(message);
}

Morse morse(SPEAKER_PIN, morseStopTest, error);


////////////////////////////////////////////////////////////////////////////////

#include <RGBLED.h>
#include <Adafruit_NeoPixel.h>

Adafruit_NeoPixel leds = Adafruit_NeoPixel(1, RGB_LED_PIN, NEO_GRB + NEO_KHZ800);

void ExtendoHand::setColor(unsigned long color) {
    leds.setPixelColor(0, (uint32_t) color);
    leds.show();
}


////////////////////////////////////////////////////////////////////////////////

void ExtendoHand::vibrate(unsigned long durationMs) {
    digitalWrite(VIBRO_PIN, HIGH);
    delay(durationMs);
    digitalWrite(VIBRO_PIN, LOW);
}

void ExtendoHand::playTone(unsigned int frequency, unsigned long durationMs) {
    tone(SPEAKER_PIN, frequency, durationMs);
}


////////////////////////////////////////////////////////////////////////////////

void ExtendoHand::setup() {
    pinMode(VIBRO_PIN, OUTPUT);

    leds.begin();
    setColor(RGB_YELLOW);
    droidspeak.speakPowerUpPhrase();

    osc.beginSerial();

    bundleIn = new OSCBundle();

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

        gyro.initialize();
        if (!gyro.testConnection()) {
            osc.sendError("ITG3200 connection failed");
        }

        magnet.initialize();
        if (!magnet.testConnection()) {
            osc.sendError("HMC5883L connection failed");
        }
    } else {
        // TODO: random seed using 9-axis sensor
        randomSeed(motionSensor.rawX() + motionSensor.rawY() + motionSensor.rawZ());

        // 1.5g constants, sampled 2014-06-21
        motionSensor.calibrateX(272, 794);
        motionSensor.calibrateY(332, 841);
        motionSensor.calibrateZ(175, 700);
    }

    // delay the serial open phrase until the random number generator has been seeded
    setColor(RGB_GREEN);
    droidspeak.speakSerialOpenPhrase();

    setColor(RGB_BLACK);

    vibrate(500);
    osc.sendInfo("Extend-o-Hand is ready");

    loopCount = 0;
    loopThreshold = 100;
    // initial value for loop time gets us through the first half second or so
    loopTime = 4.0/1000;
    loopStartTime = millis();
}


////////////////////////////////////////////////////////////////////////////////

const char* ExtendoHand::getContext() {
    return contextName;
}

void ExtendoHand::setContext(const char *context) {
    strcpy(contextName, context);
}

////////////////////////////////////////////////////////////////////////////////

void ExtendoHand::sendPingReply() {
    OSCMessage m(EXO_HAND_PING_REPLY);
    m.add((uint64_t) micros());

    osc.sendOSC(m);
}

#ifdef HEARTBEAT_MS
void ExtendoHand::sendHeartbeatMessage(unsigned long now) {
    OSCMessage m(EXO_HAND_HEARTBEAT);
    m.add((uint64_t) now);
    osc.sendOSC(m);
}
#endif


////////////////////////////////////////////////////////////////////////////////
// non-member OSC handler functions

void handleAudioToneMessage(class OSCMessage &m) {
    if (!osc.validArgs(m, 2)) return;

    int32_t frequency = m.getInt(0);
    int32_t duration = m.getInt(1);

    if (frequency <= 0 || frequency > 20000) {
        osc.sendError("frequency out of range: %d", (int) frequency);
    } else if (duration <= 0) {
        osc.sendError("duration must be a positive number");
    } else if (duration > 60000) {
        osc.sendError("duration too long");
    } else {
        tone(SPEAKER_PIN, (int) frequency);
        delay((unsigned long) duration);
        noTone(SPEAKER_PIN);
    }
}

void handleContextSetMessage(class OSCMessage &m) {
    if (!instance->getOSC()->validArgs(m, 1)) return;

    char buffer[32];
    m.getString(0, buffer, m.getDataLength(0) + 1);
    instance->setContext(buffer);
}

/*
void ExtendoHand::handleMorseMessage(class OSCMessage &m) {
    if (!osc.validArgs(m, 1)) return;

    int length = m.getDataLength(0);
    char buffer[length+1];
    m.getString(0, buffer, length+1);

    morse.playMorseString(buffer);
}
*/

void handlePingMessage(class OSCMessage &m) {
    instance->sendPingReply();
}

void handleRGBSetMessage(class OSCMessage &m) {
    if (!instance->getOSC()->validArgs(m, 1)) return;

    int32_t color = m.getInt(0);

    if (color < 0 || color > 0xffffff) {
        instance->getOSC()->sendError("color out of range: %d", (long) color);
    } else {
        instance->setColor(color);
    }
}

void handleVibroMessage(class OSCMessage &m) {
    if (!instance->getOSC()->validArgs(m, 1)) return;

    int32_t d = m.getInt(0);

    if (d <= 0) {
        instance->getOSC()->sendError("duration must be a positive number");
    } else if (d > 60000) {
        instance->getOSC()->sendError("duration too long");
    } else {
        instance->vibrate((unsigned long) d);
    }
}


////////////////////////////////////////////////////////////////////////////////

void ExtendoHand::handleOSCBundle(class OSCBundle &bundle) {
    if (bundle.hasError()) {
        osc.sendOSCBundleError(bundle);
    } else if (!(0
        || bundle.dispatch(EXO_HAND_AUDIO_TONE, handleAudioToneMessage)
        || bundle.dispatch(EXO_HAND_CONTEXT_SET, handleContextSetMessage)
        //|| bundle.dispatch(EXO_HAND_MORSE, handleMorseMessage)
        || bundle.dispatch(EXO_HAND_PING, handlePingMessage)
        || bundle.dispatch(EXO_HAND_RGB_SET, handleRGBSetMessage)
        || bundle.dispatch(EXO_HAND_VIBRO, handleVibroMessage)
        )) {
        osc.sendError("no messages dispatched");
    }
}


////////////////////////////////////////////////////////////////////////////////

void ExtendoHand::getAcceleration(double *x, double *y, double *z) {
    if (nineAxis) {
        int16_t ax, ay, az;
        accel.getAcceleration(&ax, &ay, &az);

        // approximate g values, per calibration with a specific sensor
        *x = ax / 230.0 - 0.05;
        *y = ay / 230.0;
        *z = az / 230.0;
        //*x = ax/230.0/16;
        //*y = ay/230.0/16;
        //*z = az/230.0/16;
    } else {
        *x = motionSensor.accelX();
        *y = motionSensor.accelY();
        *z = motionSensor.accelZ();
    }
}

void ExtendoHand::getRotation(double *x, double *y, double *z) {
    if (nineAxis) {
        int16_t gx, gy, gz;
        gyro.getRotation(&gx, &gy, &gz);
        *x = gx;
        *y = gy;
        *z = gz;
    } else {
          *x = 0;
          *y = 0;
          *z = 0;
    }
}

void ExtendoHand::getHeading(double *x, double *y, double *z) {
    if (nineAxis) {
        int16_t mx, my, mz;
        magnet.getHeading(&mx, &my, &mz);
        *x = mx;
        *y = my;
        *z = mz;
    } else {
        *x = 0;
        *y = 0;
        *z = 0;
    }
}


////////////////////////////////////////////////////////////////////////////////

void ExtendoHand::updateSamplingRate() {
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
        gyro.setRate(sampleRate);
        magnet.setDataRate(sampleRate);
    }
}

unsigned long ExtendoHand::beginLoop() {
    // no need for micros() here, and it's simpler to avoid overflow
    unsigned long now = millis();

    if (inputEnabled) {
        if (osc.receiveOSCBundle(*bundleIn)) {
            handleOSCBundle(*bundleIn);
            bundleIn->empty();
            delete bundleIn;
            bundleIn = new OSCBundle();
        }
    }

// TODO: never applies
#ifdef HEARTBEAT_MS
    if (now - lastHeartbeat > HEARTBEAT_MS) {
        sendHeartbeatMessage(now);
        lastHeartbeat = now;
    }
#endif // HEARTBEAT_MS

    // periodically adjust the loop time used in band-pass filtering (among potentially other applications)
    if (++loopCount >= loopThreshold) {
        loopTime = (now - loopStartTime)/1000.0/loopThreshold;

        updateSamplingRate();

        if (loopTimeHandler) {
            loopTimeHandler(loopTime);
        }

        loopCount = 0;
        loopStartTime = now;
    }

    return now;
}
