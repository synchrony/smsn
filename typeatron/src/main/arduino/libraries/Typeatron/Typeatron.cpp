/*
  Typeatron.cpp
  Created by Joshua Shinavier, 2013-2014
  Released into the public domain.

  D0:  Bluetooth RX
  D1:  Bluetooth TX
  D2:  push button 1
  D3:  vibration motor
  D4:  push button 2
  D5:  tactile transducer
  D6:  laser
  D7:  push button 3
  D8:  push button 4
  D9:  RGB LED red
  D10: RGB LED green
  D11: RGB LED blue
  D12: push button 5
  D13: LED
  A0:  (unused)
  A1:  (unused)
  A2:  piezo motion sensor
  A3:  photoresistor
  A4:  I2C SDA for MPU-6050
  A5:  I2C SCL for MPU-6050
  A6:  Bluetooth RTS
  A7:  Bluetooth CTS
*/

#include "Typeatron.h"
#include "Arduino.h"

const int keyPin1 = 2;
const int keyPin2 = 4;
const int keyPin3 = 7;
const int keyPin4 = 8;
const int keyPin5 = 12;
const int vibrationMotorPin = 3;
const int transducerPin = 5;
const int laserPin = 6;
const int ledPin = 13;

// note: blue and green pins were wired in reverse w.r.t. the BL-L515 datasheet
const int redPin = 9;
const int greenPin = 10;
const int bluePin = 11;

const int photoresistorPin = A3;


// note: only fairly low frequencies of flicker are discernible; the laser has more inertia than an LED
const unsigned long laserFlickerDurationMs = 1050;
const unsigned long laserFlickerDarkMs = 30;
const unsigned long laserFlickerLightMs = 45;


Typeatron *thisTypeatron;

Typeatron::Typeatron(): ExtendoDevice(OSC_EXO_TT),
  rgbled(redPin, greenPin, bluePin) {

    thisTypeatron = this;
    photoSampler = new AnalogSampler(photoresistorPin);
}

void Typeatron::setMode(int m) {
    mode = (Mode) m;
}

Morse *Typeatron::getMorse() {
    return morse;
}

Droidspeak *Typeatron::getDroidspeak() {
    return droidspeak;
}

AnalogSampler *Typeatron::getPhotoSampler() {
    return photoSampler;
}

Mode Typeatron::getMode() {
    return mode;
}

////////////////////////////////////////////////////////////////////////////////
// output signals

int morseStopTest() {
    // abort the playing of a Morse code sequence by pressing 3 or more keys at the same time
    return thisTypeatron->getTotalKeysPressed() >= 3;
}

Morse *Typeatron::createMorse() {
    return new Morse(transducerPin, morseStopTest);
}

// note: tones may not be played (via the Typeatron's transducer) in parallel with the reading of button input,
// as the vibration causes the push button switches to oscillate when depressed
Droidspeak *Typeatron::createDroidspeak() {
    return new Droidspeak(transducerPin);
}

void Typeatron::setColor(unsigned long color) {
    rgbled.setColor(color);
}

void Typeatron::vibrate(unsigned long durationMs) {
    digitalWrite(vibrationMotorPin, HIGH);
    delay(durationMs);
    digitalWrite(vibrationMotorPin, LOW);
}

// note: this is a synchronous/blocking tone, as the transducer interferes with button input, also noted above
void Typeatron::playTone(unsigned int frequency, unsigned long durationMs) {
    if (frequency) {
        tone(transducerPin, frequency);
        delay(durationMs);
        noTone(transducerPin);
    } else {
        delay(durationMs);
    }
}

void Typeatron::resetLaser() {
    laserModeHigh = false;
    laserFlickerHigh = true;
    laserFlickerStart = 0;
}

void Typeatron::laserOn() {
    resetLaser();
    laserModeHigh = true;

    digitalWrite(laserPin, HIGH);

    // also turn on the on-board LED, as a cue to the developer in USB mode (when the laser is disconnected)
    digitalWrite(ledPin, HIGH);
}

void Typeatron::laserOff() {
    resetLaser();

    digitalWrite(laserPin, LOW);

    digitalWrite(ledPin, LOW);
}

void Typeatron::laserFeedback() {
    if (!laserModeHigh) {
        return;
    }

    laserFlickerStart = millis();
}


////////////////////////////////////////////////////////////////////////////////
// setup

void Typeatron::setupPins() {
    pinMode(keyPin1, INPUT);
    pinMode(keyPin2, INPUT);
    pinMode(keyPin3, INPUT);
    pinMode(keyPin4, INPUT);
    pinMode(keyPin5, INPUT);

    // take advantage of the Arduino's internal pullup resistors
    digitalWrite(keyPin1, HIGH);
    digitalWrite(keyPin2, HIGH);
    digitalWrite(keyPin3, HIGH);
    digitalWrite(keyPin4, HIGH);
    digitalWrite(keyPin5, HIGH);

    pinMode(vibrationMotorPin, OUTPUT);
    pinMode(transducerPin, OUTPUT);
    pinMode(laserPin, OUTPUT);
    pinMode(ledPin, OUTPUT);

    rgbled.setup();
}

void Typeatron::setupOther() {
    droidspeak = createDroidspeak();
    if (droidspeak) {
        droidspeak->speakPowerUpPhrase();
    }

/*
    // TODO: it is not safe to assume Serial
    // note: this operation is performed below SLIP
    // note: match BAUD_RATE in ExtendOSC.h
    Serial.print("$");  // Print three times individually
    Serial.print("$");
    Serial.print("$");  // Enter command mode
    delay(100);  // Short delay, wait for the modem to send back CMD

    //Serial.println("SM,6"); // pair mode (purportedly)
    Serial.println("f,1"); // fast data mode
    delay(100);
*/

    morse = createMorse();
    resetLaser();
}


////////////////////////////////////////////////////////////////////////////////
// looping

void Typeatron::onBeginLoop(unsigned long now) {
    if (!laserModeHigh) {
        return;
    }

    if (laserFlickerStart > 0) {
        unsigned long elapsed = now - laserFlickerStart;
        if (elapsed >= laserFlickerDurationMs) {
            laserFlickerStart = 0;
        } else {
            unsigned long ms = (elapsed) % (laserFlickerDarkMs + laserFlickerLightMs);
            bool light = ms > laserFlickerDarkMs;
            if (light) {
                if (!laserFlickerHigh) {
                    digitalWrite(laserPin, HIGH);
                    digitalWrite(ledPin, HIGH);
                    laserFlickerHigh = true;
                }
            } else {
                if (laserFlickerHigh) {
                    digitalWrite(laserPin, LOW);
                    digitalWrite(ledPin, LOW);
                    laserFlickerHigh = false;
                }
            }
        }
    }
}

void Typeatron::onLoopTimeUpdated(double loopTime) {
    // do nothing
}


////////////////////////////////////////////////////////////////////////////////
// keyer

unsigned int Typeatron::getKeyState() {
    return keyState;
}

int Typeatron::getTotalKeysPressed() {
    return totalKeysPressed;
}

void Typeatron::updateKeys() {
    keys[0] = !digitalRead(keyPin1);
    keys[1] = !digitalRead(keyPin2);
    keys[2] = !digitalRead(keyPin3);
    keys[3] = !digitalRead(keyPin4);
    keys[4] = !digitalRead(keyPin5);

    keyState = 0;
    totalKeysPressed = 0;

    for (int i = 0; i < 5; i++) {
        keyState |= keys[i] << i;

        if (keys[i]) {
            totalKeysPressed++;
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// OSC in

void handleLaserFeedbackMessage(class OSCMessage &m) {
    thisTypeatron->laserFeedback();
}

void handleLaserOffMessage(class OSCMessage &m) {
    thisTypeatron->laserOff();
}

void handleLaserOnMessage(class OSCMessage &m) {
    thisTypeatron->laserOn();
}

void handleLaserTriggerMessage(class OSCMessage &m) {
    thisTypeatron->setMode(LaserTrigger);
}

const int morseBufferLength = 32;
char morseBuffer[morseBufferLength];

/* TODO: restore; these are commented out so as to save some space
void handleMorseMessage(class OSCMessage &m) {
    if (!thisTypeatron->getOSC()->validArgs(m, 1)) return;

    int length = m.getDataLength(0);
    if (length >= morseBufferLength) {
        thisTypeatron->getOSC()->sendError("Morse message is too long");
        thisTypeatron->errorCue();
    } else {
        m.getString(0, morseBuffer, length+1);
        thisTypeatron->getMorse()->playMorseString((const char*) morseBuffer);
    }
}

void handlePhotoGetMessage(class OSCMessage &m) {
    AnalogSampler *sampler = thisTypeatron->getPhotoSampler();
    sampler->reset();
    sampler->beginSample();
    sampler->measure();
    sampler->endSample();

    thisTypeatron->sendLightLevel();
}
*/

bool Typeatron::handleOSCBundle(class OSCBundle &bundle) {
    return 0
        || bundle.dispatch(address(OSC_LASER_FEEDBACK), handleLaserFeedbackMessage)
        || bundle.dispatch(address(OSC_LASER_OFF), handleLaserOffMessage)
        || bundle.dispatch(address(OSC_LASER_ON), handleLaserOnMessage)
        || bundle.dispatch(address(OSC_LASER_TRIGGER), handleLaserTriggerMessage);
        //|| bundle.dispatch(address(OSC_MORSE), handleMorseMessage)
        //|| bundle.dispatch(address(OSC_PHOTO_GET), handlePhotoGetMessage);
}


////////////////////////////////////////////////////////////////////////////////
// OSC out

void sendAnalogObservation(class ExtendOSC &osc, class AnalogSampler &s, const char* address) {
    OSCMessage m(address);
    m.add((uint64_t) s.getStartTime());
    m.add((uint64_t) s.getEndTime());
    m.add((int) s.getNumberOfMeasurements());
    m.add(s.getMinValue());
    m.add(s.getMaxValue());
    m.add(s.getMean());
    m.add(s.getVariance());

    osc.sendOSC(m);
}

void Typeatron::sendKeyState() {
    char keyStr[6];
    for (int i = 0; i < 5; i++) {
        keyStr[i] = keys[i] + 48;
    }
    keyStr[5] = 0;

    OSCMessage m(address(OSC_KEYS));
    m.add(keyStr);

    osc.sendOSC(m);
}

void Typeatron::sendLightLevel() {
    sendAnalogObservation(osc, *photoSampler, address(OSC_PHOTO_DATA));
}

void Typeatron::sendLaserEvent() {
    OSCMessage m(address(OSC_LASER_EVENT));
    m.add((uint64_t) micros());

    osc.sendOSC(m);
}
