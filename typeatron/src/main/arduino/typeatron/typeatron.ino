/*
 * Monomanual Typeatron firmware, copyright 2013-2014 by Joshua Shinavier
 * See: https://github.com/joshsh/extendo and the Typeatron Mark 1 EAGLE schematic.
 *
 * D0:  Bluetooth RX 
 * D1:  Bluetooth TX
 * D2:  push button 1
 * D3:  vibration motor
 * D4:  push button 2
 * D5:  tactile transducer
 * D6:  laser
 * D7:  push button 3
 * D8:  push button 4
 * D9:  RGB LED red
 * D10: RGB LED green
 * D11: RGB LED blue
 * D12: push button 5
 * D13: LED
 * A0:  piezo motion sensor
 * A1:  (unused)
 * A2:  (unused)
 * A3:  photoresistor
 * A4:  I2C SDA for MPU-6050
 * A5:  I2C SCL for MPU-6050
 */

// OSC addresses
const char *EXO_TT_ERROR         = "/exo/tt/error";
const char *EXO_TT_INFO          = "/exo/tt/info";
const char *EXO_TT_KEYS          = "/exo/tt/keys";
const char *EXO_TT_LASER_EVENT   = "/exo/tt/laser/event";
const char *EXO_TT_LASER_TRIGGER = "/exo/tt/laser/trigger";
const char *EXO_TT_MORSE         = "/exo/tt/morse";
const char *EXO_TT_OK            = "/exo/tt/ok";
const char *EXO_TT_PHOTO_DATA    = "/exo/tt/photo/data";
const char *EXO_TT_PHOTO_GET     = "/exo/tt/photo/get";
const char *EXO_TT_PING          = "/exo/tt/ping";
const char *EXO_TT_PING_REPLY    = "/exo/tt/ping/reply";
const char *EXO_TT_READY         = "/exo/tt/ready";
const char *EXO_TT_RGB_SET       = "/exo/tt/rgb/set";
const char *EXO_TT_TONE          = "/exo/tt/tone";
const char *EXO_TT_VIBRO         = "/exo/tt/vibro";
const char *EXO_TT_WARNING       = "/exo/tt/warning";

// these are global so that we can read from setup() as well as loop()
unsigned int keys[5];
unsigned int keyState;
unsigned int totalKeysPressed;
unsigned int lastKeyState = 0;

const unsigned long infoCueHapticDurationMs = 100;
const unsigned long infoCueVisualDurationMs = 200;
const unsigned long errorCueHapticDurationMs = 100;
const unsigned long errorCueVisualDurationMs = 200;
const unsigned long okCueVisualDurationMs = 100;
const unsigned long readyCueVisualDurationMs = 10000;
const unsigned long warningCueHapticDurationMs = 100;
const unsigned long warningCueVisualDurationMs = 200;


////////////////////////////////////////////////////////////////////////////////

const int keyPin1 = 2;
const int keyPin2 = 4;
const int keyPin3 = 7;
const int keyPin4 = 8;
const int keyPin5 = 12;
const int vibrationMotorPin = 3;
const int transducerPin = 5;  
const int laserPin = 6;
const int ledPin =  13;

// note: blue and green pins were wired in reverse w.r.t. the BL-L515 datasheet
const int redPin = 9;
const int greenPin = 10;
const int bluePin = 11;

const int photoresistorPin = A3;


////////////////////////////////////////////////////////////////////////////////

#include <RGBLED.h>

RGBLED rgbled(redPin, greenPin, bluePin, sendError);

unsigned long ledCueLength = 0;
unsigned long ledCueSince = 0;

void okCue() {
    rgbled.replaceColor(RGB_GREEN);
    ledCueLength = okCueVisualDurationMs;
    ledCueSince = millis();
}

void infoCue() {
    rgbled.replaceColor(RGB_BLUE);
    ledCueLength = infoCueVisualDurationMs;
    ledCueSince = millis();
}

void readyCue() {
    rgbled.replaceColor(RGB_WHITE);
    ledCueLength = readyCueVisualDurationMs;
    ledCueSince = millis();
}

void warningCue() {
    rgbled.replaceColor(RGB_YELLOW);
    ledCueLength = warningCueVisualDurationMs;
    ledCueSince = millis();
}

void errorCue() {
    rgbled.replaceColor(RGB_RED);
    ledCueLength = errorCueVisualDurationMs;
    ledCueSince = millis();
}

void checkLedStatus(unsigned long now) {
    if (ledCueSince > 0 && now - ledCueSince > ledCueLength) {
        rgbled.replaceColor(0);
        ledCueSince = 0;
    }
}


////////////////////////////////////////////////////////////////////////////////

#include <OSCBundle.h>
#include <ExtendOSC.h>

ExtendOSC osc("/exo/tt");

OSCBundle *bundleIn;

void sendError(const char *message) {
   errorCue();
   osc.sendError(message);
}


////////////////////////////////////////////////////////////////////////////////

// TODO: tailor the bounce interval to the switch being used.
// This 2ms value is a conservative estimate based on an average over many kinds of switches.
// See "A Guide to Debouncing" by Jack G. Ganssle
unsigned int debounceMicros = 2000;


////////////////////////////////////////////////////////////////////////////////

#include <AnalogSampler.h>

AnalogSampler photoSampler(photoresistorPin);


////////////////////////////////////////////////////////////////////////////////

#include <Droidspeak.h>

// note: tones may not be played (via the Typeatron's transducer) in parallel with the reading of button input,
// as the vibration causes the push button switches to oscillate when depressed
Droidspeak droidspeak(transducerPin);


////////////////////////////////////////////////////////////////////////////////

void laserOn() {
    digitalWrite(laserPin, HIGH); 
   
    // also turn on the on-board LED, as a cue to the developer in USB mode (when the laser is powered off)
    digitalWrite(ledPin, HIGH);
}

void laserOff() {
    digitalWrite(laserPin, LOW); 
   
    digitalWrite(ledPin, LOW);
}


////////////////////////////////////////////////////////////////////////////////

void vibrateForDuration(unsigned long ms) {
    digitalWrite(vibrationMotorPin, HIGH);
    delay(ms);
    digitalWrite(vibrationMotorPin, LOW);
}


////////////////////////////////////////////////////////////////////////////////

typedef enum { 
    Normal = 0,
    LaserTrigger,
    LaserPointer
} Mode;

Mode mode;

void setMode(int m) {
    mode = (Mode) m;
}


////////////////////////////////////////////////////////////////////////////////

void readKeys() {
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

#include <Morse.h>

int morseStopTest() {
    // abort the playing of a Morse code sequence by pressing 3 or more keys at the same time
    return totalKeysPressed >= 3;
}

Morse *morse;


////////////////////////////////////////////////////////////////////////////////

void setup() {
    pinMode(keyPin1, INPUT);
    pinMode(keyPin2, INPUT);     
    pinMode(keyPin3, INPUT);     
    pinMode(keyPin4, INPUT);     
    pinMode(keyPin5, INPUT);

    pinMode(vibrationMotorPin, OUTPUT);
    pinMode(transducerPin, OUTPUT);
    pinMode(laserPin, OUTPUT);
    pinMode(ledPin, OUTPUT);

    // take advantage of the Arduino's internal pullup resistors
    digitalWrite(keyPin1, HIGH);    
    digitalWrite(keyPin2, HIGH);    
    digitalWrite(keyPin3, HIGH);    
    digitalWrite(keyPin4, HIGH);    
    digitalWrite(keyPin5, HIGH);

    rgbled.setup();

    rgbled.replaceColor(RGB_YELLOW);
    droidspeak.speakPowerUpPhrase();
    vibrateForDuration(200);
    rgbled.replaceColor(RGB_GREEN);
    
    osc.beginSerial();
    droidspeak.speakSerialOpenPhrase();

    bundleIn = new OSCBundle();

    morse = new Morse(transducerPin, morseStopTest, sendError);
    
    rgbled.replaceColor(RGB_BLACK);
}


////////////////////////////////////////////////////////////////////////////////

const int morseBufferLength = 32;
char morseBuffer[morseBufferLength];

void handleOSCBundle(class OSCBundle &bundle) {
    if (bundle.hasError()) {
        errorCue();
        osc.sendOSCBundleError(bundle);
    } else if (!(0
        || bundle.dispatch(EXO_TT_ERROR, handleErrorMessage)
        || bundle.dispatch(EXO_TT_INFO, handleInfoMessage)
        || bundle.dispatch(EXO_TT_LASER_TRIGGER, handleLaserTriggerMessage)
        || bundle.dispatch(EXO_TT_MORSE, handleMorseMessage)
        || bundle.dispatch(EXO_TT_OK, handleOkMessage)
        || bundle.dispatch(EXO_TT_PHOTO_GET, handlePhotoGetMessage)
        || bundle.dispatch(EXO_TT_PING, handlePingMessage)
        || bundle.dispatch(EXO_TT_READY, handleReadyMessage)
        || bundle.dispatch(EXO_TT_RGB_SET, handleRGBSetMessage)
        || bundle.dispatch(EXO_TT_TONE, handleToneMessage)
        || bundle.dispatch(EXO_TT_VIBRO, handleVibroMessage)
        || bundle.dispatch(EXO_TT_WARNING, handleWarningMessage)
        )) {
          for (int i = 0; i < bundle.size(); i++) {
              OSCMessage *m = bundle.getOSCMessage(i);
              char address[256];
              m->getAddress(address);
              errorCue();
              osc.sendError("no handler for address %s", address);
          }
    }
}

void handleErrorMessage(class OSCMessage &m) {
    errorCue();
    vibrateForDuration(errorCueHapticDurationMs);
}

void handleInfoMessage(class OSCMessage &m) {
    infoCue();
    vibrateForDuration(infoCueHapticDurationMs);
}

void handleLaserTriggerMessage(class OSCMessage &m) {
    setMode(LaserTrigger); 
}

void handleMorseMessage(class OSCMessage &m) {
    if (!osc.validArgs(m, 1)) return;

    int length = m.getDataLength(0);
    if (length >= morseBufferLength) {
        osc.sendError("Morse message is too long");
        errorCue();
    } else {
        m.getString(0, morseBuffer, length+1);
        morse->playMorseString((const char*) morseBuffer);
    }
}

void handleOkMessage(class OSCMessage &m) {
    okCue();  
}

void handlePhotoGetMessage(class OSCMessage &m) {
    photoSampler.reset();
    photoSampler.beginSample();
    photoSampler.measure();
    photoSampler.endSample();
   
    sendLightLevel();
}

void handlePingMessage(class OSCMessage &m) {
    sendPingReply();
}

void handleReadyMessage(class OSCMessage &m) {
    readyCue();  
}

void handleRGBSetMessage(class OSCMessage &m) {
    if (!osc.validArgs(m, 1)) return;
  
    int32_t color = m.getInt(0);

    if (color < 0 || color > 0xffffff) {
        errorCue();
        osc.sendError("color out of range: %d", (long) color);
    } else {
        rgbled.replaceColor(color);
    }
}

void handleToneMessage(class OSCMessage &m) {
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
        tone(transducerPin, (int) frequency);
        delay((unsigned long) duration);
        noTone(transducerPin);
    }
}

void handleVibroMessage(class OSCMessage &m) {
    if (!osc.validArgs(m, 1)) return;

    int32_t d = m.getInt(0);

    if (d <= 0) {
        errorCue();
        osc.sendError("duration must be a positive number");
    } else if (d > 60000) {
        errorCue();
        osc.sendError("duration too long");
    } else {
        vibrateForDuration((unsigned long) d);
    }
}

void handleWarningMessage(class OSCMessage &m) {
    warningCue();
    vibrateForDuration(warningCueHapticDurationMs);
}


////////////////////////////////////////////////////////////////////////////////

void sendAnalogObservation(class AnalogSampler &s, const char* address) {
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

void sendKeyEvent(const char *keys) {
    OSCMessage m(EXO_TT_KEYS);
    m.add(keys);

    osc.sendOSC(m);
}

void sendLightLevel() {
    sendAnalogObservation(photoSampler, EXO_TT_PHOTO_DATA);
}

void sendPingReply() {
    OSCMessage m(EXO_TT_PING_REPLY);
    m.add((uint64_t) micros());
    
    osc.sendOSC(m);
}

void sendLaserEvent() {
    OSCMessage m(EXO_TT_LASER_EVENT);
    m.add((uint64_t) micros());
    
    osc.sendOSC(m);
}


////////////////////////////////////////////////////////////////////////////////

void loop() {
    // SLIP+OSC serial input
    if (osc.receiveOSCBundle(*bundleIn)) {
        handleOSCBundle(*bundleIn);
        bundleIn->empty();
        delete bundleIn;
        bundleIn = new OSCBundle();
    }
    
    unsigned long now = millis();
    checkLedStatus(now);
    
    // keying action
    readKeys(); 
    if (keyState != lastKeyState) {
        if (LaserTrigger == mode) {
            if (keyState) {
                laserOn();
                sendLaserEvent();
                setMode(LaserPointer);
            }
        } else if (LaserPointer == mode) {
            if (!keyState) {
                setMode(Normal);
                laserOff();
            }
        } else {       
            unsigned int before = micros();

            char keyStr[6];
            for (int i = 0; i < 5; i++) {
                keyStr[i] = keys[i] + 48;
            }
            keyStr[5] = 0;
    
            sendKeyEvent(keyStr);
    
            unsigned int after = micros();
            
            if (after - before < debounceMicros) {
                delayMicroseconds(debounceMicros - (after - before));
            }
        }
    } 
    lastKeyState = keyState;
}


