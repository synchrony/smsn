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

// forward declaration for morse.h
void sendError(const char *message);


////////////////////////////////////////////////////////////////////////////////

// send and receive messages using Bluetooth/Amarino as opposed to plain serial
//#define USE_BLUETOOTH


////////////////////////////////////////////////////////////////////////////////

#include <OSCMessage.h>
#include <OSCBundle.h>

#ifdef BOARD_HAS_USB_SERIAL
#include <SLIPEncodedUSBSerial.h>
SLIPEncodedUSBSerial SLIPSerial( thisBoardsSerialUSB );
#else
#include <SLIPEncodedSerial.h>
SLIPEncodedSerial SLIPSerial(Serial);
#endif


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

// TODO: tailor the bounce interval to the switch being used.
// This 2ms value is a conservative estimate based on an average over many kinds of switches.
// See "A Guide to Debouncing" by Jack G. Ganssle
unsigned int debounceMicros = 2000;

char errstr[128];


////////////////////////////////////////////////////////////////////////////////

#include <AnalogSampler.h>

AnalogSampler photoSampler(photoresistorPin);


////////////////////////////////////////////////////////////////////////////////

#include <RGBLED.h>

RGBLED rgbled(redPin, greenPin, bluePin);


int colorToggle = 0;

void colorDebug() {
    long modeColor = getModeColor();
    rgbled.replaceColor(modeColor);
}


////////////////////////////////////////////////////////////////////////////////

#include <Droidspeak.h>

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

void vibrateForDuration(int ms) {
    digitalWrite(vibrationMotorPin, HIGH);
    delay(ms);
    digitalWrite(vibrationMotorPin, LOW);  
}


////////////////////////////////////////////////////////////////////////////////

const int totalModes = 6;

typedef enum { 
    Text = 0,
    LaserTrigger,
    LaserPointer,
    Mash
} Mode;

const char *modeNames[] = {
    "Text",
    "LaserTrigger",
    "LaserPointer",
    "Mash"
};
    
const unsigned long modeColors[] = {
    RGB_BLUE,   // Text
    RGB_BLACK,  // LaserTrigger
    RGB_RED,    // LaserPointer
    RGB_WHITE   // Mash
};

Mode mode;

void setMode(int m) {
    mode = (Mode) m;
}

unsigned long getModeColor() {
    return modeColors[mode];      
}

int modeValueOf(const char *name) {
    int i;
    for (i = 0; i < totalModes; i++) {
        if (!strcmp(name, modeNames[i])) {
            sprintf(errstr, "identified mode as %d", i);
            sendInfo(errstr);
            return i;
        }
    }
    
    sprintf(errstr, "no such mode: %s", name);
    sendError(errstr);
    
    return Text;
}


////////////////////////////////////////////////////////////////////////////////

// these are global so that we can read from setup() as well as loop()
unsigned int keys[5];
unsigned int keyState;
unsigned int totalKeysPressed;

unsigned int lastKeyState = 0;

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

#include "morse.h"

int morseStopTest() {
    // abort the playing of a Morse code sequence by pressing 3 or more keys at the same time
    return totalKeysPressed >= 3; 
}


////////////////////////////////////////////////////////////////////////////////

// note: tones may not be played (via the Typeatron's transducer) in parallel with the reading of button input,
// as the vibration causes the push button switches to oscillate when depressed
void startupSequence() {
    rgbled.replaceColor(RGB_RED);
    droidspeak.speakPowerUpPhrase();
    rgbled.replaceColor(RGB_BLUE);
    digitalWrite(vibrationMotorPin, HIGH);
    delay(200);
    digitalWrite(vibrationMotorPin, LOW);
    rgbled.replaceColor(RGB_GREEN);
    droidspeak.speakSetupCompletedPhrase();

    //playMorseString("hello, world!", morseStopTest);
}

OSCMessage *messageIn;

void setup() {
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

    // OSCuino: begin SLIPSerial just like Serial
    // set this as high as you can reliably run on your platform
    // BlueSMiRF Silver is compatible with any baud rate from 2400-115200
    SLIPSerial.begin(115200);
#if ARDUINO >= 100
    while(!Serial); // Leonardo "feature"
#endif

    //setMode(LowercaseText);
    
    startupSequence(); 
 
    messageIn = new OSCMessage();   
}


////////////////////////////////////////////////////////////////////////////////

const char *BUFFER_FULL_msg = "BUFFER_FULL",
    *INVALID_OSC_msg = "INVALID_OSC",
    *ALLOCFAILED_msg = "ALLOCFAILED",
    *INDEX_OUT_OF_BOUNDS_msg = "INDEX_OUT_OF_BOUNDS",
    *unknown_msg = "unknown";

void sendOSCMessageError(class OSCMessage &message) {
    OSCErrorCode code = message.getError();
    const char *name;

    switch(code) {
      case BUFFER_FULL:
          name = BUFFER_FULL_msg;
          break;
      case INVALID_OSC:
         name = INVALID_OSC_msg;
         break;
      case ALLOCFAILED:
         name = ALLOCFAILED_msg;
         break;
      case INDEX_OUT_OF_BOUNDS:
         name = INDEX_OUT_OF_BOUNDS_msg;
         break;
      default:
         name = unknown_msg;
    } 
    
    sprintf(errstr, "OSC message hasError (error type: %s)", name);
    sendError(errstr);
}


void receiveOSCMessage(class OSCMessage &messageIn) {
    if (messageIn.hasError()) {
        sendOSCMessageError(messageIn);
//sprintf(errstr, "invalid message occupies %d bytes", messageIn.bytes());
//sendInfo(errstr);
    } else {

// temporary: echo the received message
sendOSC(messageIn);

        boolean called = 0
        || messageIn.dispatch("/exo/tt/laser/trigger", receiveLaserTriggerMessage)
        || messageIn.dispatch("/exo/tt/mode", receiveModeMessage)
        || messageIn.dispatch("/exo/tt/morse", receiveMorseMessage)
        || messageIn.dispatch("/exo/tt/photo/get", receivePhotoGetMessage)
        || messageIn.dispatch("/exo/tt/ping", receivePingMessage)
        || messageIn.dispatch("/exo/tt/vibr", receiveVibroMessage)
        ;
    }
}

void sendOSC(class OSCMessage &messageOut) {
    SLIPSerial.beginPacket();  
    messageOut.send(SLIPSerial); // send the bytes to the SLIP stream
    SLIPSerial.endPacket(); // mark the end of the OSC Packet
    messageOut.empty(); // free the space occupied by the message
}

void sendAnalogObservation(class AnalogSampler &s, const char* address) {
    OSCMessage m(address);
    m.add((uint64_t) s.getStartTime());
    m.add((uint64_t) s.getEndTime());
    m.add((int) s.getNumberOfMeasurements());
    m.add(s.getMinValue());
    m.add(s.getMaxValue());
    m.add(s.getMean());
    m.add(s.getVariance());
 
    sendOSC(m); 
}


////////////////////////////////////////////////////////////////////////////////

void receiveLaserTriggerMessage(class OSCMessage &m) {
    setMode(LaserTrigger); 
}

void receiveModeMessage(class OSCMessage &m) {
    if (m.isString(0)) {
        int length = m.getDataLength(0);
        char buffer[length+1];
        m.getString(0, buffer, length+1);
        
        setMode(modeValueOf(buffer));
    } else {
        sendError("expected string-valued mode name"); 
    }
}

void receiveMorseMessage(class OSCMessage &m) {
    int length = m.getDataLength(0);
    char buffer[length+1];
    m.getString(0, buffer, length+1);

    playMorseString(buffer, morseStopTest);
}

void receivePhotoGetMessage(class OSCMessage &m) {
    photoSampler.reset();
    photoSampler.beginSample();
    photoSampler.measure();
    photoSampler.endSample();
   
    sendLightLevel();
}

void receivePingMessage(class OSCMessage &m) {
    // send reply as soon as possible
    sendPingReply();
    
    playMorseString("p", morseStopTest);  
}

void receiveVibroMessage(class OSCMessage &m) {
    rgbled.pushColor(RGB_PURPLE);

    int d = m.getInt(0);
    //playMorseInt(d, morseStopTest);

    if (d <= 0) {
        sendError("vibro duration must be a positive number");
    } else if (d > 60000) {
        sendError("exceeded artificial bound of one minute for vibration cue");
    } else {
        vibrateForDuration(d);
    }

    rgbled.popColor();
}


////////////////////////////////////////////////////////////////////////////////

void sendError(const char *message) {
    rgbled.pushColor(RGB_ORANGE);
    
    OSCMessage m("/exo/tt/error");
    m.add(message);

    sendOSC(m);
    
    rgbled.popColor();
}

void sendInfo(const char *message) {
    rgbled.pushColor(RGB_BLUE);

    OSCMessage m("/exo/tt/info");
    m.add(message);

    sendOSC(m);
    
    rgbled.popColor();
}

void sendKeyEvent(const char *keys) {
    OSCMessage m("/exo/tt/keys");
    m.add(keys);

    sendOSC(m);
}

void sendLightLevel() {
    sendAnalogObservation(photoSampler, "/exo/tt/photo/data");
}

void sendPingReply() {
    OSCMessage m("/exo/tt/ping/reply");
    m.add((int32_t) micros());
    
    sendOSC(m);
}

void sendLaserEvent() {
    OSCMessage m("/exo/tt/laser/event");
    m.add((int32_t) micros());
    
    sendOSC(m);
}


////////////////////////////////////////////////////////////////////////////////

void loop() {    
#ifdef USE_BLUETOOTH
    int done = SLIPSerial.endofPacket();
    
    int size;
    while ((size = SLIPSerial.available()) > 0) {
        while (size--) {
            int c = SLIPSerial.read();
            messageIn->fill(c);
            //sprintf(errstr, "received a byte: %d. bytes: %d, size: %d, hasError: %d", c, messageIn2.bytes(), messageIn2.size(), messageIn2.hasError());
            //sendInfo(errstr);
        }
    }
    done = done || SLIPSerial.endofPacket();
    if (done) {
        receiveOSCMessage(*messageIn);       
        messageIn->empty();
        //messageIn.reset();
        delete messageIn;
        messageIn = new OSCMessage();
    }
#endif
    rgbled.replaceColor(RGB_BLACK);
    
    readKeys();
    
    if (keyState != lastKeyState) {
        colorDebug();

        if (LaserTrigger == mode) {
            if (keyState) {
                laserOn();
                sendLaserEvent();
                setMode(LaserPointer);
            }
        } else if (LaserPointer == mode) {
            if (!keyState) {
                setMode(Text);
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


    int l = analogRead(A3);
    System.out.print("light level: ");
    System.out.println(l);
}


