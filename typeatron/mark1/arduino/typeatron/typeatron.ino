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


////////////////////////////////////////////////////////////////////////////////

// send and receive messages using Bluetooth/Amarino as opposed to plain serial
#define USE_BLUETOOTH

// if defined, make serial output more legible
//#define DEBUG

// a simple mode for a mocked-up video demo
#define DEMO


////////////////////////////////////////////////////////////////////////////////

#include <MeetAndroid.h>

// needed for registerFunction
MeetAndroid meetAndroid;

const char ack = 19;
const char startFlag = 18;
//const char abord = 27;


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

AnalogSampler sampler_photoresistor(photoresistorPin);


////////////////////////////////////////////////////////////////////////////////

// prevents red (which otherwise requires a higher resistance) from dominating the RGB LED
const unsigned int RED_FACTOR = 100;

const unsigned long WHITE = 0xffffff;
const unsigned long RED = 0xff0000;
const unsigned long YELLOW = 0xffff00;
const unsigned long GREEN = 0x00ff00;
const unsigned long CYAN = 0x00ffff;
const unsigned long BLUE = 0x0000ff;
const unsigned long PURPLE = 0xff00ff;
const unsigned long BLACK = 0x000000;

unsigned long currentRGBColor = 0;

void writeRGBColor(unsigned long color)
{
    unsigned long red = (color & RED) >> 16;
    unsigned long green = (color & GREEN) >> 8;
    unsigned long blue = (color & BLUE);

    red = (red * RED_FACTOR) / 255;

    analogWrite(redPin, 255 - (unsigned int) red);
    analogWrite(greenPin, 255 - (unsigned int) green);
    analogWrite(bluePin, 255 - (unsigned int) blue);
    
    currentRGBColor = color;
}

int colorToggle = 0;

void colorDebug() {
// don't toggle colors in demo mode
#ifndef DEMO
    if (colorToggle) {
        writeRGBColor(RED);
    } else {
        writeRGBColor(BLUE);
    }
    colorToggle = !colorToggle; 
#endif 
}


////////////////////////////////////////////////////////////////////////////////

void laserOn() {
    digitalWrite(laserPin, HIGH); 
   
    // also turn on the on-board LED, as a cue to the developer in USB mode (when the laser is powered off)
    digitalWrite(ledPin, HIGH);
    
#ifdef DEMO
    writeRGBColor(RED);
#endif
}

void laserOff() {
    digitalWrite(laserPin, LOW); 
   
    digitalWrite(ledPin, LOW);  
    
#ifdef DEMO
    writeRGBColor(BLUE);
#endif
}


////////////////////////////////////////////////////////////////////////////////

void vibrateForDuration(int ms) {
    digitalWrite(vibrationMotorPin, HIGH);
    delay(ms);
    digitalWrite(vibrationMotorPin, LOW);  
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
    tone(transducerPin, 220);
    writeRGBColor(RED);
    digitalWrite(vibrationMotorPin, HIGH);
    delay(200);
    tone(transducerPin, 440);
    writeRGBColor(GREEN);
    delay(200);
    tone(transducerPin, 880);
    writeRGBColor(BLUE);
    digitalWrite(vibrationMotorPin, LOW); 
    delay(200);
    noTone(transducerPin);
    
    //playMorseString("hello, world!", morseStopTest);
}

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
    pinMode(redPin, OUTPUT);
    pinMode(greenPin, OUTPUT);
    pinMode(bluePin, OUTPUT);

    // BlueSMiRF Silver is compatible with any baud rate from 2400-115200
    // Note: the Amarino receiver appears to be compatible with a variety baud rates, as well
    //Serial.begin(115200); 

    // OSCuino: begin SLIPSerial just like Serial
    SLIPSerial.begin(115200);   // set this as high as you can reliably run on your platform
#if ARDUINO >= 100
    while(!Serial) ; // Leonardo "feature"
#endif
       
#ifdef USE_BLUETOOTH
    meetAndroid.registerFunction(receiveBluetoothOSC, 'o');  // 'o' for OSC
#endif
    
    startupSequence();
}


////////////////////////////////////////////////////////////////////////////////

void receiveBluetoothOSC(byte flag, byte numOfValues)
{
    unsigned long lastColor = currentRGBColor;
    writeRGBColor(GREEN);

    // note: length includes the null terminator
    int length = meetAndroid.stringLength();
    
    char buffer[length];
    meetAndroid.getString(buffer);
        
    sprintf(errstr, "received OSC message of length %d to %s", length, buffer);
    sendInfo(errstr); 
    
    //playMorseInt(length, morseStopTest);  

    OSCMessage messageIn;
    
    for (int i = 0; i < length - 1; i++) {
        messageIn.fill(buffer[i]);
    }
    
    if (messageIn.hasError()) {
        sendError("OSC bundle hasError");
    } else {
        int called;
        called = messageIn.dispatch("/exo/tt/morse", receiveMorseMessage);
        called = messageIn.dispatch("/exo/tt/photo/get", receivePhotoGetMessage);
        called = messageIn.dispatch("/exo/tt/ping", receivePingMessage);
        called = messageIn.dispatch("/exo/tt/rgb/set", receiveRgbSetMessage);
        called = messageIn.dispatch("/exo/tt/vibro", receiveVibroMessage);
    }

    messageIn.empty();
    writeRGBColor(lastColor);
}

void sendOSC(class OSCMessage &m) {
#ifdef USE_BLUETOOTH
    // "manually" begin Bluetooth/Amarino message
    SLIPSerial.print(startFlag);
#endif

    SLIPSerial.beginPacket();  
    m.send(SLIPSerial); // send the bytes to the SLIP stream
    SLIPSerial.endPacket(); // mark the end of the OSC Packet
    m.empty(); // free the space occupied by the message
        
#ifdef USE_BLUETOOTH
    // "manually" end Bluetooth/Amarino message
    SLIPSerial.print(ack);
#elif defined(DEBUG)
    // put OSC messages on separate lines so as to make them more readable
    SLIPSerial.println("");
#endif  
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

void receiveMorseMessage(class OSCMessage &m) {
    if (m.isString(0)) {
        int length = m.getDataLength(0);
        char buffer[length+1];
        m.getString(0, buffer, length+1);   
        
        unsigned long lastColor = currentRGBColor;
        writeRGBColor(YELLOW);
        playMorseString(buffer, morseStopTest); 
        writeRGBColor(lastColor);
    } else {
        sendError("expected string argument to Morse code control");
    }  
}

void receivePhotoGetMessage(class OSCMessage &m) {
    sampler_photoresistor.reset();
    sampler_photoresistor.beginSample();
    sampler_photoresistor.measure();
    sampler_photoresistor.endSample();
   
    sendLightLevel();
}

void receivePingMessage(class OSCMessage &m) {
    // send reply as soon as possible
    sendPingReply();
    
    writeRGBColor(GREEN);
    playMorseString("p", morseStopTest);    
}

void receiveRgbSetMessage(class OSCMessage &m) {
    if (m.isInt(0)) {
        unsigned long color = (unsigned long) m.getInt(0);
        writeRGBColor(color);
    } else {
        sendError("expected integer argument to RGB LED control");
    }
}

void receiveVibroMessage(class OSCMessage &m) {
    writeRGBColor(PURPLE);
  //sendInfo("we made it into the vibro control!");
    if (m.isInt(0)) {
        int d = m.getInt(0);
        playMorseInt(d, morseStopTest);
        
        writeRGBColor(YELLOW);
        if (d <= 0) {
            sendError("vibro duration must be a positive number");
        } else if (d > 60000) {
            sendError("exceeded artificial bound of one minute for vibration cue");
        } else {
            vibrateForDuration(d);  
        }  
    } else {
        sendError("expected integer argument to vibro control");
    }
}


////////////////////////////////////////////////////////////////////////////////

void sendError(char *message) {
    OSCMessage m("/exo/tt/error");
    m.add(message);

    sendOSC(m);
}

void sendInfo(char *message) {
    OSCMessage m("/exo/tt/info");
    m.add(message);

    sendOSC(m);
}

void sendKeyEvent(char *keys) {
    OSCMessage m("/exo/tt/keys");
    m.add(keys);

    sendOSC(m);  
}

void sendLightLevel() {
    sendAnalogObservation(sampler_photoresistor, "/exo/tt/photo/data"); 
  
    //Serial.println("");
    //Serial.print(sampler_photoresistor.getMean());
    //Serial.println("");
}

void sendPingReply() {
    OSCMessage m("/exo/tt/ping/reply");
    m.add((int32_t) micros());
    
    sendOSC(m);  
}


////////////////////////////////////////////////////////////////////////////////

void loop() {    
#ifdef USE_BLUETOOTH
    // this must be kept in loop() to receive events via Amarino
    meetAndroid.receive();  
#endif

    readKeys();
    
    if (keyState != lastKeyState) {
        colorDebug();
//sprintf(errstr, "input changed from %d to %d\n", lastKeyState, keyState);
//ƒsendInfo(errstr);  
        
        // bells and whistles
        if (keys[4] == HIGH) {
            laserOn();
        } else {
            laserOff();
        }
      
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
  
    lastKeyState = keyState;  
}


