/*
 * Monomanual Typeatron breadboard prototyping sketch
 *
 * Connect the push button switches to pins 2-6 in a pulldown configuration
 * Connect the piezo buzzer to pin 7 and GND
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
 * A1:  photoresistor
 * A2:  (unused)
 * A3:  (unused)
 * A4:  I2C SDA for MPU-6050
 * A5:  I2C SCL for MPU-6050
 */


////////////////////////////////////////////////////////////////////////////////

// if defined, use more straightforward serial output
#define DEBUG

// send and receive messages using Bluetooth/Amarino as opposed to plain serial
#define USE_BLUETOOTH

#ifdef DEBUG
#define EOL '@'
#else
#define EOL '\n'
#endif


////////////////////////////////////////////////////////////////////////////////

#include <MeetAndroid.h>

MeetAndroid meetAndroid;

const char ack = 19;
const char startFlag = 18;


////////////////////////////////////////////////////////////////////////////////

#include <OSCMessage.h>

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
const int ledPin =  13;

// note: blue and green pins were wired in reverse w.r.t. the BL-L515 datasheet
const int redPin = 9;
const int greenPin = 10;
const int bluePin = 11;

const int photoresistorPin = A1;


////////////////////////////////////////////////////////////////////////////////

unsigned int lastInputState = 0;

char serialInputStr[256];

unsigned int serialInputPtr = 0;

// TODO: tailor the bounce interval to the switch being used.
// This 2ms value is a conservative estimate based on an average over many kinds of switches.
// See "A Guide to Debouncing" by Jack G. Ganssle
unsigned int debounceMicros = 2000;


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

void writeRGBColor(unsigned long color)
{
  unsigned long red = (color & RED) >> 16;
  unsigned long green = (color & GREEN) >> 8;
  unsigned long blue = (color & BLUE);

  red = (red * RED_FACTOR) / 255;

  analogWrite(redPin, 255 - (unsigned int) red);
  analogWrite(greenPin, 255 - (unsigned int) green);
  analogWrite(bluePin, 255 - (unsigned int) blue);
}


////////////////////////////////////////////////////////////////////////////////


void startupSequence() {
    writeRGBColor(RED);
    digitalWrite(vibrationMotorPin, HIGH);
    delay(300);
    writeRGBColor(BLUE);
    digitalWrite(vibrationMotorPin, LOW); 
}

void setup() {
    pinMode(keyPin1, INPUT);     
    pinMode(keyPin2, INPUT);     
    pinMode(keyPin3, INPUT);     
    pinMode(keyPin4, INPUT);     
    pinMode(keyPin5, INPUT);     

    pinMode(vibrationMotorPin, OUTPUT);
    pinMode(transducerPin, OUTPUT); 
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
   
    serialInputPtr = 0; 
   
    startupSequence();
}

void handlePhotoResistorCommand(char *args) {
    if (!strcmp(args, "read")) {
        int v = analogRead(photoresistorPin);
        Serial.print("/exo/tt/photo/value ");
        Serial.println(v);
    }
}

void handleCommand(char *command) {
    char *address = command, *args;
    char *c = strstr(command, " ");

    if (c) {
        *c = 0;
        args = c + 1;  
    } else {
        args = NULL;
    }

    if (!strcmp(address, "/exo/tt/photo")) {
        handlePhotoResistorCommand(args);  
    } else if (!strcmp(address, "/exo/tt/rgb")) {
      
    }
    /*
    Serial.print("got command at address ");
    Serial.print(address);
    if (args) {
        Serial.print(" with args ");
        Serial.println(args);
    } else {
        Serial.println("");
    }*/
}

void loop() {  
    if (Serial.available() > 0) {            
        int c = Serial.read();
        //Serial.print("I received: ");
        //Serial.println(incomingByte, DEC);
        if (EOL == c) {
            serialInputStr[serialInputPtr] = 0;
            //Serial.print("received input: ");
            //Serial.println(serialInputStr);
            
            handleCommand(serialInputStr);
            serialInputPtr = 0;
        } else {
            serialInputStr[serialInputPtr++] = c;  
        }
    }
    
    unsigned int input[5];
    unsigned inputState = 0;
      
    input[0] = digitalRead(keyPin1);
    input[1] = digitalRead(keyPin2);
    input[2] = digitalRead(keyPin3);
    input[3] = digitalRead(keyPin4);
    input[4] = digitalRead(keyPin5);
  
    for (int i = 0; i < 5; i++) {
      inputState |= input[i] << i;  
    }
    
    // bells and whistles
    if (input[0] == HIGH) {     
        digitalWrite(ledPin, HIGH); 
        tone(transducerPin, 440); 
    } else {
        digitalWrite(ledPin, LOW);
        noTone(transducerPin);
    }
    
    if (inputState != lastInputState) {
        unsigned int before = micros();

        char keys[6];
        for (int i = 0; i < 5; i++) {
            keys[i] = input[i] + 48;
        }
        keys[5] = 0;

        OSCMessage msg("/exo/tt/keys");
        msg.add(keys);

#ifdef USE_BLUETOOTH
        // TODO: receive() just once per key press, or once per loop() iteration?
        meetAndroid.receive();

        // "manually" begin Bluetooth/Amarino message
        SLIPSerial.print(startFlag);
#endif

        SLIPSerial.beginPacket();  
        msg.send(SLIPSerial); // send the bytes to the SLIP stream
        SLIPSerial.endPacket(); // mark the end of the OSC Packet
        msg.empty(); // free space occupied by message
        
#ifdef USE_BLUETOOTH
        // "manually" end Bluetooth/Amarino message
        SLIPSerial.print(ack);
#elif defined(DEBUG)
        // put OSC messages on separate lines so as to make them more readable
        SLIPSerial.println("");
#endif

        unsigned int after = micros();
        
        if (after - before < debounceMicros) {
            delayMicroseconds(debounceMicros - (after - before));
        }
    }
  
    lastInputState = inputState;
}
