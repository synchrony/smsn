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
 * D13: LED and Bluetooth power
 * A0:  piezo motion sensor
 * A1:  (unused)
 * A2:  (unused)
 * A3:  photoresistor
 * A4:  I2C SDA for MPU-6050
 * A5:  I2C SCL for MPU-6050
 */


////////////////////////////////////////////////////////////////////////////////

// if defined, use more straightforward serial output
//#define DEBUG

// send and receive messages using Bluetooth/Amarino as opposed to plain serial
#define USE_BLUETOOTH

#ifdef DEBUG
#define EOL '@'
#else
#define EOL '\n'
#endif


////////////////////////////////////////////////////////////////////////////////

//#include <MeetAndroid.h>

//MeetAndroid meetAndroid;

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
const int bluetoothPowerPin =  13;

// note: blue and green pins were wired in reverse w.r.t. the BL-L515 datasheet
const int redPin = 9;
const int greenPin = 10;
const int bluePin = 11;

const int photoresistorPin = A3;


////////////////////////////////////////////////////////////////////////////////

char serialInputStr[256];

unsigned int serialInputPtr = 0;

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

int colorToggle = 0;

void colorDebug() {
    if (colorToggle) {
        writeRGBColor(RED);
    } else {
        writeRGBColor(BLUE);
    }
    colorToggle = !colorToggle;  
}


////////////////////////////////////////////////////////////////////////////////

// these are global so that we can read from setup() as well as loop()
unsigned int keys[5];
unsigned keyState;
    
unsigned int lastKeyState = 0;

void readKeys() {
    keys[0] = !digitalRead(keyPin1);
    keys[1] = !digitalRead(keyPin2);
    keys[2] = !digitalRead(keyPin3);
    keys[3] = !digitalRead(keyPin4);
    keys[4] = !digitalRead(keyPin5);
    
    keyState = 0;
    
    for (int i = 0; i < 5; i++) {
      keyState |= keys[i] << i;  
    }  
}

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
    pinMode(bluetoothPowerPin, OUTPUT);
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
       
//#ifdef USE_BLUETOOTH
//    meetAndroid.registerFunction(receiveBluetoothOSC, 'e');
//#endif

    serialInputPtr = 0; 

    initializeBluetooth();
    
    startupSequence();
}


////////////////////////////////////////////////////////////////////////////////

void RGBLEDcontrol(class OSCMessage &m) {
  sendInfo("we made it into the RGB control!");
    if (m.isInt(0)) {
        unsigned long color = (unsigned long) m.getInt(0);
        writeRGBColor(color);
    } else {
        sendError("expected integer argument to RGB LED control");
    }
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


////////////////////////////////////////////////////////////////////////////////

void initializeBluetooth() {
#ifdef USE_BLUETOOTH
    // to start the device with Bluetooth disabled, hold a key while powering up or resetting
    readKeys();
    if (keyState) {
        digitalWrite(bluetoothPowerPin, LOW);   
    } else {
        digitalWrite(bluetoothPowerPin, HIGH); 
    }
    keyState = 0;
#else
    digitalWrite(bluetoothPowerPin, LOW);       
#endif
}


////////////////////////////////////////////////////////////////////////////////

const int STATE_START = 0;
const int STATE_PACKET_BODY = 1;
const int STATE_WAITING_FOR_ACK = 2;

int readState = STATE_START;

OSCBundle bundleIN;

const char amarinoFlag = 'e';

const unsigned int readBufferLength = 1024;
int readBuffer[readBufferLength];
int readBufferPos = 0;

int readBuffer2[readBufferLength];


const int delimiter = ';';

int getArrayLength()
{
        if (readBufferPos == 1) return 0; // only a flag and ack was sent, not data attached
        int numberOfValues = 1;
        // find the amount of values we got
        for (int a=1; a<readBufferPos;a++){
                if (readBuffer[a]==delimiter) numberOfValues++;
        }
        return numberOfValues;
}

void getIntValues(int values[])
{
        int t = 0; // counter for each char based array
        int pos = 0;

        int start = 1; // start of first value
        for (int end=1; end<readBufferPos;end++){
                // find end of value
                if (readBuffer[end]==delimiter) {
                        // now we know start and end of a value
                        char b[(end-start)+1]; // create container for one value plus '\0'
                        t = 0;
                        for(int i = start;i < end;i++){
                                b[t++] = (char)readBuffer[i];
                        }
                        b[t] = '\0';
                        values[pos++] = atoi(b);
                        start = end+1;
                }
        }
        // get the last value
        char b[(readBufferPos-start)+1]; // create container for one value plus '\0'
        t = 0;
        for(int i = start;i < readBufferPos;i++){
                b[t++] = (char)readBuffer[i];
        }
        b[t] = '\0';
        values[pos] = atoi(b);
}

void readSerial() {
    int ch;

    while (SLIPSerial.available() > 0) {
        ch = SLIPSerial.read();

#ifdef USE_BLUETOOTH
        if (ack == ch) {
            for (int i = 0; i < readBufferPos; i++) {
                readBufferPos++;
                
                int l = getArrayLength();

                getIntValues(readBuffer2);
sprintf(errstr, "received a string of length %d", l);
sendInfo(errstr);              
                for (int i = 0; i < l; i++) {
                    bundleIN.fill(readBuffer2[i]);
                }                
            }          
        } else {
            // note: soft fail on buffer overrun may result in invalid, truncated messages
            if (readBufferPos < readBufferLength) {
                readBuffer[readBufferPos++] = ch;
            }
        }
#else
        bundleIN.fill(ch);
        
        if (SLIPSerial.endofPacket()) {
            if (bundleIN.hasError()) {
                sendError("OSC bundle hasError");
            } else {
                bundleIN.dispatch("/exo/tt/rgb", RGBLEDcontrol);
            }

            bundleIN.empty();
        }
#endif
    }
}

void receiveBluetoothOSC(byte flag, byte numOfValues)
{
  // TODO: read arguments
  //int state = meetAndroid.getInt();
 
    writeRGBColor(GREEN);
}

void sendOSC(class OSCMessage &m) {
#ifdef USE_BLUETOOTH
    // "manually" begin Bluetooth/Amarino message
    SLIPSerial.print(startFlag);
#endif

    SLIPSerial.beginPacket();  
    m.send(SLIPSerial); // send the bytes to the SLIP stream
    SLIPSerial.endPacket(); // mark the end of the OSC Packet
    m.empty(); // free space occupied by message
        
#ifdef USE_BLUETOOTH
    // "manually" end Bluetooth/Amarino message
    SLIPSerial.print(ack);
#elif defined(DEBUG)
    // put OSC messages on separate lines so as to make them more readable
    SLIPSerial.println("");
#endif  
}


////////////////////////////////////////////////////////////////////////////////

void sendAnalogObservation(class AnalogSampler &s, char* address) {
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

void receiveLightLevelRequest(byte flag, byte numOfValues) {
    sampler_photoresistor.reset();
    sampler_photoresistor.beginSample();
    sampler_photoresistor.measure();
    sampler_photoresistor.endSample();
    
    sendLightLevel();
}

void receivePing(byte flag, byte numOfValues) {
    sendPingReply();
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
  // TODO: restore me
    //readSerial();
    
//#ifdef USE_BLUETOOTH
    // this must be kept in loop() to receive events via Amarino
//    meetAndroid.receive();  
//#endif

#ifdef DEBUG
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
#endif
      
    readKeys();
    
    if (keyState != lastKeyState) {
        colorDebug();
//sprintf(errstr, "input changed from %d to %d\n", lastKeyState, keyState);
//ƒsendInfo(errstr);  
        
        // bells and whistles
        if (keys[4] == HIGH) {
            digitalWrite(laserPin, HIGH);
        } else {
            digitalWrite(laserPin, LOW);
        }
        if (keys[3] == HIGH) {
            receiveLightLevelRequest(0, 0);  
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


