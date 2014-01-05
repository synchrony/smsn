/*
 * Monomanual Typeatron firmware, copyright 2013 by Joshua Shinavier
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
 * A1:  photoresistor
 * A2:  (unused)
 * A3:  (unused)
 * A4:  I2C SDA for MPU-6050
 * A5:  I2C SCL for MPU-6050
 */


////////////////////////////////////////////////////////////////////////////////

// if defined, use more straightforward serial output
//#define DEBUG

// send and receive messages using Bluetooth/Amarino as opposed to plain serial
//#define USE_BLUETOOTH

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
const int ledPin =  13;

// note: blue and green pins were wired in reverse w.r.t. the BL-L515 datasheet
const int redPin = 9;
const int greenPin = 10;
const int bluePin = 11;

const int photoresistorPin = A1;


////////////////////////////////////////////////////////////////////////////////

char serialInputStr[256];

unsigned int serialInputPtr = 0;

// TODO: tailor the bounce interval to the switch being used.
// This 2ms value is a conservative estimate based on an average over many kinds of switches.
// See "A Guide to Debouncing" by Jack G. Ganssle
unsigned int debounceMicros = 2000;

char errstr[128];


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

struct StateNode {
    char *symbol;
    StateNode *nextNodes[5];
};

struct StateNode *newStateNode() {
    StateNode *n = (StateNode*) malloc(sizeof(StateNode));

    n->symbol = NULL;

    for (int i = 0; i < 5; i++) {
        n->nextNodes[i] = NULL;
    }

    return n;
}

void destroyStateNode(StateNode *n) {
    if (n->symbol) {
        free(n->symbol);
    }

    for (int i = 0; i < 5; i++) {
        StateNode *next = n->nextNodes[i];
        if (next) {
            destroyStateNode(next);
        }
    }

    free(n);
}

StateNode *stateTree;
StateNode *currentButtonState;
int totalButtonsCurrentlyPressed;

void addKeyMapping(const char *sequence, const char *symbol) {
    StateNode *cur = stateTree;
    int l = strlen(sequence);
    for (int j = 0; j < l; j++) {
//sprintf(errstr, "\t%d", sequence[j]-48);
//SLIPSerial.println(errstr);
        int index = sequence[j] - 49;
        StateNode *next = cur->nextNodes[index];
        if (!next) {
            next = newStateNode();
            cur->nextNodes[index] = next;
        }

        cur = next;
    }

    if (cur->symbol) {
        sprintf(errstr, "conflicting symbols for sequence %s", sequence);
//SLIPSerial.println(errstr);
//            error(errstr);
    } else {
        cur->symbol = (char*) malloc(strlen(symbol) + 1);
        strcpy(cur->symbol, symbol);  
    }
}

void setupParser() {  
    // TODO: shouldn't assume the device powers up with no buttons pressed, although this is likely
    totalButtonsCurrentlyPressed = 0;

    stateTree = newStateNode();
    currentButtonState = stateTree;
    
    // map keys in individual calls, rather than wasting valuable SRAM on a temporary array
    addKeyMapping("2112", "a");
    //addKeyMapping("2121", ""},
    addKeyMapping("2332", "e");
    addKeyMapping("2323", "w");
    addKeyMapping("2442", "i");
    addKeyMapping("2424", "y");
    addKeyMapping("2552", "o");
    addKeyMapping("2525", "u");
    addKeyMapping("3113", "p");
    addKeyMapping("3131", "b");
    addKeyMapping("3223", "t");
    addKeyMapping("3232", "d");
    addKeyMapping("3443", "k");
    addKeyMapping("3434", "g");
    addKeyMapping("3553", "q");
    //addKeyMapping("3535", ""},
    addKeyMapping("4114", "f");
    addKeyMapping("4141", "v");
// sacrifice some keys to save memory (otherwise, we run into the stack)
/*
    addKeyMapping("4224", "c");
    addKeyMapping("4242", "j");
*/
    addKeyMapping("4334", "s");
    addKeyMapping("4343", "z");
    addKeyMapping("4554", "h");
    addKeyMapping("4545", "x");
    addKeyMapping("5115", "m");
    //addKeyMapping("5151", ""},
    addKeyMapping("5225", "n");
    //addKeyMapping("5252", ""},
    addKeyMapping("5335", "l");
    //addKeyMapping("5353", ""},
    addKeyMapping("5445", "r");
    //addKeyMapping("5454", ""}

//SLIPSerial.println("done setting up parser");
}

// note: provided on general principle, but never actually used, as loop() never exits
void teardownParser() {
    destroyStateNode(stateTree);
}

// buttonIndex: 0 (thumb) through 4 (pinky)
void buttonEvent(int buttonIndex) {
    if (currentButtonState) {
        currentButtonState = currentButtonState->nextNodes[buttonIndex];
    }
}

void buttonPressed(int buttonIndex) {
//sprintf(errstr, "buttonPressed(%d)", buttonIndex);
//SLIPSerial.println(errstr);
    totalButtonsCurrentlyPressed++;

    buttonEvent(buttonIndex);
}

void buttonReleased(int buttonIndex) {
//sprintf(errstr, "buttonReleased(%d)", buttonIndex);
//SLIPSerial.println(errstr);
    totalButtonsCurrentlyPressed--;

    buttonEvent(buttonIndex);

    // at present, events are triggered when the last key of a sequence is released
    if (0 == totalButtonsCurrentlyPressed) {
//SLIPSerial.println("done");
        if (currentButtonState) {
//SLIPSerial.println("\tstate-non-null");          
            // TODO
            const char *symbol = currentButtonState->symbol;
            if (symbol) {
//sprintf("\t%s", symbol);
SLIPSerial.println(symbol);      

/*
                OSCMessage m("/exo/tt/key");
                m.add(symbol);
                sendOSC(m);
*/
                //Serial.println(symbol);
            }
//else {
//sprintf(errstr, "\tchildren: %d %d %d %d %d", currentButtonState->nextNodes[0], currentButtonState->nextNodes[1], currentButtonState->nextNodes[2], currentButtonState->nextNodes[3], currentButtonState->nextNodes[4]);
//SLIPSerial.println(errstr);
//}
        }

        currentButtonState = stateTree;
    }
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
   
//#ifdef USE_BLUETOOTH
//    meetAndroid.registerFunction(receiveBluetoothOSC, 'e');
//#endif

    serialInputPtr = 0; 

    setupParser();

    startupSequence();
}


////////////////////////////////////////////////////////////////////////////////

void RGBLEDcontrol(class OSCMessage &m) {
  error("we made it into the RGB control!");
    if (m.isInt(0)) {
        unsigned long color = (unsigned long) m.getInt(0);
        writeRGBColor(color);
    } else {
        error("expected integer argument to RGB LED control");
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
error(errstr);              
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
                error("OSC bundle hasError");
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

void error(char *message) {
    OSCMessage m("/exo/tt/error");
    m.add(message);

    sendOSC(m);
}


////////////////////////////////////////////////////////////////////////////////

unsigned int lastInput[5] = {0,0,0,0,0};

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

    unsigned int input[5];
    unsigned inputState = 0;
      
    input[0] = digitalRead(keyPin1);
    input[1] = digitalRead(keyPin2);
    input[2] = digitalRead(keyPin3);
    input[3] = digitalRead(keyPin4);
    input[4] = digitalRead(keyPin5);

    // bells and whistles
    if (input[0] == HIGH) {     
        digitalWrite(ledPin, HIGH); 
        tone(transducerPin, 440); 
    } else {
        digitalWrite(ledPin, LOW);
        noTone(transducerPin);
    }
    if (input[4] == HIGH) {
        digitalWrite(laserPin, HIGH);
    } else {
        digitalWrite(laserPin, LOW);
    }

    int changed = 0;
    unsigned int before;

    for (int i = 0; i < 5; i++) {
        // Generally, at most one button should change per time step
        // However, if two buttons change state, it is an arbitrary choice w.r.t. which one changed first
        if (input[i] != lastInput[i]) {
            before = micros();
            changed = true;
            if (input[i]) {
                buttonPressed(i);
            } else {
                buttonReleased(i);
            }
        }
        
        lastInput[i] = input[i];
    }

//        char keys[6];
//        for (int i = 0; i < 5; i++) {
//            keys[i] = input[i] + 48;
//        }
//        keys[5] = 0;
//
//        OSCMessage m("/exo/tt/keys");
//        m.add(keys);
//
//        sendOSC(m);

    // artificial delay to prevent spurious button presses and releases due to bouncing switches
    if (changed) {
        unsigned int after = micros();
        if (after - before < debounceMicros) {
            delayMicroseconds(debounceMicros - (after - before));
        }
    }   
}


