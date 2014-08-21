/*
 * Extend-o-Hand firmware, copyright 2013-2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/extendo
 */


// send and receive messages using Bluetooth/Amarino as opposed to plain serial
//#define USE_BLUETOOTH

// if defined, make serial output more legible to a human eye
// also accept simple serial input
#define SIMPLE_IO

//#define GESTURE_MODE
#define KEYBOARD_MODE

// output raw accelerometer data in addition to gestures
#define PRINT_SENSOR_DATA

#define NINEAXIS

//#define SEP '\t'
#define SEP ','


////////////////////////////////////////////////////////////////////////////////

#define MOTION_X_PIN A0
#define MOTION_Y_PIN A1
#define MOTION_Z_PIN A2

#define SPEAKER_PIN  8


////////////////////////////////////////////////////////////////////////////////

#include "gesture.h"


////////////////////////////////////////////////////////////////////////////////

#include <MMA7361.h>

MMA7361 motionSensor(MOTION_X_PIN, MOTION_Y_PIN, MOTION_Z_PIN);

#ifdef NINEAXIS
#include <Wire.h>
#include <I2Cdev.h>
#include <ADXL345.h>
#include <ITG3200.h>
#include <HMC5883L.h>

ADXL345 accel;
ITG3200 gyro;
HMC5883L magnet;
#endif

////////////////////////////////////////////////////////////////////////////////

#include <Droidspeak.h>

Droidspeak droidspeak(SPEAKER_PIN);


////////////////////////////////////////////////////////////////////////////////

// This is only necessary for communication *to* the Arduino
// For communication from the Arduino to the Android phone, we use a modified OSCuino
#include <MeetAndroid.h>
MeetAndroid meetAndroid;

const char ack = 19;
const char startFlag = 18;


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

#ifdef GESTURE_MODE
const double lowerBound = 1.25;
const double upperBound = 1.75;
#else
#ifdef KEYBOARD_MODE
const double lowerBound = 1.05;
const double upperBound = 1.20;
#endif
#endif

const int STATE_ONE = 1;
const int STATE_TWO = 2;
const int STATE_THREE = 3;
const int STATE_FOUR = 4;

int state;

// acceleration at last (potential) turning point
double amax;
double ax_max, ay_max, az_max;

// time of last (potential) turning point
int32_t tmax;

// settable identifier which is prepended to each gestural output
char contextName[32];
char inputBuffer[32];

void setup()  
{
    randomSeed(motionSensor.rawX() + motionSensor.rawY() + motionSensor.rawZ());

    droidspeak.speakPowerUpPhrase();

    // OSCuino: begin SLIPSerial just like Serial
    // BlueSMiRF Silver is compatible with any baud rate from 2400-115200
    SLIPSerial.begin(115200);   // set this as high as you can reliably run on your platform
    //SLIPSerial.begin(38400);  // works equally well with 8MHz and 16MHz
#if ARDUINO >= 100
    while (!Serial); // for Arduino Leonardo
#endif

    droidspeak.speakSerialOpenPhrase();

    // 1.5g constants, sampled 2014-06-21
    motionSensor.calibrateX(272, 794);
    motionSensor.calibrateY(332, 841);
    motionSensor.calibrateZ(175, 700);

#ifdef NINEAXIS
    // adjust the power settings after you call this method if you want the accelerometer
    // to enter standby mode, or another less demanding mode of operation
    accel.initialize();
    if (!accel.testConnection()) {
        SLIPSerial.println("ADXL345 connection failed");
    }
    
    gyro.initialize();
    if (!accel.testConnection()) {
        SLIPSerial.println("ITG3200 connection failed");
    }
    
    magnet.initialize();
    if (!accel.testConnection()) {
        SLIPSerial.println("HMC5883L connection failed");
    }
#endif

    meetAndroid.registerFunction(ping, 'p');

    state = STATE_ONE;
    
    strcpy(contextName, "default");
}


////////////////////////////////////////////////////////////////////////////////

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
#elif defined(SIMPLE_IO)
    // put OSC messages on separate lines so as to make them more readable
    SLIPSerial.println("");
#endif  
}


////////////////////////////////////////////////////////////////////////////////

void sendError(const char *message) {
    OSCMessage m("/exo/hand/error");
    m.add(message);

    sendOSC(m);
}

void sendInfo(const char *message) {
    OSCMessage m("/exo/hand/info");
    m.add(message);

    sendOSC(m);
}



////////////////////////////////////////////////////////////////////////////////

#include <Morse.h>

int morseStopTest() {
    // no need to abort
    return 0;
}

void morseSendError(const char *message) {
   sendError(message); 
}

Morse morse(SPEAKER_PIN, morseStopTest, morseSendError);


////////////////////////////////////////////////////////////////////////////////

#ifdef SIMPLE_IO
int inputPos = 0;
#endif

void loop()
{
    // TODO: temporary.  This will slow down gesture recognition
    // you need to keep this in your loop() to receive events
    //meetAndroid.receive();

#ifdef SIMPLE_IO
    while (SLIPSerial.available()) {
        int c = SLIPSerial.read();
        if (13 == c) {
            inputBuffer[inputPos] = 0;
            if (strlen(inputBuffer)) {
                strcpy(contextName, inputBuffer);
                morse.playMorseString(contextName);
            } else {
                droidspeak.speakWarningPhrase();  
            }      
            inputPos = 0;      
        } else {
            inputBuffer[inputPos++] = c;
        }
    }
#endif

    int32_t now = micros();

    double ax, ay, az;
    double a;
    
    ax = motionSensor.accelX();
    ay = motionSensor.accelY();
    az = motionSensor.accelZ();
    
#ifdef PRINT_SENSOR_DATA
    Serial.print(contextName);
    Serial.print(SEP); Serial.print(now);
    Serial.print(SEP); Serial.print(ax);
    Serial.print(SEP); Serial.print(ay);
    Serial.print(SEP); Serial.print(az);
   
#ifdef NINEAXIS
    int16_t ax2, ay2, az2;
    accel.getAcceleration(&ax2, &ay2, &az2);
    Serial.print(SEP); Serial.print("accel");
    Serial.print(SEP); Serial.print(ax2);
    Serial.print(SEP); Serial.print(ay2);
    Serial.print(SEP); Serial.print(az2);
    int16_t gx, gy, gz;
    gyro.getRotation(&gx, &gy, &gz);
    Serial.print(SEP); Serial.print("gyro");
    Serial.print(SEP); Serial.print(gx);
    Serial.print(SEP); Serial.print(gy);
    Serial.print(SEP); Serial.print(gz);
    int16_t mx, my, mz;
    magnet.getHeading(&mx, &my, &mz);
    Serial.print(SEP); Serial.print("magnet");
    Serial.print(SEP); Serial.print(mx);
    Serial.print(SEP); Serial.print(my);
    Serial.print(SEP); Serial.print(mz);
#endif
   
    Serial.print('\n');
#endif
    
    a = sqrt(ax*ax + ay*ay + az*az);
    
    switch (state) {
      case STATE_ONE:
        if (a >= lowerBound) {
          state = STATE_TWO; 
        }
        break;
      case STATE_TWO:
        if (a >= upperBound) {
          state = STATE_THREE; 
          amax = 0; 
        } else if (a < lowerBound) {
          state = STATE_ONE;
        }
        break;
      case STATE_THREE:
        if (a > amax) {
           amax = a;
           ax_max = ax;
           ay_max = ay;
           az_max = az;
           tmax = now;
        }
        
        if (a < upperBound) {
           state = STATE_FOUR;
        }
        break;
      case STATE_FOUR:
        if (a >= upperBound) {
          state = STATE_THREE;  
        } else if (a < lowerBound) {
          state = STATE_ONE;
          
          double gestureVector[3];
          gestureVector[0] = ax_max;
          gestureVector[1] = ay_max;
          gestureVector[2] = az_max;
          const char *gesture = classifyGestureVector(gestureVector, tmax, now);
          
            // gesture event
#ifdef SIMPLE_IO
            // comma-separated format for the gesture event, for ease of importing to R and similar tools
            Serial.print(contextName); Serial.print(',');
            Serial.print(tmax); Serial.print(',');  // time of turning point
            Serial.print(now); Serial.print(',');  // time of recognition
            Serial.print(amax); Serial.print(',');
            Serial.print(ax_max); Serial.print(',');
            Serial.print(ay_max); Serial.print(',');
            Serial.print(az_max); Serial.print(',');
            Serial.println(gesture);        
#else
            OSCMessage m("/exo/hand/gesture");
            m.add(tmax);  // time of turning point
            m.add(now);  // time of recognition
            m.add(amax);
            m.add(ax_max);
            m.add(ay_max);
            m.add(az_max);
            m.add(gesture);
            sendOSC(m);   
#endif
            // play short audio cues associated with gestures
            if (gestureToneLength > 0) {
                tone(SPEAKER_PIN, gestureTone);
                delay(gestureToneLength);
                noTone(SPEAKER_PIN);
                gestureToneLength = 0;
            }
        }
        break;
    }
}


////////////////////////////////////////////////////////////////////////////////

// Amarino-formatted function
void ping(byte flag, byte numOfValues)
{
    OSCMessage m("/exo/hand/ping-reply");
    m.add((int32_t) micros());
    sendOSC(m);
}

