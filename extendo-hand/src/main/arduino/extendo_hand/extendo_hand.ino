/*
 * Extend-o-Hand firmware, copyright 2013-2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/extendo
 */


//#define GESTURE_MODE
#define KEYBOARD_MODE

// if defined, emit raw motion (accelerometer/gyro/magnetometer) data
#define OUTPUT_SENSOR_DATA

// if defined, recognize and emit gestures
//#define OUTPUT_GESTURES

// if defined, emit info messages with the current sampling rate
//#define OUTPUT_SAMPLING_RATE

//#define THREEAXIS
#define NINEAXIS

// if defined, output a heartbeat message every so many milliseconds
//#define HEARTBEAT_MS 1000

// if enabled, listen for incoming OSC messages.  Otherwise, do not wait for input,
// which permits a higher sampling and output rate
//#define INPUT_ENABLED

#define SIMPLE_OUTPUT

// OSC addresses
const char *EXO_HAND             = "/exo/hand";
const char *EXO_HAND_AUDIO_TONE  = "/exo/hand/audio/tone";
const char *EXO_HAND_CONTEXT_SET = "/exo/hand/context/set";
const char *EXO_HAND_GESTURE     = "/exo/hand/gesture";
const char *EXO_HAND_HEARTBEAT   = "/exo/hand/heartbeat";
const char *EXO_HAND_INFO        = "/exo/hand/info";
const char *EXO_HAND_MORSE       = "/exo/hand/morse";
const char *EXO_HAND_MOTION      = "/exo/hand/motion";
const char *EXO_HAND_PING        = "/exo/hand/ping";
const char *EXO_HAND_PING_REPLY  = "/exo/hand/ping/reply";


////////////////////////////////////////////////////////////////////////////////

#define MOTION_X_PIN A0
#define MOTION_Y_PIN A1
#define MOTION_Z_PIN A2

#define SPEAKER_PIN  9  // PWM preferred


////////////////////////////////////////////////////////////////////////////////

#include "gesture.h"


////////////////////////////////////////////////////////////////////////////////

#ifdef THREEAXIS
#include <MMA7361.h>

MMA7361 motionSensor(MOTION_X_PIN, MOTION_Y_PIN, MOTION_Z_PIN);
#endif

#ifdef NINEAXIS
#include <Wire.h>
#include <I2Cdev.h>
#include <ADXL345.h>
#include <ITG3200.h>
#include <HMC5883L.h>

ADXL345 accel;
ITG3200 gyro;
HMC5883L magnet;

const int magnetBufferLength = 100;
int16_t magnetBufferX[magnetBufferLength], magnetBufferY[magnetBufferLength], magnetBufferZ[magnetBufferLength];
int32_t magnetSumX, magnetSumY, magnetSumZ;
unsigned long lastMagnetRefTime;
#endif


////////////////////////////////////////////////////////////////////////////////

#include <Droidspeak.h>

Droidspeak droidspeak(SPEAKER_PIN);


////////////////////////////////////////////////////////////////////////////////

#include <OSCBundle.h>
#include <ExtendOSC.h>

ExtendOSC osc(EXO_HAND);

OSCBundle *bundleIn;

void error(const char *message) {
    osc.sendError(message);
}


////////////////////////////////////////////////////////////////////////////////

#include <Morse.h>

int morseStopTest() {
    // no need to abort
    return 0;
}

Morse morse(SPEAKER_PIN, morseStopTest, error);


////////////////////////////////////////////////////////////////////////////////

#ifdef GESTURE_MODE
const double lowerBound = 1.25;
const double upperBound = 1.75;
#else
#ifdef KEYBOARD_MODE
const double lowerBound = 1.25;
const double upperBound = 1.50;
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
unsigned long tmax;

// settable identifier which is included with each gestural and sensor output
char contextName[32];

void setup()  
{
#ifdef THREEAXIS
    // TODO: random seed using 9-axis sensor
    randomSeed(motionSensor.rawX() + motionSensor.rawY() + motionSensor.rawZ());
    
    // 1.5g constants, sampled 2014-06-21
    motionSensor.calibrateX(272, 794);
    motionSensor.calibrateY(332, 841);
    motionSensor.calibrateZ(175, 700);
#else
#ifdef NINEAXIS
    randomSeed(accel.getAccelerationX() - accel.getAccelerationY() + accel.getAccelerationZ());
#endif
#endif

    droidspeak.speakPowerUpPhrase();

    osc.beginSerial();

    droidspeak.speakSerialOpenPhrase();

    bundleIn = new OSCBundle();   

#ifdef NINEAXIS
    // adjust the power settings after you call this method if you want the accelerometer
    // to enter standby mode, or another less demanding mode of operation
    accel.initialize();
    if (!accel.testConnection()) {
        osc.sendError("ADXL345 connection failed");
    }
    
    gyro.initialize();
    if (!gyro.testConnection()) {
        osc.sendError("ITG3200 connection failed");
    }
    
    magnet.initialize();
    if (!magnet.testConnection()) {
        osc.sendError("HMC5883L connection failed");
    }
#endif

    state = STATE_ONE;
    
    strcpy(contextName, "default");
}


////////////////////////////////////////////////////////////////////////////////

void sendPingReply() {
    OSCMessage m(EXO_HAND_PING_REPLY);
    m.add((uint64_t) micros());
    
    osc.sendOSC(m);
}

#ifdef HEARTBEAT_MS
void sendHeartbeatMessage(unsigned long now) {
    OSCMessage m(EXO_HAND_HEARTBEAT);
    m.add((uint64_t) now);
    osc.sendOSC(m); 
}
#endif

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
    if (!osc.validArgs(m, 1)) return;

    m.getString(0, contextName, m.getDataLength(0) + 1);
}

/*
void handleMorseMessage(class OSCMessage &m) {
    if (!osc.validArgs(m, 1)) return;

    int length = m.getDataLength(0);
    char buffer[length+1];
    m.getString(0, buffer, length+1);

    morse.playMorseString(buffer);
}
*/

void handlePingMessage(class OSCMessage &m) {
    sendPingReply();
}

void handleOSCBundle(class OSCBundle &bundle) {
    if (bundle.hasError()) {
        osc.sendOSCBundleError(bundle);
    } else if (!(0
        || bundle.dispatch(EXO_HAND_AUDIO_TONE, handleAudioToneMessage)
        || bundle.dispatch(EXO_HAND_CONTEXT_SET, handleContextSetMessage)
        //|| bundle.dispatch(EXO_HAND_MORSE, handleMorseMessage)
        || bundle.dispatch(EXO_HAND_PING, handlePingMessage)
        )) {
        osc.sendError("no messages dispatched");
    }
}


////////////////////////////////////////////////////////////////////////////////

unsigned long lastHeartbeat = 0;

// the number of samples taken since the last output
unsigned long samples = 0;
unsigned long lastSampleOutput = 0;

void loop()
{
#ifdef OUTPUT_SAMPLING_RATE
    samples++;
#endif

#ifdef INPUT_ENABLED
    if (osc.receiveOSCBundle(*bundleIn)) {
        handleOSCBundle(*bundleIn);
        bundleIn->empty();
        delete bundleIn;
        bundleIn = new OSCBundle();
    }
#endif

    unsigned long now = micros();
    unsigned long nowMillis = millis();
    
#ifdef HEARTBEAT_MS
    if (nowMillis - lastHeartbeat > HEARTBEAT_MS) {
        sendHeartbeatMessage(nowMillis);
        lastHeartbeat = nowMillis;  
    }
#endif

    double ax, ay, az;
    double a;

#ifdef THREEAXIS
    ax = motionSensor.accelX();
    ay = motionSensor.accelY();
    az = motionSensor.accelZ();
#else
    int16_t ax2, ay2, az2;
    accel.getAcceleration(&ax2, &ay2, &az2);
    int16_t gx, gy, gz;
    gyro.getRotation(&gx, &gy, &gz);
    int16_t mx, my, mz;
    magnet.getHeading(&mx, &my, &mz);
    
    // approximate g values, per calibration with a specific sensor
    ax = ax2 / 230.0 - 0.05;
    ay = ay2 / 230.0;
    az = az2 / 230.0;
#endif

#ifdef OUTPUT_SENSOR_DATA
#ifdef SIMPLE_OUTPUT
    Serial.print(EXO_HAND_MOTION);
    Serial.print(','); Serial.print(contextName);
    Serial.print(','); Serial.print(now);
#else
    OSCMessage mout(EXO_HAND_MOTION);
    mout.add(contextName);
    mout.add((uint64_t) now);
    //mout.add(ax); mout.add(ay); mout.add(az);
#endif // ifdef SIMPLE_OUTPUT
#ifdef NINEAXIS
#ifdef SIMPLE_OUTPUT
    Serial.print(','); Serial.print(ax2); Serial.print(','); Serial.print(ay2); Serial.print(','); Serial.print(az2);
    Serial.print(','); Serial.print(gx); Serial.print(','); Serial.print(gy); Serial.print(','); Serial.print(gz);
    Serial.print(','); Serial.print(mx); Serial.print(','); Serial.print(my); Serial.print(','); Serial.print(mz);
#else
    mout.add((int32_t) ax2); mout.add((int32_t) ay2); mout.add((int32_t) az2);
    mout.add((int32_t) gx); mout.add((int32_t) gy); mout.add((int32_t) gz);
    mout.add((int32_t) mx); mout.add((int32_t) my); mout.add((int32_t) mz);
#endif // ifdef SIMPLE_OUTPUT
#endif // ifdef NINEAXIS
#ifdef SIMPLE_OUTPUT
    Serial.println("");
#else
    osc.sendOSC(mout);
#endif // ifdef SIMPLE_OUTPUT
#endif // ifdef OUTPUT_SENSOR_DATA
    
#ifdef OUTPUT_GESTURES
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

            // output the gesture if non-null
            if (gesture) {
                // play short audio cues associated with gestures
                // initiate the cue before dealing with serial communication, which involves a delay
                if (gestureToneLength > 0) {
                    /*
                    noTone(SPEAKER_PIN);
                    tone(SPEAKER_PIN, gestureTone, gestureToneLength);
                    //*/
                    //*
                    tone(SPEAKER_PIN, gestureTone);
                    delay(gestureToneLength);
                    noTone(SPEAKER_PIN);
                    //*/
                    
                    gestureToneLength = 0;
                    gestureToneVolume = 1.0;
                }
                
                // gesture event
                OSCMessage m(EXO_HAND_GESTURE);
                m.add(contextName);
                m.add((uint64_t) tmax);  // time of turning point
                m.add((uint64_t) now);  // time of recognition
                m.add(amax);
                m.add(ax_max);
                m.add(ay_max);
                m.add(az_max);
                m.add(gesture);
                osc.sendOSC(m);

#ifdef OUTPUT_SAMPLING_RATE
                unsigned long freshNow = millis();
                if (lastSampleOutput > 0 && freshNow > lastSampleOutput) {
                    int rate = (samples * 1000) / (freshNow - lastSampleOutput); 
                    osc.sendInfo("samping rate: %d Hz", rate);
                }
                
                lastSampleOutput = freshNow;
                samples = 0;
#endif // OUTPUT_SAMPLING_RATE
            }
        }
        break;
    }
#endif // OUTPUT_GESTURES
}
