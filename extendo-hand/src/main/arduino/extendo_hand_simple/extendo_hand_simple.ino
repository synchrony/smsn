/*
 * Extend-o-Hand firmware, copyright 2013-2014 by Joshua Shinavier
 *
 * See: https://github.com/joshsh/extendo
 */


//#define THREEAXIS
#define NINEAXIS


////////////////////////////////////////////////////////////////////////////////

#define MOTION_X_PIN A0
#define MOTION_Y_PIN A1
#define MOTION_Z_PIN A2

#define SPEAKER_PIN  9  // PWM preferred


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
#endif // ifdef NINEAXIS


////////////////////////////////////////////////////////////////////////////////

#include <Droidspeak.h>

Droidspeak droidspeak(SPEAKER_PIN);


////////////////////////////////////////////////////////////////////////////////

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

    Serial.begin(115200);
#if ARDUINO >= 100
    while(!Serial); // for Arduino Leonardo
#endif

    droidspeak.speakSerialOpenPhrase();

#ifdef NINEAXIS
    // adjust the power settings after you call this method if you want the accelerometer
    // to enter standby mode, or another less demanding mode of operation
    accel.initialize();
    if (!accel.testConnection()) {
        osc.sendError("ADXL345 connection failed");
    }

    gyro.initialize();
    if (!accel.testConnection()) {
        osc.sendError("ITG3200 connection failed");
    }

    magnet.initialize();
    if (!accel.testConnection()) {
        osc.sendError("HMC5883L connection failed");
    }
#endif
}


////////////////////////////////////////////////////////////////////////////////

void loop()
{
    unsigned long now = micros();
    unsigned long nowMillis = millis();

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

    Serial.print(EXO_HAND_MOTION);
    Serial.print(','); Serial.print(contextName);
    Serial.print(','); Serial.print(now);
#ifdef NINEAXIS
    Serial.print(','); Serial.print(ax2); Serial.print(','); Serial.print(ay2); Serial.print(','); Serial.print(az2);
    Serial.print(','); Serial.print(gx); Serial.print(','); Serial.print(gy); Serial.print(','); Serial.print(gz);
    Serial.print(','); Serial.print(mx); Serial.print(','); Serial.print(my); Serial.print(','); Serial.print(mz);
#endif // ifdef NINEAXIS
    Serial.println("");
}
