/*
 * Extend-o-Hand calibration and sampling, copyright 2013-2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/smsn
 */

#include <MMA7361.h>

MMA7361 motionSensor(A0, A1, A2);


void setup() {

    // 1.5g constants, sampled 2014-06-21
    motionSensor.calibrateX(272, 794);
    motionSensor.calibrateY(332, 841);
    motionSensor.calibrateZ(175, 700);

    /* previous 1.5g constants
    motionSensor.calibrateX(200, 565);
    motionSensor.calibrateY(240, 610);
    motionSensor.calibrateZ(110, 500);
    //*/

    /* 6g constants (independent of 1.5g constants; must be separately sampled)
    motionSensor.calibrateX(320, 415);
    motionSensor.calibrateY(340, 430);
    motionSensor.calibrateZ(290, 395);
    //*/

    Serial.begin(115200);

#if ARDUINO >= 100
    while(!Serial) ;
#endif
}

int xminn = 1000, yminn = 1000, zminn = 1000;
int xmaxn = 0, ymaxn = 0, zmaxn = 0;

void loop() {
    /* test calibrated sensor (comparing acceleration output against observable gravity)
    double ax, ay, az;
    double a;
    
    ax = motionSensor.accelX();
    ay = motionSensor.accelY();
    az = motionSensor.accelZ();
    
    a = sqrt(ax*ax + ay*ay + az*az);
    
    Serial.print(micros());
    Serial.print(","); Serial.print(a);
    Serial.print(": ("); Serial.print(ax);
    Serial.print(","); Serial.print(ay);
    Serial.print(","); Serial.print(az);
    Serial.println(")");
    
    delay(300);
    //*/
    
    /* calibration: output raw values.  Manually identify min/max values in major axes
    
    int xraw = motionSensor.rawX();
    int yraw = motionSensor.rawY();
    int zraw = motionSensor.rawZ();
    
    Serial.print(micros());
    Serial.print(","); Serial.print(xraw);
    Serial.print(","); Serial.print(yraw);
    Serial.print(","); Serial.print(zraw);
    Serial.println("");
    
    delay(200);
    //*/
    
    /* calibration: find extreme values.  Note: values may be misleading due to small jerks and twitches of the hand
    
    int xraw = motionSensor.rawX();
    int yraw = motionSensor.rawY();
    int zraw = motionSensor.rawZ();
    
    if (xraw < xminn) xminn = xraw;
    if (xraw > xmaxn) xmaxn = xraw;
    if (yraw < yminn) yminn = yraw;
    if (yraw > ymaxn) ymaxn = yraw;
    if (zraw < zminn) zminn = zraw;
    if (zraw > zmaxn) zmaxn = zraw;

    Serial.print("x: ["); Serial.print(xminn); Serial.print(", "); Serial.print(xmaxn);
    Serial.print("] y: ["); Serial.print(yminn); Serial.print(", "); Serial.print(ymaxn);
    Serial.print("] z: ["); Serial.print(zminn); Serial.print(", "); Serial.print(zmaxn); Serial.println("]");
    //*/

    /* identify extremes of acceleration in gesture
    
    double ax, ay, az;
    double ax_max, ay_max, az_max;
    double a;
    double amax = 0;
    unsigned long start, finish;
    int nsamples = 1000;
    
    start = micros();
    for (int i = 0; i < nsamples; i++) {
        
        ax = motionSensor.accelX();
        ay = motionSensor.accelY();
        az = motionSensor.accelZ();
        
        a = sqrt(ax*ax + ay*ay + az*az);
        
        if (a > amax) {
          amax = a;
          ax_max = ax;
          ay_max = ay;
          az_max = az;
        }
    }
    finish = micros();
    
    if (amax > 1.5) {
        Serial.print("max a = "); Serial.print(amax); Serial.print(": (");
        Serial.print(ax_max); Serial.print(", "); Serial.print(ay_max); Serial.print(", "); Serial.print(az_max); Serial.print(")");
        Serial.print(" -- "); Serial.print(1000000.0 * nsamples / (finish - start)); Serial.println(" /s");
    }
    //*/

    //* stream data over the serial port as quickly as possible
    
    double ax, ay, az;
    
    ax = motionSensor.accelX();
    ay = motionSensor.accelY();
    az = motionSensor.accelZ();

    // use a commas rather than tabs, as they are easier to work with on the receiving end
    Serial.print((int32_t) micros()); Serial.print(",");
    Serial.print(ax); Serial.print(",");
    Serial.print(ay); Serial.print(",");
    Serial.print(az); Serial.print("\n");
    //*/
}
