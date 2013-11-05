

const int pinX = A0;
const int pinY = A1;
const int pinZ = A2;


////////////////////////////////////////
// 1.5g constants

//*
const int xmin = 200;
const int xmax = 565;
const int ymin = 240;
const int ymax = 610;
const int zmin = 110;
const int zmax = 500;
//*/


////////////////////////////////////////
// 6g constants (independent of 1.5g constants; must be separately sampled)

/*
const int xmin = 320;
const int xmax = 415;
const int ymin = 340;
const int ymax = 430;
const int zmin = 290;
const int zmax = 395;
//*/


// the "steadiness of hand" with which the sensor was moved to gather the max/min values.
// If precision = 0.8, then 1g was overestimated by 20% due to a shaky hand.
// Use 1.0 if static samples were taken.
const double steadiness = 1.0;

const double xrange = (xmax - xmin) * steadiness;
const double yrange = (ymax - ymin) * steadiness;
const double zrange = (zmax - zmin) * steadiness;

const double xmid = (xmin + xmax) / 2.0;
const double ymid = (ymin + ymax) / 2.0;
const double zmid = (zmin + zmax) / 2.0;

void setup() {
    //pinMode(pinX, INPUT);
    //pinMode(pinY, INPUT);
    //pinMode(pinZ, INPUT);
    
    Serial.begin(115200);
}

int xminn = 1000, yminn = 1000, zminn = 1000;
int xmaxn = 0, ymaxn = 0, zmaxn = 0;

void loop() {
    /* test calibrated sensor (comparing acceleration output against observable gravity)
    double ax, ay, az;
    double a;
    
    ax = 2 * (analogRead(pinX) - xmid) / xrange;
    ay = 2 * (analogRead(pinY) - ymid) / yrange;
    az = 2 * (analogRead(pinZ) - zmid) / zrange;
    
    a = sqrt(ax*ax + ay*ay + az*az);
    
    Serial.print(micros());
    Serial.print(","); Serial.print(a);
    Serial.print(": ("); Serial.print(ax);
    Serial.print(","); Serial.print(ay);
    Serial.print(","); Serial.print(az);
    Serial.println(")");
    
    delay(300);
    //*/
    
    /* simply output raw values.  Manually identify min/max values in major axes.  I have found this most effective
    
    int xraw = analogRead(pinX);
    int yraw = analogRead(pinY);
    int zraw = analogRead(pinZ);
    
    Serial.print(micros());
    Serial.print(","); Serial.print(xraw);
    Serial.print(","); Serial.print(yraw);
    Serial.print(","); Serial.print(zraw);
    Serial.println("");
    
    //delay(500);
    //*/
    
    /* calibration: find extreme values.  Note: I have not found this method very effective
    
    int xraw = analogRead(pinX);
    int yraw = analogRead(pinY);
    int zraw = analogRead(pinZ);
    
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
        
        ax = 2 * (analogRead(pinX) - xmid) / xrange;
        ay = 2 * (analogRead(pinY) - ymid) / yrange;
        az = 2 * (analogRead(pinZ) - zmid) / zrange;
        
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
    
    ax = 2 * (analogRead(pinX) - xmid) / xrange;
    ay = 2 * (analogRead(pinY) - ymid) / yrange;
    az = 2 * (analogRead(pinZ) - zmid) / zrange;
    
    Serial.print(micros());
    Serial.print(","); Serial.print(ax);
    Serial.print(","); Serial.print(ay);
    Serial.print(","); Serial.print(az);
    Serial.println("");
    
    //*/
}
