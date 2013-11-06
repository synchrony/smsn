
#include <MeetAndroid.h>

MeetAndroid meetAndroid;


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


const double lowerBound = 1.25;
const double upperBound = 1.75;
const int STATE_ONE = 1;
const int STATE_TWO = 2;
const int STATE_THREE = 3;
const int STATE_FOUR = 4;

int state;
double amax;
double ax_max, ay_max, az_max;

void setup()  
{
  // use the baud rate your bluetooth module is configured to 
  // not all baud rates work well, i.e. ATMEGA168 works best with 57600
  Serial.begin(115200); 
  
  state = STATE_ONE;
}

void loop()
{
    double ax, ay, az;
    double a;
    
    ax = 2 * (analogRead(pinX) - xmid) / xrange;
    ay = 2 * (analogRead(pinY) - ymid) / yrange;
    az = 2 * (analogRead(pinZ) - zmid) / zrange;
    
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
          
          // gesture event
          Serial.print(micros());
          Serial.print(" "); Serial.print(amax);
          Serial.print(" "); Serial.print(ax_max);
          Serial.print(" "); Serial.print(ay_max);
          Serial.print(" "); Serial.print(az_max);
          Serial.print("\r");          
        }
        break;
    }    
  
    /*
    int xraw = analogRead(pinX);
    int yraw = analogRead(pinY);
    int zraw = analogRead(pinZ);
    
    Serial.print(micros());
    Serial.print(","); Serial.print(xraw);
    Serial.print(","); Serial.print(yraw);
    Serial.print(","); Serial.print(zraw);
    Serial.println("");
  
    meetAndroid.receive(); // you need to keep this in your loop() to receive events
  
    meetAndroid.send(xraw);
  
    // add a little delay otherwise the phone is pretty busy
    delay(100);
    //*/
}

