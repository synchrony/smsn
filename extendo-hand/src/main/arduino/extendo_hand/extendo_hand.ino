/*
  Sends sensor data to Android
  (needs SensorGraph and Amarino app installed and running on Android)
*/
 
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


void setup()  
{
  // use the baud rate your bluetooth module is configured to 
  // not all baud rates are working well, i.e. ATMEGA168 works best with 57600
  Serial.begin(115200); 
}

void loop()
{
    int xraw = analogRead(pinX);
    int yraw = analogRead(pinY);
    int zraw = analogRead(pinZ);
    
    Serial.print(micros());
    Serial.print(","); Serial.print(xraw);
    Serial.print(","); Serial.print(yraw);
    Serial.print(","); Serial.print(zraw);
    Serial.println("");
    
    //delay(500);
  
  
  meetAndroid.receive(); // you need to keep this in your loop() to receive events
  
  // read input pin and send result to Android
  meetAndroid.send(pinX);
  
  // add a little delay otherwise the phone is pretty busy
  delay(100);
}

