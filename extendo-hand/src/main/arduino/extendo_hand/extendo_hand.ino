/*
 * Extend-o-Hand firmware, copyright 2013 by Joshua Shinavier
 * See: https://github.com/joshsh/extendo
 *
 */

const int pinX = A0;
const int pinY = A1;
const int pinZ = A2;


////////////////////////////////////////////////////////////////////////////////

// if defined, use more straightforward serial output
//#define DEBUG

// send and receive messages using Bluetooth/Amarino as opposed to plain serial
#define USE_BLUETOOTH


////////////////////////////////////////////////////////////////////////////////

//#include <MeetAndroid.h>

//MeetAndroid meetAndroid;

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

char print_str[100];

void setup()  
{
    // BlueSMiRF Silver is compatible with any baud rate from 2400-115200
    // Note: the Amarino receiver appears to be compatible with a variety baud rates, as well
    //Serial.begin(115200);

    // OSCuino: begin SLIPSerial just like Serial
    SLIPSerial.begin(115200);   // set this as high as you can reliably run on your platform
#if ARDUINO >= 100
    while(!Serial) ; // Leonardo "feature"
#endif
     
  state = STATE_ONE;
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
#elif defined(DEBUG)
    // put OSC messages on separate lines so as to make them more readable
    SLIPSerial.println("");
#endif  
}

void error(char *message) {
    OSCMessage m("/exo/hand/error");
    m.add(message);

    sendOSC(m);
}


////////////////////////////////////////////////////////////////////////////////

void loop()
{
    //* gesture mode
    
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
          OSCMessage m("/exo/hand/raw");
          m.add((int32_t) micros());
          m.add((int) (amax * 100));
          m.add((int) (ax_max * 100));
          m.add((int) (ay_max * 100));
          m.add((int) (az_max * 100));
          sendOSC(m);       
        }
        break;
    }
}

