/*
 * Extend-o-Hand sampling code, copyright 2013-2014 by Joshua Shinavier
 * 
 * See: https://github.com/joshsh/extendo
 */

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
    Serial.begin(115200);
#if ARDUINO >= 100
    while(!Serial) ; // Leonardo "feature"
#endif
}


////////////////////////////////////////////////////////////////////////////////

void loop()
{
    double ax, ay, az;

    ax = 2 * (analogRead(pinX) - xmid) / xrange;
    ay = 2 * (analogRead(pinY) - ymid) / yrange;
    az = 2 * (analogRead(pinZ) - zmid) / zrange;

    // comma-separated format for the measurement, for ease of importing to R and similar tools
    Serial.print((int32_t) micros()); Serial.print(",");
    Serial.print(ax); Serial.print(",");
    Serial.print(ay); Serial.print(",");
    Serial.print(az); Serial.print("\n");
}


