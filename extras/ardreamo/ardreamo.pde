/*
 * Arduino sketch for DIY actigraph and NovaDreamer-like sleep interface
 *
 * 1 bit per second for 12 hours: 5.3 kB (5,400 bytes). Uncompressed, this
 * exceeds the Arduino's RAM.
 *
 * Arduino_Available_RAM_Test reports 1278 bytes of available RAM.  For a
 * 12-hour run, that allows 14 bits per minute (without compression), or one
 * bit every 4 seconds or so.
 * 
 * For more bits per second, a simple compression scheme may work:
 * 1) each byte represents at least 7 (not 8) samples
 * 2) if the first bit of the byte is 0, the next seven are samples
 * 3) if the first bit is 1, the next seven are the number of consecutive
 *    7-tuples of zero-valued samples, up to 128 (using an offset by 1 from the
 *    actual number).
 * Ignoring the range of the encoded number (which is pretty high, enough for
 * almost 15 minutes at one sample per second), this scheme is more
 * space-efficient than the simple bit string so long as one eighth or more of
 * all 7-samples are zeroes which follow another zero.  For an actigraph
 * record of a sleeping person, this will almost certainly be the case.
 */

const int pin0 = 0;    // RX
const int pin1 = 1;    // TX
const int pin2 = 2;    // digital 
const int pin3 = 3;    // digital (PWM)
const int pin4 = 4;    // digital
const int pin5 = 5;    // digital (PWM)
const int pin6 = 6;    // digital (PWM)
const int pin7 = 7;    // digital
const int pin8 = 8;    // digital (PWM)
const int pin9 = 9;    // digital (PWM)
const int pin10= 10;   // digital (PWM)
const int pin11 = 11;  // digital
const int pin12 = 12;  // digital
const int pin13 = 13;  // digital
const int pin14 = 14;  // analog in 0
const int pin15 = 15;  // analog in 1
const int pin16 = 16;  // analog in 2
const int pin17 = 17;  // analog in 3
const int pin18 = 18;  // analog in 4
const int pin19 = 19;  // analog in 5

const int analogInputPin0 = 0;
const int analogInputPin1 = 1;
const int analogInputPin2 = 2;
const int analogInputPin3 = 3;
const int analogInputPin4 = 4;
const int analogInputPin5 = 5;

// above 1800 or so causes strange behavior and non-behavior
#define DATA_BYTES 1000

unsigned char data[DATA_BYTES];

void setup()
{
  Serial.begin(9600);
  
  for (int i = 0; i < DATA_BYTES; i++) {
    data[i] = i;
  }
}

int loopno = 0;

void loop()
{  
  Serial.print(loopno);
  Serial.print(")\t");
  Serial.println(data[loopno++]);
}
