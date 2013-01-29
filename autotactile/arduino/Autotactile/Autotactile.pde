/*
 * Autotactile test program
 */

#include <stdlib.h>

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

const int sensorPin = pin18;

//const int muxSelectPins1[] = {pin16, pin17, pin18, pin19};
const int firstMuxSelectPin = pin14;
//const int muxSelectPins1[] = {pin2, pin3, pin4, pin5};

const unsigned int REVERSE = false;
//const unsigned int REVERSE = true;

#define PRETTY_MODE

////////////////////////////////////////

#define MUX_LEVELS 1
#if MUX_LEVELS == 1
  #define SELECT_PINS 4
  #define BITS_PER_SAMPLE 16
#elif MUX_LEVELS == 2
  #define SELECT_PINS 8
  #define BITS_PER_SAMPLE 256
#elif MUX_LEVELS == 3
  #define SELECT_PINS 12
  #define BITS_PER_SAMPLE 4096
#endif

#define SAMPLES_PER_CYCLE 1
#define SENSOR_MULTIPLIER 16 // 16 MUXes for 256 sensors
//#define SENSOR_MULTIPLIER 64 // 64 MUXes for 1024 sensors

unsigned char sampleData[BITS_PER_SAMPLE / 8]; 
unsigned char prevSampleData[BITS_PER_SAMPLE / 8];

////////////////////////////////////////

inline void setMuxSelectPin(unsigned int j, unsigned int val)
{
  digitalWrite(firstMuxSelectPin + j, REVERSE ? !val : val); 
}

inline unsigned int sampleSensor()
{
  return !digitalRead(sensorPin);
}

void sampleSensors()
{
  for (unsigned int s = 0; s < SAMPLES_PER_CYCLE * SENSOR_MULTIPLIER; s++)
  {
    for (unsigned int k = 0; k < SELECT_PINS; k++)
    {
      setMuxSelectPin(k, false);
    }
    
    for (unsigned int m = 0; m < BITS_PER_SAMPLE / 8; m++)
    {
      unsigned char c = 0;

      // 0
      if (m > 0)
      {
        setMuxSelectPin(0, false);
        setMuxSelectPin(1, false);
        setMuxSelectPin(2, false);
        unsigned int j = 3;
        unsigned int i = (m - 1) * 8;
        while ((i >> j) & 1)
        {
          setMuxSelectPin(j, false);
          j++;
        }
        setMuxSelectPin(j, true);        
      }
      c |= sampleSensor();
      
      // 1
      setMuxSelectPin(0, true);
      c |= sampleSensor() << 1;
      
      // 2      
      setMuxSelectPin(0, false);
      setMuxSelectPin(1, true);
      c |= sampleSensor() << 2;
      
      setMuxSelectPin(0, true);
      c |= sampleSensor() << 3;
      
      setMuxSelectPin(0, false);
      setMuxSelectPin(1, false);
      setMuxSelectPin(2, true);
      c |= sampleSensor() << 4;
      
      setMuxSelectPin(0, true);
      c |= sampleSensor() << 5;
      
      setMuxSelectPin(0, false);
      setMuxSelectPin(1, true);
      c |= sampleSensor() << 6;
      
      setMuxSelectPin(0, true);
      c |= sampleSensor() << 7;
      
      sampleData[m] = c;
    }  
  }
}

////////////////////////////////////////

int lineno = 0;

boolean hasChanged;

void advanceSampleData()
{
  hasChanged = false;
  
  unsigned char *cur = sampleData;
  unsigned char *prev = prevSampleData;
  
  while (cur < sampleData + BITS_PER_SAMPLE / 8)
  {
    if (*cur != *prev)
    {
      hasChanged = true;
    }
    
    *prev = *cur;
    *cur = 0;
    
    cur++;
    prev++;
  }
}

void outputSampleData(unsigned long count)
{
#ifdef PRETTY_MODE
/*
  lineno++;

  Serial.print(lineno);
  Serial.print(") ");*/
  Serial.print("data ");
  
  for (unsigned int i = 0; i < BITS_PER_SAMPLE; i++)
  {
    unsigned int b = prevSampleData[i / 8] & (1 << (i % 8));
    Serial.print(b ? 'o' : '.');
  }
#else
  for (int l = 0; l < BITS_PER_SAMPLE / 8; l++)
  {
    Serial.print(prevSampleData[l]);      
  }
#endif

  Serial.print(" ");
  Serial.println(count);
  //Serial.println("");
}

////////////////////////////////////////

//#define TIMING_TEST

void setup()
{
  Serial.begin(9600);
  pinMode(sensorPin, INPUT);
  
  for (unsigned int j = 0; j < SELECT_PINS; j++)
  {
    pinMode(firstMuxSelectPin + j, OUTPUT);
  }
  
  // Trick to force outputting of an artificial blank sensor map.
  prevSampleData[0] = 1;
}

unsigned long count = 0;

void loop()
{  
// Current results: around 3500 Hz (at 1 iteration over a single
// 16-pin mux)
#ifdef TIMING_TEST
  unsigned int iters = 100;
  unsigned long before, after;
  before = millis();
  for (unsigned int i = 0; i < iters; i++) {
    advanceSampleData();
    sampleSensors();  
  }
  after = millis();
  Serial.print((1000 * (double) iters) / (double) (after - before));
  Serial.println(" samples per second");
#else
  advanceSampleData();
  
  if (hasChanged)
  {
    outputSampleData(count);
  }

  sampleSensors();
#endif

  count++;
}
