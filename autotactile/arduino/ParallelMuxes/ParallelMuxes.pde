/*
 * Autotactile parallel-mux test program
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

const int amuxInputPin = pin15;
const int bmuxInputPin = pin6;

//const int amuxSelectPins[] = {pin16, pin17, pin18, pin19};
//const int bmuxSelectPins[] = {pin2, pin3, pin4, pin5};
const int firstAMuxSelectPin = pin16;
const int firstBMuxSelectPin = pin2;

const unsigned int INVERT_BMUX_SELECT = true;

int lineno = 0;

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

#define SAMPLING_DELAY 0

#define SENSOR_MULTIPLIER 16

unsigned char sampleData[BITS_PER_SAMPLE / 8]; 
unsigned char prevSampleData[BITS_PER_SAMPLE / 8];

#define PRETTY_MODE

// temporary ///////////////////////////


////////////////////////////////////////

unsigned char amuxSelectPinsState[SELECT_PINS];
unsigned char bmuxSelectPinsState[SELECT_PINS];

inline void setAMuxSelectPin(unsigned int j, unsigned int val)
{
  digitalWrite(firstAMuxSelectPin + j, val); 
  amuxSelectPinsState[j] = val;
}

inline void setBMuxSelectPin(unsigned int j, unsigned int val)
{
  digitalWrite(firstBMuxSelectPin + j, INVERT_BMUX_SELECT ? !val : val); 
  bmuxSelectPinsState[j] = val;
}

inline void setAMuxSelectPinNew(unsigned int j, unsigned int val)
{
  digitalWrite(firstAMuxSelectPin + j, val); 
}

inline void setBMuxSelectPinNew(unsigned int j, unsigned int val)
{
  digitalWrite(firstBMuxSelectPin + j, INVERT_BMUX_SELECT ? !val : val); 
}

//unsigned int count;

inline void sampleSensorParallel(unsigned int m)
{
  unsigned int x;
  unsigned int mi = m / 8;
  unsigned int mj = m % 8;
  
  for (unsigned int q = 0; q < SENSOR_MULTIPLIER; q++) {
    for (unsigned int k = 0; k < SELECT_PINS; k++)
    {
      setBMuxSelectPin(k, false);
    }
  
    unsigned int i = 0;
    // Note: stopping before m instead of going up to BITS_PER_SAMPLE takes
    // advantage of the symmetry of touch (and also avoids sampling a sensor
    // against itself).  This nearly doubles the sampling rate.
    while (i < m)
    {
      if (i > 0)
      {
        unsigned int j = 0;
        while (bmuxSelectPinsState[j])
        {
          setBMuxSelectPin(j, false);
          j++;
        } 
        setBMuxSelectPin(j, true);
      }
      
      x = !digitalRead(amuxInputPin);
      sampleData[mi] |= (x << mj);
  //    count++;
      i++;
      
      setBMuxSelectPin(0, true);
      x = !digitalRead(amuxInputPin);
      sampleData[mi] |= (x << mj);
  //    count++;
      i++;
    }
  }
}

int hasChanged;

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

void sampleParallel()
{
  //delay(1);
  
  for (unsigned int q = 0; q < SENSOR_MULTIPLIER; q++) {
    for (int k = 0; k < SELECT_PINS; k++)
    {
      setAMuxSelectPin(k, false);
    }
  
    int i = 0;
    //Serial.println("Sampling...");
    while (i < BITS_PER_SAMPLE)
    {
      /*
      Serial.print("\t\t");
      for (int k = 0; k < SELECT_PINS; k++)
      {
        Serial.print(amuxSelectPinsState[k]);
      }
      Serial.println("");*/
      
      if (i > 0)
      {
        int j = 0;
        while (amuxSelectPinsState[j])
        {
          setAMuxSelectPin(j, false);
          j++;
        } 
        setAMuxSelectPin(j, true);
      }
      
      sampleSensorParallel(i);
      i++;
      setAMuxSelectPin(0, true);
      sampleSensorParallel(i);
      i++;
    }
  }
}

/* Using a memo array is more efficient
void sampleParallelSimple()
{
  for (unsigned int q = 0; q < SENSOR_MULTIPLIER; q++) {
    for (unsigned int i = 0; i < BITS_PER_SAMPLE; i++)
    { 
      unsigned int c = i / 8;
      unsigned int offset = i % 8;
      
      // Set all A pins
      for (unsigned int k = 0; k < SELECT_PINS; k++)
      {
        setAMuxSelectPinNew(k, (i >> k) & 1);
      }
        
      for (unsigned int j = 0; j < BITS_PER_SAMPLE; j++)
      {
        // Set all B pins
        for (unsigned int k = 0; k < SELECT_PINS; k++)
        {
          setBMuxSelectPinNew(k, (j >> k) & 1);
        }
        
        sampleData[c] |= (!digitalRead(amuxInputPin) << offset);
      }
    }
  }
}
*/

void outputSampleData()
{
#ifdef PRETTY_MODE
  lineno++;

  Serial.print(lineno);
  Serial.print(") ");
  for (int i = 0; i < BITS_PER_SAMPLE; i++)
  {
    int b = prevSampleData[i / 8] & (1 << (i % 8));
    Serial.print(b ? 'o' : ' ');
  }
#else
  for (int l = 0; l < BITS_PER_SAMPLE / 8; l++)
  {
    Serial.print(prevSampleData[l]);      
  }
#endif

  Serial.println("");
}

void setup()
{
  // set up Serial library at 9600 bps
  Serial.begin(9600);
  
  pinMode(amuxInputPin, INPUT);
  pinMode(bmuxInputPin, INPUT);
  
  for (int j = 0; j < SELECT_PINS; j++)
  {
    pinMode(firstAMuxSelectPin + j, OUTPUT);
    pinMode(firstBMuxSelectPin + j, OUTPUT);
  }
  
  // Trick to force outputting of an artificial blank sensor map.
  prevSampleData[0] = 1;
}

#define TIMING_TEST

void loop()
{   
// Current results:
// around 230 samples / second (at 1 iteration over a pair of 16-pin muxes)
// less than 1/s (at 16^2 iterations)
#ifdef TIMING_TEST
  unsigned long iters = 100;
  unsigned long before, after;
  before = millis();
  for (unsigned long i = 0; i < iters; i++) {
    advanceSampleData();
    //sampleParallel(); 
    sampleParallel(); 
  }
  after = millis();
  Serial.print(1000 * (double) iters / (double) (after - before));
  Serial.println(" samples per second");
#else
  advanceSampleData();
  
  if (hasChanged)
  {
    outputSampleData();
  }

  //count = 0;
  //sampleParallel();
  sampleParallel();
  //Serial.println(count);
#endif
}
