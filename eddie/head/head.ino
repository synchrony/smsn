#include <Wire.h>

#define TIMER_OSC_PREFIX         "/eddie/head/timer"
#define PHOTO_OSC_PREFIX         "/eddie/head/sensor/photo"
#define DHT22_OSC_PREFIX         "/eddie/head/sensor/dht22"
#define BMP085_OSC_PREFIX        "/eddie/head/sensor/BMP085"

// Each sampling cycle must take at least this long.
// If a cycle is finished sooner, we will wait before starting the next cycle.
#define CYCLE_MILLIS_MIN  3000

// A sampling cycle may take at most this long.
// If the sample runs over, an error message will be generated
#define CYCLE_MILLIS_MAX  5000

////////////////////////////////////////

#include <DHT22.h>

#define DHT22_PIN 7

// Setup a DHT22 instance
DHT22 dht22(DHT22_PIN);

////////////////////////////////////////

#include <BMP085.h>

BMP085 bmp085;

////////////////////////////////////////

#define PHOTO_PIN A2

////////////////////////////////////////

#include "eh-timer.h"

////////////////////////////////////////


void setup() {
    Serial.begin(9600);
    bmp085.setup();
    
    //lastCycle_highBits = 0;
    //lastCycle_lowBits = millis();
}

void loop()
{    
    startCycle();
    
    samplePhotoresistor();
    sampleDHT22();
    sampleBMP085();
 
    //rateTest();
    
    endCycle();
}

void samplePhotoresistor()
{
    int v = analogRead(PHOTO_PIN); 
    double rel = v / 1024.0; 
   
    Serial.print(PHOTO_OSC_PREFIX);
    Serial.print("/data "); 
    Serial.println(rel);     
}

// output temperature (C) and humidity (%)
// needs 2s between readings
// however, the actual readData operation takes only 0.03ms on Arduino Nano
void sampleDHT22()
{
  DHT22_ERROR_t errorCode;

  //delay(2000);
  errorCode = dht22.readData();
  
  Serial.print(DHT22_OSC_PREFIX);
  
  switch(errorCode)
  {
    case DHT_ERROR_NONE:
      Serial.print("/data ");
      Serial.print(dht22.getTemperatureC());
      Serial.print(" ");
      Serial.print(dht22.getHumidity());
      Serial.println();
      break;
    case DHT_ERROR_CHECKSUM:
      Serial.println("/error checksum-error");
      break;
    case DHT_BUS_HUNG:
      Serial.println("/error bus-hung");
      break;
    case DHT_ERROR_NOT_PRESENT:
      Serial.println("/error not-present");
      break;
    case DHT_ERROR_ACK_TOO_LONG:
      Serial.println("/error ack-timeout");
      break;
    case DHT_ERROR_SYNC_TIMEOUT:
      Serial.println("/error sync-timeout");
      break;
    case DHT_ERROR_DATA_TIMEOUT:
      Serial.println("/error data-timeout");
      break;
    case DHT_ERROR_TOOQUICK:
      Serial.println("/error polled-too-quick");
      break;
  }  
}

// output temperature (0.1 deg C) and pressure (Pa)
void sampleBMP085()
{
    // this has been found to take around 12ms (on Arduino Nano)
    bmp085.sample();

    Serial.print(BMP085_OSC_PREFIX);
    Serial.print("/data ");
    Serial.print(bmp085.getLastTemperature(), DEC);
    Serial.print(" ");
    Serial.print(bmp085.getLastPressure(), DEC);
    Serial.println();
}

void rateTest()
{
  long before = millis();
  long unsigned int n = 100000;
  for (long unsigned int i = 0; i < n; i++)
  {
      dht22.readData();
  }
  long duration = millis() - before;
  Serial.print(n);
  Serial.print(" iterations in ");
  Serial.print(duration);
  Serial.println(" ms");
}
//*/
