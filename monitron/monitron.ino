// TODO: add program code for the PIR motion sensor

#include <Wire.h>

////////////////////////////////////////

#define AUDIO_PIN         A0
#define VIBRO_PIN         A1
#define PHOTO_PIN         A2
#define DUST_PIN          A3
// analog pin 4 is reserved for I2C
// analog pin 5 is reserved for I2C
// A6
// A7

// digital pin 0 is reserved for RX
// digital pin 1 is reserved for TX
#define SPEAKER_PIN       2
#define DUST_LED_PIN      3
// 4 -- light sensor LED
// 5
// 6
#define DHT22_PIN         7
// 8 -- PIR motion sensor
#define RGB_LED_GREEN_PIN 9
#define RGB_LED_BLUE_PIN  10
#define RGB_LED_RED_PIN   11
// 12
#define LED_PIN           13

////////////////////////////////////////

#define AUDIO_OSC_PREFIX         "/om/sensor/phone"
#define BMP085_OSC_PREFIX        "/om/sensor/BMP085"
#define DHT22_OSC_PREFIX         "/om/sensor/dht22"
#define DUST_OSC_PREFIX          "/om/sensor/dust"
#define PHOTO_OSC_PREFIX         "/om/sensor/photo"
#define TIMER_OSC_PREFIX         "/om/timer"
#define VIBRO_OSC_PREFIX         "/om/sensor/piezo"

// Each sampling cycle must take at least this long.
// If a cycle is finished sooner, we will wait before starting the next cycle.
#define CYCLE_MILLIS_MIN  3000

// A sampling cycle may take at most this long.
// If the sample runs over, an error message will be generated
#define CYCLE_MILLIS_MAX  5000

////////////////////////////////////////

#include <AnalogSampler.h>

AnalogSampler audioSampler(AUDIO_PIN);
AnalogSampler vibroSampler(VIBRO_PIN);
AnalogSampler photoSampler(PHOTO_PIN);

////////////////////////////////////////

#include <DHT22.h>

DHT22 dht22(DHT22_PIN);

////////////////////////////////////////

#include <BMP085.h>

BMP085 bmp085;

////////////////////////////////////////

#include "om_droidspeak.h"
#include "om_dust.h"
#include "om_timer.h"
#include "om_rgb_led.h"
#include "om_vibration.h"

////////////////////////////////////////

void setup() {
    pinMode(SPEAKER_PIN, OUTPUT);    
    pinMode(DUST_LED_PIN, OUTPUT);
    pinMode(LED_PIN, OUTPUT);
    
    rgb_led_setup();
    
    // TODO: why is this white immediately overridden by red?
    pushColor(WHITE);

    speakPowerUpPhrase();
    
    randomSeed(analogRead(PHOTO_PIN));
 
    Serial.begin(9600);
    bmp085.setup();
        
    //lastCycle_highBits = 0;
    //lastCycle_lowBits = millis();
    
    speakSetupCompletedPhrase(); 
    
    popColor();
    pushColor(GREEN);
}

void loop()
{        
    startCycle();
    digitalWrite(LED_PIN, HIGH);
    
    //samplePhotoresistor();
    sampleAnalog(100000);
    sampleDHT22();
    sampleBMP085();
    sampleDustSensor();
    
    //rateTest();
    
    digitalWrite(LED_PIN, LOW);
    endCycle();
}

void beginSample()
{
    pushColor(BLUE);
}

void endSample()
{
    popColor();
}

void beginOSCWrite()
{
    pushColor(WHITE);
    tick();
}

void endOSCWrite()
{
    popColor();
}

void sampleAnalog(unsigned long iterations)
{
    int len = 3;
    AnalogSampler samplers[] = {audioSampler, vibroSampler, photoSampler};
    char* prefixes[] = {AUDIO_OSC_PREFIX, VIBRO_OSC_PREFIX, PHOTO_OSC_PREFIX};
    
    beginSample();
    for (unsigned long i = 0; i < iterations; i++)
    {
        for (int j = 0; j < len; j++)
        {
            samplers[j].sample();
        }
    }
    endSample();
    
    for (int j = 0; j < len; j++)
    {
        AnalogSampler s = samplers[j];
        
        beginOSCWrite();
        Serial.print(prefixes[j]);
        Serial.print("/data "); 
        Serial.print(s.getMinValue());
        Serial.print(" ");
        Serial.print(s.getMaxValue()); 
        Serial.print(" ");
        Serial.print(s.getMean());
        Serial.print(" ");
        Serial.println(s.getVariance());
        samplers[j].reset();
        endOSCWrite();
    }
}

void samplePhotoresistor()
{
    beginSample();
    int v = analogRead(PHOTO_PIN); 
    endSample();
    
    double rel = v / 1024.0; 
    
    beginOSCWrite();
    Serial.print(PHOTO_OSC_PREFIX);
    Serial.print("/data "); 
    Serial.println(rel);     
    endOSCWrite();
}

// output temperature (C) and humidity (%)
// needs 2s between readings
// however, the actual readData operation takes only 0.03ms on Arduino Nano
void sampleDHT22()
{
  DHT22_ERROR_t errorCode;

  beginSample();
  errorCode = dht22.readData();
  endSample();
  
  beginOSCWrite();
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
 
    endOSCWrite(); 
}

// output temperature (0.1 deg C) and pressure (Pa)
void sampleBMP085()
{
    // this has been found to take around 12ms (on Arduino Nano)
    beginSample();
    bmp085.sample();
    endSample();
    
    beginOSCWrite();
    Serial.print(BMP085_OSC_PREFIX);
    Serial.print("/data ");
    Serial.print(bmp085.getLastTemperature(), DEC);
    Serial.print(" ");
    Serial.print(bmp085.getLastPressure(), DEC);
    Serial.println();
    endOSCWrite();
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
  
  //beginOSCWrite();
  Serial.print(n);
  Serial.print(" iterations in ");
  Serial.print(duration);
  Serial.println(" ms");
  //endOSCWrite();
}
//*/
