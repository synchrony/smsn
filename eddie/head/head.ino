#include <Wire.h>

////////////////////////////////////////

#define AUDIO_PIN A0
#define VIBRO_PIN A1
#define PHOTO_PIN A2
#define DUST_PIN  A3
// analog pin 4 is reserved for I2C
// analog pin 5 is reserved for I2C
// A6
// A7

// digital pin 0 is reserved for RX
// digital pin 1 is reserved for TX
#define SPEAKER_PIN  2
#define DUST_LED_PIN 3
// 4 -- light sensor LED
// 5
// 6
#define DHT22_PIN    7
// 7 -- PIR motion sensor
// 9, 10, 11 -- RGB LED
// 12
// 13 - LED

////////////////////////////////////////

#define AUDIO_OSC_PREFIX         "/eddie/head/sensor/phone"
#define BMP085_OSC_PREFIX        "/eddie/head/sensor/BMP085"
#define DHT22_OSC_PREFIX         "/eddie/head/sensor/dht22"
#define DUST_OSC_PREFIX          "/eddie/head/sensor/dust"
#define PHOTO_OSC_PREFIX         "/eddie/head/sensor/photo"
#define TIMER_OSC_PREFIX         "/eddie/head/timer"
#define VIBRO_OSC_PREFIX         "/eddie/head/sensor/piezo"

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

// DHT22 instance
DHT22 dht22(DHT22_PIN);

////////////////////////////////////////

#include <BMP085.h>

// BMP085 instance
BMP085 bmp085;

////////////////////////////////////////

#include "eh_droidspeak.h"
#include "eh_dust.h"
#include "eh_timer.h"
#include "eh_rgb_led.h"
#include "eh_vibration.h"

////////////////////////////////////////

void setup() {
    pinMode(SPEAKER_PIN, OUTPUT);
    pinMode(DUST_LED_PIN, OUTPUT);

    randomSeed(analogRead(PHOTO_PIN));
 
    Serial.begin(9600);
    bmp085.setup();
    
    rgb_led_setup();
    
    //lastCycle_highBits = 0;
    //lastCycle_lowBits = millis();
    
    speakStartupPhrase(); 
}

void loop()
{    
    writeColor(GREEN);
    
    startCycle();
    
    //samplePhotoresistor();
    sampleAnalog(10000);
    sampleDHT22();
    sampleBMP085();
    sampleDustSensor();
    
    //rateTest();
    
    endCycle();
    tick();
}

void testTone(unsigned long duration, int frequency)
{
    unsigned long hwl = 500000 / frequency;
    unsigned long dur = 1000 * duration;
    unsigned long start = micros();
    
    // Just bail if the timer is about to roll over
    if (start + dur < start) {
        return;
    }
    
    unsigned long now;
    do
    {
        now = micros() - start;

        if ((now / hwl) % 2)
            digitalWrite(SPEAKER_PIN, LOW);
        else
            digitalWrite(SPEAKER_PIN, HIGH);  
    } while (now < dur);
}

void testGlide(unsigned long duration, int frequency1, int frequency2)
{
    //double freq1 = frequency1 / 1000000.0;
    //double freq2 = frequency2 / 1000000.0;
  
    long hwl1 = 500000 / frequency1;
    long hwl2 = 500000 / frequency2;
    Serial.print("hwl1: ");
    Serial.println(hwl1);
    unsigned long dur = 1000 * duration;
    unsigned long start = micros();
    
    // Just bail if the timer is about to roll over
    if (start + dur < start) {
        return;
    }
    
    unsigned long now;
    int prev = 0;
    int count = 0;
    do
    {
        now = micros() - start;
/*
        double hwl = hwl1 + (now * (hwl2 - hwl1)) / (double) dur;
          
        //double m = now / hwl
        int cur = ((int) (now / hwl)) % 2;
        if (cur != prev) {
            if (cur)
                digitalWrite(SPEAKER_PIN, HIGH);
            else
                digitalWrite(SPEAKER_PIN, LOW);  
                
            prev = cur;
        }
*/
count++;
    } while (now < dur);
    
    Serial.print("count: ");
    Serial.println(count);
}

// Note: a single-cycle tick is barely audible (using the Sanco EMB-3008A speaker),
// just enough to hear with your ear next to the device.
// Ten cycles produces a more noticeable, though still quiet, click.
void tick()
{
    for (int i = 0; i < 10; i++) {
        digitalWrite(SPEAKER_PIN, HIGH);
        digitalWrite(SPEAKER_PIN, LOW);
    }
}

void sampleAnalog(unsigned long iterations)
{
    int len = 3;
    AnalogSampler samplers[] = {audioSampler, vibroSampler, photoSampler};
    char* prefixes[] = {AUDIO_OSC_PREFIX, VIBRO_OSC_PREFIX, PHOTO_OSC_PREFIX};
    
    for (unsigned long i = 0; i < iterations; i++)
    {
        for (int j = 0; j < len; j++)
        {
            samplers[j].sample();
        }
    }
    
    for (int j = 0; j < len; j++)
    {
        Serial.print(prefixes[j]);
        Serial.print("/data "); 
        Serial.print(samplers[j].getMinValue());
        Serial.print(" ");
        Serial.println(samplers[j].getMaxValue()); 
        samplers[j].reset();
    }
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
