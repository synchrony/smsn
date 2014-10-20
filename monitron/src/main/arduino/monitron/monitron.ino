/*
 * Omnisensory Monitron firmware, copyright 2012-2014 by Joshua Shinavier
 *
 * See: https://github.com/joshsh/extendo
 */


////////////////////////////////////////////////////////////////////////////////

// Each sampling cycle must take at least this long.
// If a cycle is finished sooner, we will wait before starting the next cycle.
#define CYCLE_MILLIS_MIN  3000

// A sampling cycle may take at most this long.
// If the sample runs over, an error message will be generated
#define CYCLE_MILLIS_MAX  4000


////////////////////////////////////////////////////////////////////////////////

#define SOUND_PIN         A0
#define VIBR_PIN          A1
#define LIGHT_PIN         A2
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
#define MOTION_PIN        8
#define RGB_LED_GREEN_PIN 9
#define RGB_LED_BLUE_PIN  10
#define RGB_LED_RED_PIN   11
// 12
#define LED_PIN           13


////////////////////////////////////////////////////////////////////////////////

// OSC addresses
const char *EXO_OM                            = "/exo/om";
const char *EXO_OM_PING                       = "/exo/om/ping";
const char *EXO_OM_PING_REPLY                 = "/exo/om/ping/reply";
const char *EXO_OM_SENSOR_7BB206L0_VIBRN      = "/exo/om/sensor/7bb206l0/vibr";
const char *EXO_OM_SENSOR_ADJDS311CR999_BLUE  = "/exo/om/sensor/adjds311cr999/blue";
const char *EXO_OM_SENSOR_ADJDS311CR999_GREEN = "/exo/om/sensor/adjds311cr999/green";
const char *EXO_OM_SENSOR_ADJDS311CR999_RED   = "/exo/om/sensor/adjds311cr999/red";
const char *EXO_OM_SENSOR_BMP085_PRESSURE     = "/exo/om/sensor/bmp085/press";
const char *EXO_OM_SENSOR_BMP085_TEMP         = "/exo/om/sensor/bmp085/temp";
const char *EXO_OM_SENSOR_GP2Y1010AU0F_DUST   = "/exo/om/sensor/gp2y1010au0f/dust";
const char *EXO_OM_SENSOR_MD9745APZF_SOUND    = "/exo/om/sensor/md9745apzf/sound";
const char *EXO_OM_SENSOR_PHOTO_LIGHT         = "/exo/om/sensor/photo/light";
const char *EXO_OM_SENSOR_RHT03_HUMID         = "/exo/om/sensor/rht03/humid";
const char *EXO_OM_SENSOR_RHT03_TEMP          = "/exo/om/sensor/rht03/temp";
const char *EXO_OM_SENSOR_SE10_MOTION         = "/exo/om/sensor/se10/motion";
const char *EXO_OM_SYSTEM_TIME                = "/exo/om/system/time";


////////////////////////////////////////////////////////////////////////////////

#include <Wire.h>


////////////////////////////////////////////////////////////////////////////////

#include <AnalogSampler.h>

AnalogSampler sampler_7bb206l0_vibr(VIBR_PIN);
AnalogSampler sampler_adjds311cr999_blue(0);
AnalogSampler sampler_adjds311cr999_green(0);
AnalogSampler sampler_adjds311cr999_red(0);
AnalogSampler sampler_bmp085_press(0);
AnalogSampler sampler_bmp085_temp(0);
AnalogSampler sampler_gp2y1010au0f_dust(0);
AnalogSampler sampler_md9745apzf_sound(SOUND_PIN);
AnalogSampler sampler_photo_light(LIGHT_PIN);
AnalogSampler sampler_rht03_humid(0);
AnalogSampler sampler_rht03_temp(0);


////////////////////////////////////////////////////////////////////////////////

#include <DHT22.h>

DHT22 dht22(DHT22_PIN);


////////////////////////////////////////////////////////////////////////////////

#include <BMP085.h>

BMP085 bmp085;


////////////////////////////////////////////////////////////////////////////////

#include <RGBLED.h>

RGBLED rgbled(RGB_LED_RED_PIN, RGB_LED_GREEN_PIN, RGB_LED_BLUE_PIN);


////////////////////////////////////////////////////////////////////////////////

#include <Droidspeak.h>

Droidspeak droidspeak(SPEAKER_PIN);


////////////////////////////////////////////////////////////////////////////////

#include <OSCBundle.h>
#include <ExtendOSC.h>

ExtendOSC osc(EXO_OM);

OSCBundle *bundleIn;

void error(const char *message) {
    osc.sendError(message);
}


////////////////////////////////////////////////////////////////////////////////

#include "om_dust.h"
#include "om_motion.h"
#include "om_timer.h"


////////////////////////////////////////////////////////////////////////////////

void setup() {
    pinMode(SPEAKER_PIN, OUTPUT);    
    pinMode(DUST_LED_PIN, OUTPUT);
    pinMode(LED_PIN, OUTPUT);
    pinMode(MOTION_PIN, INPUT);
    
    rgbled.setup();
    droidspeak.speakPowerUpPhrase();
    
    randomSeed(analogRead(LIGHT_PIN));

    bmp085.setup();

    Serial.begin(115200);
    droidspeak.speakSerialOpenPhrase();

    bundleIn = new OSCBundle();

    //lastCycle_highBits = 0;
    //lastCycle_lowBits = millis();

    rgbled.replaceColor(RGB_GREEN);
    osc.sendInfo("Monitron is ready");
}


////////////////////////////////////////////////////////////////////////////////

void sendPingReply() {
    OSCMessage m(EXO_OM_PING_REPLY);
    m.add((uint64_t) micros());

    osc.sendOSC(m);
}

void handleAudioToneMessage(class OSCMessage &m) {
    if (!osc.validArgs(m, 2)) return;

    int32_t frequency = m.getInt(0);
    int32_t duration = m.getInt(1);

    if (frequency <= 0 || frequency > 20000) {
        osc.sendError("frequency out of range: %d", (int) frequency);
    } else if (duration <= 0) {
        osc.sendError("duration must be a positive number");
    } else if (duration > 60000) {
        osc.sendError("duration too long");
    } else {
        tone(SPEAKER_PIN, (int) frequency);
        delay((unsigned long) duration);
        noTone(SPEAKER_PIN);
    }
}

void handlePingMessage(class OSCMessage &m) {
    sendPingReply();
}

void handleOSCBundle(class OSCBundle &bundle) {
    if (bundle.hasError()) {
        osc.sendOSCBundleError(bundle);
    } else if (!(0
        || bundle.dispatch(EXO_OM_AUDIO_TONE, handleAudioToneMessage)
        || bundle.dispatch(EXO_HAND_PING, handlePingMessage)
        )) {
        osc.sendError("no messages dispatched");
    }
}


////////////////////////////////////////////////////////////////////////////////

void loop()
{
    if (osc.receiveOSCBundle(*bundleIn)) {
        handleOSCBundle(*bundleIn);
        bundleIn->empty();
        delete bundleIn;
        bundleIn = new OSCBundle();
    }

    startCycle();
    digitalWrite(LED_PIN, HIGH);
    
    resetMotionDetector();
    sampleMotionDetector();
    
    //samplePhotoresistor();
    sampleAnalog();
    sampleDHT22();
    sampleBMP085();
    sampleDustSensor();
    
    sampleMotionDetector();
    reportMotionDetectorResults();
    
    //rateTest();
    
    digitalWrite(LED_PIN, LOW);
    endCycle();
}


////////////////////////////////////////////////////////////////////////////////

void beginSample()
{
    rgbled.pushColor(RGB_BLUE);
}

void endSample()
{
    rgbled.popColor();
}

void beginOSCWrite()
{
    rgbled.pushColor(RGB_CYAN);
    droidspeak.tick();
    //tone(SPEAKER_PIN, 55);
}

void endOSCWrite()
{
    //noTone(SPEAKER_PIN);
    rgbled.popColor();
}

void beginOSCErrorMessage()
{
    rgbled.pushColor(RGB_RED);
    droidspeak.tick();
    //tone(SPEAKER_PIN, 110);
}

void endOSCErrorMessage()
{
    //noTone(SPEAKER_PIN);
    delay(50);
    rgbled.popColor();
}

void finishAnalogObservation(AnalogSampler &s, char* address)
{
    OSCMessage m(address);
    m.add((uint64_t) s.getStartTime());
    m.add((uint64_t) s.getEndTime());
    m.add(s.getNumberOfMeasurements());
    m.add(s.getMinValue());
    m.add(s.getMaxValue());
    m.add(s.getMean());
    m.add(s.getVariance());
    osc.sendOSC(m);
    
    s.reset();
}

const unsigned long analogIterations = 10000;

void sampleAnalog()
{    
    beginSample();
    sampler_7bb206l0_vibr.beginSample();
    sampler_md9745apzf_sound.beginSample();
    sampler_photo_light.beginSample();
    for (unsigned long i = 0; i < analogIterations; i++)
    {
        sampler_7bb206l0_vibr.measure();
        sampler_md9745apzf_sound.measure();
        sampler_photo_light.measure();
    }
    sampler_7bb206l0_vibr.endSample();
    sampler_md9745apzf_sound.endSample();
    sampler_photo_light.endSample();    
    endSample();
    
    finishAnalogObservation(sampler_7bb206l0_vibr, EXO_OM_SENSOR_7BB206L0_VIBRN);
    finishAnalogObservation(sampler_md9745apzf_sound, EXO_OM_SENSOR_MD9745APZF_SOUND);
    finishAnalogObservation(sampler_photo_light, EXO_OM_SENSOR_PHOTO_LIGHT);
}

// needs 2s between readings
// however, the actual readData operation takes only 0.03ms on Arduino Nano
void sampleDHT22()
{
    DHT22_ERROR_t errorCode;

    beginSample();
    sampler_rht03_humid.beginSample();
    sampler_rht03_temp.beginSample();
    errorCode = dht22.readData();
    if (DHT_ERROR_NONE == errorCode) {
        sampler_rht03_humid.addMeasurement(dht22.getHumidity() / 100.0);
        sampler_rht03_temp.addMeasurement(dht22.getTemperatureC());
    }
    sampler_rht03_humid.endSample();
    sampler_rht03_temp.endSample();
    endSample();
    
    switch(errorCode)
    {
        case DHT_ERROR_NONE:
            beginOSCWrite();
            finishAnalogObservation(sampler_rht03_humid, EXO_OM_SENSOR_RHT03_HUMID);
            finishAnalogObservation(sampler_rht03_temp, EXO_OM_SENSOR_RHT03_TEMP);
            endOSCWrite();
            break;
        case DHT_ERROR_CHECKSUM:
            osc.sendError("RHT03 checksum-error");
            break;
        case DHT_BUS_HUNG:
            osc.sendError("RHT03 bus-hung");
            break;
        case DHT_ERROR_NOT_PRESENT:
            osc.sendError("RHT03 not-present");
            break;
        case DHT_ERROR_ACK_TOO_LONG:
            osc.sendError("RHT03 ack-timeout");
            break;
        case DHT_ERROR_SYNC_TIMEOUT:
            osc.sendError("RHT03 sync-timeout");
            break;
        case DHT_ERROR_DATA_TIMEOUT:
            osc.sendError("RHT03 data-timeout");
            break;
        case DHT_ERROR_TOOQUICK:
            osc.sendError("RHT03 polled-too-quick");
            break;
        default:
            osc.sendError("unexpected RHT03 error code: %d", errorCode);
    }
}

void sampleBMP085()
{
    // this has been found to take around 12ms (on Arduino Nano)
    beginSample();
    sampler_bmp085_press.beginSample();
    sampler_bmp085_temp.beginSample();
    bmp085.sample();
    sampler_bmp085_press.addMeasurement(bmp085.getLastPressure());
    sampler_bmp085_temp.addMeasurement(bmp085.getLastTemperature() / 10.0);
    sampler_bmp085_press.endSample();
    sampler_bmp085_temp.endSample();
    endSample();
    
    beginOSCWrite();
    finishAnalogObservation(sampler_bmp085_press, EXO_OM_SENSOR_BMP085_PRESSURE);
    finishAnalogObservation(sampler_bmp085_temp, EXO_OM_SENSOR_BMP085_TEMP);
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

    osc.sendInfo("%d iterations in %d ms", n, duration);
}
