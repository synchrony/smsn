/*
 * Monomanual Typeatron driver sketch, copyright 2014 by Joshua Shinavier
 *
 * See: https://github.com/joshsh/extendo
 */

#include <Typeatron.h>

#include <ExtendoDevice.h>
#include <AnalogSampler.h>
#include <Droidspeak.h>
#include <ExtendOSC.h>
#include <Morse.h>
#include <RGBLED.h>
#include <OSCBundle.h>

Typeatron typeatron;


// TODO: tailor the bounce interval to the switch being used.
// This 2ms value is a conservative estimate based on an average over many kinds of switches.
// See "A Guide to Debouncing" by Jack G. Ganssle
unsigned int debounceMicros = 2000;

void setup() {
    typeatron.setup();
}

unsigned int lastKeyState = 0;

unsigned long flickerDurationMs = 300;
void flickerTmp() {
   unsigned long start = millis(), now;
   do {
       typeatron.setColor(RGB_RED);
       delay(30);
       typeatron.setColor(RGB_BLACK);
       delay(10);
       now = millis();
   } while (now - start < flickerDurationMs); 
}

// basic key and laser loop
void loop() {
    unsigned long now = typeatron.beginLoop();
//typeatron.setColorFor(RGB_RED, 10000);

    typeatron.updateKeys();
    unsigned int keyState = typeatron.getKeyState();
    Mode mode = typeatron.getMode();
    if (keyState != lastKeyState) {
//flickerTmp();
        if (LaserTrigger == mode) {
            if (keyState) {
                typeatron.laserOn();
                typeatron.sendLaserEvent();
                typeatron.setMode(LaserPointer);
            }
        } else if (LaserPointer == mode) {
            if (!keyState) {
                typeatron.setMode(Normal);
                typeatron.laserOff();
            }
        } else {       
            unsigned int before = micros();
            typeatron.sendKeyState();
            unsigned int after = micros();
            
            if (after - before < debounceMicros) {
                delayMicroseconds(debounceMicros - (after - before));
            }
        }
    } 
    
    lastKeyState = keyState;
}
