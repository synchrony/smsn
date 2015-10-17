/*
 * Monomanual Typeatron driver sketch, copyright 2014-2015 by Joshua Shinavier
 *
 * See: https://github.com/joshsh/smsn
 */

#include <Typeatron.h>

#include <SmSn.h>
#include <SmSnDevice.h>
#include <AnalogSampler.h>
#include <Droidspeak.h>
#include <SmSnOsc.h>
#include <Morse.h>
#include <RGBLED.h>
#include <OSCBundle.h>

Typeatron typeatron;

// TODO: tailor the bounce interval to the switch being used.
// This 2ms value is a conservative estimate based on an average over many kinds of switches.
// See "A Guide to Debouncing" by Jack G. Ganssle
unsigned int debounceMicros = 2000;

unsigned int lastKeyState = 0;
unsigned int lastUpdate = 0;

void setup() {
  typeatron.setup();

  typeatron.pingUntilConnected();
}

// basic key and laser loop
void loop() {
    // execute the I/O portion of the loop as quickly as possible
    unsigned long ms = typeatron.beginLoop();
    
    // throttle sampling of keys appropriately
    unsigned int us = micros();
    if (us - lastUpdate >= debounceMicros) {
        typeatron.updateKeys();
        unsigned int keyState = typeatron.getKeyState();
        if (keyState != lastKeyState) {
            Mode mode = typeatron.getMode();
            if (LaserTrigger == mode) {
                if (keyState) {
                    typeatron.laserOn();
                    typeatron.sendLaserEvent();
                    typeatron.setMode(LaserPointer);
                } else {
                    typeatron.setMode(Normal);
                    typeatron.laserOff(); 
                }
            } else if (LaserPointer == mode) {
                typeatron.laserOff();
                if (!keyState) {
                    typeatron.setMode(Normal);
                }
            } else {      
                // TODO: temporary; shouldn't be necessary 
                typeatron.laserOff();
                
                typeatron.sendKeyState();
            }
                        
            lastUpdate = us;
            lastKeyState = keyState;
        }        
    }    
}



