/*
 * Monomanual Typeatron breadboard prototyping sketch
 * 
 * Connect the push button switches to pins 2-6 in a pulldown configuration
 * Connect the piezo buzzer to pin 7 and GND
 */

const int keyPin1 = 2;
const int keyPin2 = 3;
const int keyPin3 = 4;
const int keyPin4 = 5;
const int keyPin5 = 6;

const int speakerPin = 7;  
const int ledPin =  13;

void setup() {
    pinMode(keyPin1, INPUT);     
    pinMode(keyPin2, INPUT);     
    pinMode(keyPin3, INPUT);     
    pinMode(keyPin4, INPUT);     
    pinMode(keyPin5, INPUT);     

    pinMode(speakerPin, OUTPUT); 
    pinMode(ledPin, OUTPUT);
  
    Serial.begin(115200);  
}

unsigned int lastInputState = 0;

void loop() {  
    unsigned int input[5];
    unsigned inputState = 0;
      
    input[0] = digitalRead(keyPin1);
    input[1] = digitalRead(keyPin2);
    input[2] = digitalRead(keyPin3);
    input[3] = digitalRead(keyPin4);
    input[4] = digitalRead(keyPin5);
  
    for (int i = 0; i < 5; i++) {
      inputState |= input[i] << i;  
    }
 
    if (inputState != lastInputState) {
        Serial.print("/exo/tt/keys ");
        for (int i = 0; i < 5; i++) {
            Serial.print(input[i] ? "1" : "0");
        } 
        Serial.println("");
    }
  
    lastInputState = inputState;

    if (input[0] == HIGH) {     
        digitalWrite(ledPin, HIGH); 
        tone(speakerPin, 440); 
    } else {
        digitalWrite(ledPin, LOW);
        noTone(speakerPin);
    }
}
