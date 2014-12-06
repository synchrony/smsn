
int speakerOut = 2;

void setup() {
 pinMode(speakerOut, OUTPUT);
}

void loop() {
  int d = 100;
  int volume = 1023;
  int iterations = 4;
  
  delay(500);
  for (int i = 0; i < iterations; i++) {
      digitalWrite(speakerOut, HIGH);
      delayMicroseconds(d);
      digitalWrite(speakerOut, LOW);
      delayMicroseconds(d);
  }
}
