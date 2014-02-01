

// Codes for punctuation taken from: http://home.windstream.net/johnshan/cw_ss_list_punc.html
// Not used: the paragraph symbol .-.-..
const char* morseCodes[96] = {
/* ! */ "---.",
/* " */ ".-..-.",
/* # */ 0,
/* $ */ "...-..-",
/* % */ 0,
/* & */ 0,
/* ' */ ".----.",
/* ( */ "-.--.",
/* ) */ "-.--.-",
/* * */ 0,
/* + */ ".-.-.",
/* , */ "--..--",
/* - */ "-....-",
/* . */ ".-.-.-",
/* / */ "-..-.",
/* 0 */ "-----",
/* 1 */ ".----",
/* 2 */ "..---",
/* 3 */ "...--",
/* 4 */ "....-",
/* 5 */ ".....",
/* 6 */ "-....",
/* 7 */ "--...",
/* 8 */ "---..",
/* 9 */ "----.",
/* : */ "---...",
/* ; */ "-.-.-.",
/* < */ 0,
/* = */ "-...-",
/* > */ 0,
/* ? */ "..--..",
/* @ */ ".--.-.",
/* A */ ".-",
/* B */ "-...",
/* C */ "-.-.",
/* D */ "-..",
/* E */ ".",
/* F */ "..-.",
/* G */ "--.",
/* H */ "....",
/* I */ "..",
/* J */ ".---",
/* K */ "-.-",
/* L */ ".-..",
/* M */ "--",
/* N */ "-.",
/* O */ "---",
/* P */ ".--.",
/* Q */ "--.-",
/* R */ ".-.",
/* S */ "...",
/* T */ "-",
/* U */ "..-",
/* V */ "...-",
/* W */ ".--",
/* X */ "-..-",
/* Y */ "-.--",
/* Z */ "--..",
/* [ */ "-.--.",
/* \ */ 0,
/* ] */ "-.--.-",
/* ^ */ 0,
/* _ */ "..--.-",
/* ` */ 0,
/* a */ ".-",
/* b */ "-...",
/* c */ "-.-.",
/* d */ "-..",
/* e */ ".",
/* f */ "..-.",
/* g */ "--.",
/* h */ "....",
/* i */ "..",
/* j */ ".---",
/* k */ "-.-",
/* l */ ".-..",
/* m */ "--",
/* n */ "-.",
/* o */ "---",
/* p */ ".--.",
/* q */ "--.-",
/* r */ ".-.",
/* s */ "...",
/* t */ "-",
/* u */ "..-",
/* v */ "...-",
/* w */ ".--",
/* x */ "-..-",
/* y */ "-.--",
/* z */ "--..",
/* { */ 0,
/* | */ 0,
/* } */ 0,
/* ~ */ 0,
/* <del> */ 0,
};

int morsePin = transducerPin;

int morsePitch = 1760;
long timeUnit = 50;

void dit() {
    tone(morsePin, morsePitch); 
    delay(timeUnit);    
}

void dah() {
    tone(morsePin, morsePitch);
    delay(3 * timeUnit);
}

void interElementGap() {
    noTone(morsePin);
    delay(timeUnit);
}

// gap between letters
void shortGap() {
    noTone(morsePin);
    delay(3 * timeUnit);
}

// gap between words
void mediumGap() {
    noTone(morsePin);
    delay(7 * timeUnit);
}

void beginSequence() {
    // insert a short gap at the beginning of the message, to separate it from any (non-Morse) tones which came before
    interElementGap();      
}

void endSequence() {
    // end the last tone and insert a gap after the message, in case another message follows it
    mediumGap();
}

void playMorseString(const char* message, int (*stopTest)()) {    
    int firstLetterInWord = true;
    
    const char *cur = message;
    while (*cur) {
        char c = *cur;
        
        if (32 == c) {
            mediumGap();
            firstLetterInWord = true;
            cur++;
            continue;      
        } else if (c < 32 || c > 127) {
            c = '?'; 
        }
        
        const char *sequence = morseCodes[c - 33];
        if (!sequence) {
            sequence = morseCodes['?' - 33];
        }
        
        if (firstLetterInWord) {
            firstLetterInWord = false; 
        } else {
            shortGap();
        }
        
        int firstElementInLetter = true;
        const char *cur2 = sequence;
        while (*cur2) {
            if (firstElementInLetter) {
                firstElementInLetter = false;
            } else {
                interElementGap();  
            }
            char c2 = *cur2;
            if ('.' == c2) {
                dit();  
            } else if ('-' == c2) {
                dah();
            } else {
                sendError("element in morse sequence is neither a dit '.' nor a dah '-'");
            }
            
            cur2++; 
        }
        
        cur++;
    }
    
    endSequence();
}

void playMorseInt(int d, int (*stopTest)()) {
    sprintf(errstr, "%d", d);
    playMorseString(errstr, stopTest);  
}

