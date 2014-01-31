#include <math.h>

double mag(double *vin) {
    return sqrt(pow(vin[0], 2) + pow(vin[1], 2) + pow(vin[2], 2));  
}

void normalize(double *vin, double *vout) {
    double m = mag(vin);
    vout[0] = vin[0] / m;
    vout[1] = vin[1] / m;
    vout[2] = vin[2] / m;
}

double distance(double *v1, double *v2) {
    return sqrt(pow(v1[0] - v2[0], 2) + pow(v1[1] - v2[1], 2) + pow(v1[2] - v2[2], 2));  
}

double openPalmCenter[] = {0.3093581, -0.6281864, -0.7139183};
double openPalmTolerance = 0.3;

double waveCenter1[] = {-0.8356163, 0.4055072, 0.3705529};
double waveTolerance1 = 0.5;

double waveCenter2[] = {0.9125032, -0.1936484, -0.3603307};
double waveTolerance2 = 0.5;

const char
    *openPalm = "open-palm",
    *wave1 = "wave-1",
    *wave2 = "wave-2",
    *none = "none";
    
const char *classifyGestureVector(double *v) {
    double normed[3];
    normalize(v, normed);
    
    double d;
    d = distance(normed, openPalmCenter);
    if (d <= openPalmTolerance) {
        return openPalm;
    }
    d = distance(normed, waveCenter1);
    if (d <= waveTolerance1) {
        return wave1;
    }
    d = distance(normed, waveCenter2);
    if (d <= waveTolerance2) {
        return wave2;
    }    
    
    return none;    
}
