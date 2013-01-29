/**
	@file
	chand_autocorr 
*/

#include <math.h>
#include <string.h>

#include "ext.h"							// standard Max include, always required
#include "ext_obex.h"						// required for new style Max object
#include "ext_systime.h"  // So we can get the time in milliseconds... right?

#define DEFAULT_SAMPLE_SIZE 16
#define MAX_SAMPLE_SIZE 256

// Half life of event significance, in milliseconds.
#define HALF_LIFE 500.0

//#define DAMPING_BASE 0.9952
#define DAMPING_FACTOR 0.0125

#define T_high 50.0
#define T_low 250.0
#define f_high 1000.0
#define f_low 220.0
double slope = (f_low - f_high) / (T_low - T_high);

////////////////////////// object struct

typedef struct _chand_autocorr 
{
	t_object	ob;			// the object itself (must be first)
	int			sample_size;
	void*		m_out;
	void**		outputs;
	//void*		m_proxy;
	//long		m_proxy_inletnum;          // space for the inlet number used by all the proxies
} t_chand_autocorr;

typedef struct _sensor_history {
	double total_weight;
	double weighted_value;
	unsigned long time;  // time of last change, in milliseconds
} t_sensor_history;

///////////////////////// function prototypes

void *chand_autocorr_new(t_symbol *s, long argc, t_atom *argv);
void chand_autocorr_free(t_chand_autocorr *x);
void chand_autocorr_assist(t_chand_autocorr *x, void *b, long m, long a, char *s);
void chand_autocorr_inletinfo(t_chand_autocorr *x, void *b, long a, char *t);
void chand_autocorr_handle_sample_old(t_chand_autocorr *x, int data);
void chand_autocorr_handle_sample(t_chand_autocorr *x, int data);
void chand_autocorr_handle_sample_new(t_chand_autocorr *x, t_symbol *s, long argc, t_atom *argv);

void initialize_sensor_history(t_sensor_history *h);
double next_frequency(unsigned long t, t_sensor_history *h);

// global class pointer variable
void *chand_autocorr_class;

t_sensor_history histories[MAX_SAMPLE_SIZE];
char last_sample_data[MAX_SAMPLE_SIZE + 1];

// new /////////////////////////////////////////////////////////////////////////

typedef unsigned char boolean;

#define SAMPLE_SIZE DEFAULT_SAMPLE_SIZE

// We 'remember' this many samples for each sensor.
// For example, if there are 200 samples per second, a TAU_MAX of 200 gives us
// a memory of one second.
#define TAU_MAX	200

typedef struct _autocorr_signal {
	unsigned char history[TAU_MAX];
	double autocorrelation[TAU_MAX];
	double radix;
    unsigned int cur_index;
} t_autocorr_signal;

t_autocorr_signal *create_autocorr_signal(unsigned int halflife) {
	int i;
	t_autocorr_signal *s = malloc(sizeof(t_autocorr_signal));
	
	for (i = 0; i < TAU_MAX; i++) {
		s->history[i] = 0;
		s->autocorrelation[i] = 0;
	}
	
	s->cur_index = 0;
	
	s->radix = pow(0.5, (1.0 / (double) halflife));
	
	return s;
}

void free_autocorr_signal(t_autocorr_signal *s) {
	free(s);
}

void advance_autocorr_signal(t_autocorr_signal *s,
							 boolean value) {
	// j is tau - 1
    int j;
	
	// Note: the autocorrelation value for a given tau is not normalized.
	// To normalize the value to (0, 1), multiply it by (1 - radix).
	for (j = 0; j < TAU_MAX; j++) {
		boolean f = value && s->history[(s->cur_index + TAU_MAX - (j + 1)) % TAU_MAX];
		s->autocorrelation[j] = f + (s->autocorrelation[j] * s->radix);
	}
		
	s->history[s->cur_index] = value;
	s->cur_index = (s->cur_index + 1) % TAU_MAX;
}

void advance_autocorr_signal_nsteps(t_autocorr_signal *s,
									boolean value,
									unsigned int steps) {
	int i;
	
	for (i = 0; i < steps; i++) {
		advance_autocorr_signal(s, value);
	}
}


////////////////////////////////////////////////////////////////////////////////

int main(void)
{
	int i;
	
	for (i = 0; i < MAX_SAMPLE_SIZE; i++) {
		initialize_sensor_history(histories + i);
		last_sample_data[i] = 0;
	}
	
	// object initialization, OLD STYLE
	// setup((t_messlist **)&chand_autocorr_class, (method)chand_autocorr_new, (method)chand_autocorr_free, (short)sizeof(t_chand_autocorr), 
	//		0L, A_GIMME, 0);
    // addmess((method)chand_autocorr_assist,			"assist",		A_CANT, 0);  
	
	// object initialization, NEW STYLE
	t_class *c;
	
	c = class_new("chand_autocorr", (method)chand_autocorr_new, (method)chand_autocorr_free, (long)sizeof(t_chand_autocorr), 
				  0L /* leave NULL!! */, A_GIMME, 0);
	
	/* you CAN'T call this from the patcher */
    class_addmethod(c, (method)chand_autocorr_assist,	"assist",		A_CANT, 0);  
	class_addmethod(c, (method)chand_autocorr_inletinfo, "inletinfo", A_CANT, 0);

	//class_addmethod(c, (method)chand_autocorr_handle_sample_old, "int", A_LONG, 0);
	//class_addmethod(c, (method)chand_autocorr_handle_sample, "int", A_LONG, 0);
	class_addmethod(c, (method)chand_autocorr_handle_sample_new, "data", A_GIMME, 0);

	class_register(CLASS_BOX, c); /* CLASS_NOBOX */
	chand_autocorr_class = c;

	post("Merdre! De par ma chandelle verte!");
	return 0;
}

void chand_autocorr_assist(t_chand_autocorr *x, void *b, long m, long a, char *s)
{
	if (ASSIST_INLET == m) { // inlet
		if (0 == a) {
			sprintf(s, "the sample data");
		}
		//sprintf(s, "I am inlet %ld", a);
	} 
	else {	// outlet
		sprintf(s, "I am outlet %ld", a); 			
	}
/*
	if (m == ASSIST_OUTLET)
		if (a == 0)
			sprintf(s,"Minimum Value");
	if (a == 1)
		sprintf(s, "Index of the Minimum Value");
	else {
		switch (a) {	
			case 0:
				sprintf(s,"Compares Left and Right Inlets");
				break;
			case 1:
				sprintf(s,"Value to be Compared");
				break;
*/				
}

// TODO: copied from another external.  What does it do?
void chand_autocorr_inletinfo(t_chand_autocorr *x, void *b, long a, char *t)
{
	if (a) {
		*t = 1;
	}
}

void chand_autocorr_free(t_chand_autocorr *x)
{
	/*if (x->m_args) {
		sysmem_freeptr(x->m_args);
	}*/
	sysmem_freeptr(x->outputs);
}

/*
 A_GIMME signature =
	t_symbol	*s		objectname
	long		argc	num additonal args
	t_atom		*argv	array of t_atom structs
		 type = argv->a_type
		 if (type == A_LONG) ;
		 else if (type == A_FLOAT) ;
		 else if (type == A_SYM) ;
*/
/*
	t_symbol {
		char *s_name;
		t_object *s_thing;
	}
*/
void *chand_autocorr_new(t_symbol *s, long argc, t_atom *argv)
{
	t_chand_autocorr *x = NULL;
	int i;
	
	// object instantiation, NEW STYLE
	if (x = (t_chand_autocorr *)object_alloc(chand_autocorr_class)) {
        object_post((t_object *)x, "a new %s object was instantiated: 0x%X", s->s_name, x);
        object_post((t_object *)x, "it has %ld arguments", argc);
		
		x->sample_size = DEFAULT_SAMPLE_SIZE;
		if (0 < argc) {
			if (A_LONG == argv->a_type) {
				x->sample_size = atom_getlong(argv);
				if (x->sample_size < 1) {
					x->sample_size = 1;
				} else if (x->sample_size > MAX_SAMPLE_SIZE) {
					x->sample_size = MAX_SAMPLE_SIZE;
				}
			}
		}
		object_post((t_object *) x, "the sample size is %ld", x->sample_size);
		
//		x->m_proxy = proxy_new((t_object *) x, 1, &x->m_proxy_inletnum);
		x->m_out = intout(x);
		
		x->outputs = (t_atom **) sysmem_newptr(x->sample_size * sizeof(t_atom));
		for (i = 0; i < x->sample_size; i++) {
			x->outputs[i] = intout(x);
		}
		
		/*
		
        for (i = 0; i < argc; i++) {
            if ((argv + i)->a_type == A_LONG) {
                object_post((t_object *)x, "arg %ld: long (%ld)", i, atom_getlong(argv+i));
            } else if ((argv + i)->a_type == A_FLOAT) {
                object_post((t_object *)x, "arg %ld: float (%f)", i, atom_getfloat(argv+i));
            } else if ((argv + i)->a_type == A_SYM) {
                object_post((t_object *)x, "arg %ld: symbol (%s)", i, atom_getsym(argv+i)->s_name);
            } else {
                object_error((t_object *)x, "forbidden argument");
            }
        }
		 */
	}
	
	//x->port[0] = NULL;
	
	return (x);
}

void chand_autocorr_handle_sample_old(t_chand_autocorr *x, int data)
{
	object_post((t_object *) x, "received a sample: %ld", data);

	int res = data * 2;
    outlet_int(x->m_out, res);
}

// TODO: --> char *data
void chand_autocorr_handle_sample(t_chand_autocorr *x, int data)
{
	long inlet = proxy_getinlet((t_object *) x);
	switch (inlet) {
		case 0:
			post("bang received in left inlet");
			break;
		case 1:
			post("bang received in right inlet");
			break;
	}
}

void chand_autocorr_handle_sample_new(t_chand_autocorr *x, t_symbol *s, long argc, t_atom *argv) {
	long i;
	t_atom *ap;
	char *data;
	double f;
	unsigned long t;
	
	if (2 == argc) {
		ap = argv;
		if (A_SYM == atom_gettype(ap)) {
			data = atom_getsym(ap)->s_name;
			if (x->sample_size == strlen(data)) {
				//post("got the data: %s", data);
				t = systime_ms();
				
				for (i = 0; i < x->sample_size; i++) {
					if ('o' == data[i] && data[i] != last_sample_data[i]) {
					//if (data[i] != last_sample_data[i]) {
						f = next_frequency(t, histories + i);
					//if ('o' == data[i]) {
						outlet_int(x->outputs[x->sample_size - 1 - i], f);
					}						
				}
				strcpy(last_sample_data, data);
			}
		}
	}	
}

////////////////////////////////////////////////////////////////////////////////

void initialize_sensor_history(t_sensor_history *h) {
	h->time = 0;
	h->total_weight = 0;
	h->weighted_value = 0;
}

double next_frequency(unsigned long t, t_sensor_history *h) {
    long td;
	double v;
	double c;
	double f;
	double d;
		
	if (h->time) {
		// Elapsed time since last change.
		td = t - h->time;
	
		v = f_high + (slope * (td - T_high));
		/*if (v > f_high) {
			v = f_high;
		} else*/ if (v < 0) {
			v = 0;
		}
		
		// Decay factor.
		c = pow(0.5, (td / HALF_LIFE));
	
		// Damping factor.
		d = 1 - pow(0.5, DAMPING_FACTOR * (double) td);
		
		h->total_weight = d + (c * h->total_weight);
		h->weighted_value = d*v + (c * h->weighted_value);
		f = h->weighted_value / h->total_weight;
		
		post("* td:%i c:%f d:%f v:%f f:%f (x:%f)", td, c, d, v, f, (td/HALF_LIFE));
	} else {
		f = 0;
		
		post(".");
	}
	
	h->time = t;
	return f;
}








