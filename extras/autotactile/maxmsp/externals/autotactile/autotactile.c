/**
	@file
	autotactile - a max object shell
	jeremy bernstein - jeremy@bootsquad.com	

	@ingroup	examples	
*/

#include "ext.h"							// standard Max include, always required
#include "ext_obex.h"						// required for new style Max object

#define DEFAULT_SAMPLE_SIZE 16
#define MAX_SAMPLE_SIZE 256

////////////////////////// object struct
typedef struct _autotactile 
{
	t_object	ob;			// the object itself (must be first)
	int			sample_size;
	void*		m_out;
	void**		outputs;
	//void*		m_proxy;
	//long		m_proxy_inletnum;          // space for the inlet number used by all the proxies
} t_autotactile;

///////////////////////// function prototypes
//// standard set
void *autotactile_new(t_symbol *s, long argc, t_atom *argv);
void autotactile_free(t_autotactile *x);
void autotactile_assist(t_autotactile *x, void *b, long m, long a, char *s);
void autotactile_inletinfo(t_autotactile *x, void *b, long a, char *t);
void autotactile_handle_sample_old(t_autotactile *x, int data);
void autotactile_handle_sample(t_autotactile *x, int data);
void autotactile_handle_sample_new(t_autotactile *x, t_symbol *s, long argc, t_atom *argv);

// global class pointer variable
void *autotactile_class;


int main(void)
{	
	// object initialization, OLD STYLE
	// setup((t_messlist **)&autotactile_class, (method)autotactile_new, (method)autotactile_free, (short)sizeof(t_autotactile), 
	//		0L, A_GIMME, 0);
    // addmess((method)autotactile_assist,			"assist",		A_CANT, 0);  
	
	// object initialization, NEW STYLE
	t_class *c;
	
	c = class_new("autotactile", (method)autotactile_new, (method)autotactile_free, (long)sizeof(t_autotactile), 
				  0L /* leave NULL!! */, A_GIMME, 0);
	
	/* you CAN'T call this from the patcher */
    class_addmethod(c, (method)autotactile_assist,	"assist",		A_CANT, 0);  
	class_addmethod(c, (method)autotactile_inletinfo, "inletinfo", A_CANT, 0);

	//class_addmethod(c, (method)autotactile_handle_sample_old, "int", A_LONG, 0);
	//class_addmethod(c, (method)autotactile_handle_sample, "int", A_LONG, 0);
	class_addmethod(c, (method)autotactile_handle_sample_new, "data", A_GIMME, 0);

	class_register(CLASS_BOX, c); /* CLASS_NOBOX */
	autotactile_class = c;

	post("Merdre! De par ma chandelle verte!");
	return 0;
}

void autotactile_assist(t_autotactile *x, void *b, long m, long a, char *s)
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
void autotactile_inletinfo(t_autotactile *x, void *b, long a, char *t)
{
	if (a) {
		*t = 1;
	}
}

void autotactile_free(t_autotactile *x)
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
void *autotactile_new(t_symbol *s, long argc, t_atom *argv)
{
	t_autotactile *x = NULL;
	int i;
	
	// object instantiation, NEW STYLE
	if (x = (t_autotactile *)object_alloc(autotactile_class)) {
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

void autotactile_handle_sample_old(t_autotactile *x, int data)
{
	object_post((t_object *) x, "received a sample: %ld", data);

	int res = data * 2;
    outlet_int(x->m_out, res);
}

// TODO: --> char *data
void autotactile_handle_sample(t_autotactile *x, int data)
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

void autotactile_handle_sample_new(t_autotactile *x, t_symbol *s, long argc, t_atom *argv) {
	long i;
	t_atom *ap;
	char *data;
	
	if (2 == argc) {
		ap = argv;
		if (A_SYM == atom_gettype(ap)) {
			data = atom_getsym(ap)->s_name;
			if (x->sample_size == strlen(data)) {
				post("got the data: %s", data);
				for (i = 0; i < x->sample_size; i++) {
					if ('o' == data[i]) {
						outlet_int(x->outputs[x->sample_size - 1 - i], 1);
					}						
				}
			}
		}
	}
	/*
	
	post("message selector is %s",s->s_name);
	post("there are %ld arguments",argc);
	for (i = 0, ap = argv; i < argc; i++, ap++) {       // increment ap each time to get to the next atom
		switch (atom_gettype(ap)) {
			case A_LONG:
				post("%ld: %ld",i+1,atom_getlong(ap));
				break;
			case A_FLOAT:
				post("%ld: %.2f",i+1,atom_getfloat(ap));
				break;
			case A_SYM:
				post("%ld: %s",i+1, atom_getsym(ap)->s_name);
				break;
			default:
				post("%ld: unknown atom type (%ld)", i+1, atom_gettype(ap));
				break;
		}
	}*/	
}

//void autotactile_handle_sample_newerstill(t_autotactile *x, t_symbol *data) {
//	post("got data: %s", atom_getsym(data)
//}


