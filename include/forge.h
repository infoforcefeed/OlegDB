/*Forge is a basic utility library, consisting of pure c functions that are useful in any development. Forge is meant to be included in to all c files and all other Quel Solaar librariues and applications. Because it is included in all files it can provide some very useful functiuonality such as debugging memory and help find memory leaks. Forge is designed to be lightweight and compleatly platform and dependency indipendent.
*/


#if !defined(TYPES_H)
#define	TYPES_H

#ifndef NULL
#ifdef __cplusplus
#define NULL    0 /* Defines NULL in C++*/
#else
#define NULL    ((void *)0) /* Defines NULL in C*/
#endif
#endif

#if !defined(TRUE)
#define TRUE 1 /* Defines TRUE */
#endif
#if !defined(FALSE)
#define FALSE 0 /* Defines FALSE*/
#endif
#if defined _WIN32
typedef unsigned int uint;
#else
#include <sys/types.h>
#endif
#if !defined(VERSE_TYPES)
typedef signed char int8;
typedef unsigned char uint8;
typedef signed short int16;
typedef unsigned short uint16;
typedef signed int int32;
typedef unsigned int uint32;
typedef float real32;
typedef double real64;
typedef unsigned char boolean;
#endif

#define F_DOUBLE_PRECISION /* if F_DOUBLE_PRECISION is defined the type "freal" is defined as double otherwhise it will be defined as float. This is very usefull if you want to write an application that can be compiled to use either 32 or 64 bit floating point math. Forges entire match librarie are defined in both 32 and 64 bit precission and all functionality can be accessed using the freal type, a macro will replace all freal calls with the aropriate funtion using either 32 or 64 bit depending on if F_DOUBLE_PRECISION is defined*/

#ifdef F_DOUBLE_PRECISION 
typedef double freal;
#else
typedef float freal;
#endif

#define PI  3.141592653 /* Defines PI */



#if !defined(F_NO_MEMORY_DEBUG)
// #define F_MEMORY_DEBUG /* turns on the memory debugging system */
#endif
#if !defined(F_EXIT_CRASH)
// #define F_EXIT_CRASH /* turns on the crash on exit */
#endif

#ifdef F_MEMORY_DEBUG

/* ----- Debugging -----
If F_MEMORY_DEBUG  is enabled, the memory debugging system will create macros that replace malloc, 
free and realloc and allows the system to keppt track of and report where memory is beeing allocated, 
how much and if the memory is beeing freed. This is very useful for finding memory leaks in large applications. 
The system can also over allocate memory and fill it with a magic number and can therfor detect if the application 
writes outside of the allocated memory. if F_EXIT_CRASH is defined, then exit(); will be replaced with a funtion 
that writes to NULL. This will make it trivial ti find out where an application exits using any debugger., */

extern void *f_debug_mem_malloc(uint size, char *file, uint line); /* Replaces malloc and records the c file and line where it was called*/
extern void *f_debug_mem_calloc(uint nmemb, uint size, char *file, uint line); /* Replaces malloc and records the c file and line where it was called*/
extern void *f_debug_mem_realloc(void *pointer, uint size, char *file, uint line); /* Replaces realloc and records the c file and line where it was called*/
extern void f_debug_mem_free(void *buf); /* Replaces free and records the c file and line where it was called*/
extern void f_debug_mem_print(uint min_allocs); /* Prints out a list of all allocations made, their location, how much memorey each has allocated, freed, and how many allocations have been made. The min_allocs parameter can be set to avoid printing any allocations that have been made fewer times then min_allocs */
extern void f_debug_mem_reset(); /* f_debug_mem_reset allows you to clear all memory stored in the debugging system if you only want to record allocations after a specific point in your code*/
extern boolean f_debug_memory(); /*f_debug_memory checks if any of the bounds of any allocation has been over written and reports where to standard out. The function returns TRUE if any error was found*/

#define calloc(n, y) f_debug_mem_calloc(n, y, __FILE__, __LINE__) /* Replaces malloc. */
#define malloc(n) f_debug_mem_malloc(n, __FILE__, __LINE__) /* Replaces malloc. */
#define realloc(n, m) f_debug_mem_realloc(n, m, __FILE__, __LINE__) /* Replaces realloc. */
#define free(n) f_debug_mem_free(n) /* Replaces free. */

#endif

#ifdef F_EXIT_CRASH

extern void exit_crash(uint i); /* finction guaranteed to crash (Writes to NULL).*/
#define exit(n) exit_crash(n) /* over writhing exit(0)  with afunction guaraneed tyo crash. */

#endif

/*------- Random number generators -------
These are very fast psevdo random number generators useful for graphics applications. NOT cryptographically safe in any way.
*/


extern float f_randf(uint32 index); /* ranged 0 to 1*/
extern double f_randd(uint32 index); /* ranged 0 to 1*/
extern float f_randnf(uint32 index); /* ranged -1 to 1*/
extern double f_randnd(uint32 index); /* ranged -1 to 1*/
extern uint f_randi(uint32 index); /* integer version */

/*------- Vector math -------
Therse are common functions used in vector math */

#define d_sqrt sqrt /* replaced sqrt with carmacks  inverse sqrt aproximation */

extern float f_length2f(float *vec); /* Computes the length of a vector 2D for 32 bit floats.*/
extern float f_length3f(float *vec); /* Computes the length of a vector 3D for 32 bit floats.*/
extern float f_distance2f(float *a, float *b); /* Computes the distance between two points in 2D for 32 bit floats.*/
extern float f_distance3f(float *a, float *b); /* Computes the distance between two points in 3D for 32 bit floats.*/
extern float f_dot2f(float *a, float *b); /* Computes the dot product between two points in 2D for 32 bit floats.*/
extern float f_dot3f(float *a, float *b); /* Computes the dot product between two points in 3D for 32 bit floats.*/
extern void f_cross2f(float *output, float *a, float *b); /* Computes cross product between two points in 2D for 32 bit floats.*/
extern void f_cross3f(float *output, float *a, float *b); /* Computes cross product between two points in 3D for 32 bit floats.*/
extern void f_normalize2f(float *vec); /* Normalizes a 2D vector of 32 bit floats.*/
extern void f_normalize3f(float *vec); /* Normalizes a 3D vector of 32 bit floats.*/
extern void f_normalize4f(float *vec); /* Normalizes a 4D vector of 32 bit floats (useful for quaternions).*/
extern void f_reflect2f(float *output, float *pos, float *normal); /* Reflects a position to a normal plane in 2D for 32 bit floats.*/
extern void f_reflect3f(float *output, float *pos, float *normal); /* Reflects a position to a normal plane in 3D for 32 bit floats.*/
extern void f_flatten2f(float *output, float *pos, float *normal); /* Flattens a position to a normal plane in 2D for 32 bit floats.*/
extern void f_flatten3f(float *output, float *pos, float *normal); /* Flattens a position to a normal plane in 3D for 32 bit floats.*/
extern void f_project2f(float *output, float *plane_pos, float *normal, float *pos, float *vector); /* projects from aposition along a vector on to a positioned planein 2D for 32 bit floats.*/
extern void f_project3f(float *output, float *plane_pos, float *normal, float *pos, float *vector); /* projects fr0m aposition along a vector on to a positioned planein 3D for 32 bit floats.*/
extern void f_intersect2f(float *output, float *line_a0, float *line_a1, float *line_b0, float *line_b1); /* Computes the intersection between two lines in 2D for 32 bit floats.*/

extern double f_length2d(double *vec); /* Computes the length of a vector 2D for 64 bit doubles.*/
extern double f_length3d(double *vec); /* Computes the length of a vector 3D for 64 bit doubles.*/
extern double f_distance2d(double *a, double *b); /* Computes the distance between two points in 2D for 64 bit doubles.*/
extern double f_distance3d(double *a, double *b); /* Computes the distance between two points in 3D for 64 bit doubles.*/
extern double f_dot2d(double *a, double *b); /* Computes the dot product between two points in 2D for 64 bit doubles.*/
extern double f_dot3d(double *a, double *b); /* Computes the dot product between two points in 3D for 64 bit doubles.*/
extern void f_cross2d(double *output, double *a, double *b); /* Computes cross product between two points in 2D for 64 bit doubles.*/
extern void f_cross3d(double *output, double *a, double *b); /* Computes cross product between two points in 3D for 64 bit doubles.*/
extern void f_normalize2d(double *vec); /* Normalizes a 2D vector of 64 bit doubles.*/
extern void f_normalize3d(double *vec); /* Normalizes a 3D vector of 64 bit doubles.*/
extern void f_normalize4d(double *vec); /* Normalizes a 4D vector of 64 bit doubles (useful for quaternions).*/
extern void f_reflect2d(double *output, double *pos, double *normal); /* Reflects a position to a normal plane in 2D for 64 bit doubles.*/
extern void f_reflect3d(double *output, double *pos, double *normal); /* Reflects a position to a normal plane in 3D for 64 bit doubles.*/
extern void f_flatten2d(double *output, double *pos, double *normal); /* Flattens a position to a normal plane in 2D for 64 bit doubles.*/
extern void f_flatten3d(double *output, double *pos, double *normal); /* Flattens a position to a normal plane in 3D for 64 bit doubles.*/
extern void f_project2d(double *output, double *plane_pos, double *normal, double *pos, double *vector); /* projects from aposition along a vector on to a positioned planein 2D for 64 bit doubles.*/
extern void f_project3d(double *output, double *plane_pos, double *normal, double *pos, double *vector); /* projects fr0m aposition along a vector on to a positioned planein 3D for 64 bit doubles.*/
extern void f_intersect2d(double *output, double *line_a0, double *line_a1, double *line_b0, double *line_b1); /* Computes the intersection between two lines in 2D for 64 bit doubles.*/

/*------- Integer vector math -------------

Vector math for integer types. */

extern long f_sqrti(long value); /* Integer square root.*/
extern boolean f_normalize_2di(int *point, int fixed_point_multiplyer); /* Normalizes a 2D vector of integers. The fixed_point_multiplyer is used to set what is considerd to be one. */
extern boolean f_normalize_3di(int *point, int fixed_point_multiplyer); /* Normalizes a 2D vector of integers. The fixed_point_multiplyer is used to set what is considerd to be one. */
extern void f_intersect2di(int *output, int *line_a0, int *line_a1, int *line_b0, int *line_b1); /* Inter sects two 2d integer lines. */

/*------- Matrix operations ------------------------
Matrix operations for 4x4 matrices.*/

extern void f_matrix_clearf(float *matrix); /* Clears a 4x4 32 bit float matrix to an identity matrix.*/
extern void f_matrix_cleard(double *matrix); /* Clears a 4x4 64 bit double matrix to an identity matrix.*/ 
extern void f_transform3f(float *output, const float *matrix, const float x, const float y, const float z); /* Transforms a 3D point with a 4x4 32 bit float matrix.*/
extern void f_transform3d(double *out, const double *matrix, const double x, const double y, const double z);  /* Transforms a 3D point with a 4x4 64 bit double matrix.*/
extern void f_transform4f(float *output, const float *matrix, const float x, const float y, const float z, const double w); /* Transforms a 4D point with a 4x4 32 bit float matrix.*/
extern void f_transform4d(double *out, const double *matrix, const double x, const double y, const double z, const double w); /* Transforms a 4D point with a 4x4 64 bit double matrix.*/
extern void f_matrix_multiplyf(float *output, const float *a, const float *b); /* Multiplies two 4x4 32 bit float matrices.*/
extern void f_matrix_multiplyd(double *output, const double *a, const double *b); /* Multiplies two 4x4 64 bit double matrices.*/
extern void f_matrix_reverse4f(float *output, const float *matrix); /* Reverces a 4x4 32 bit float matrix.*/
extern void f_matrix_reverse4d(double *output, const double *matrix); /* Reverces a 4x4 64 bit double matrix.*/
extern void f_quaternion_to_matrixf(float *matrix, float x, float y, float z, float w); /* Converts a 32 bit float quaternoion to a 4x4 32 bit float matrix.*/
extern void f_quaternion_to_matrixd(double *matrix, double x, double y, double z, double w); /* Converts a 64 bit double quaternoion to a 4x4 64 bit double matrix.*/
extern void f_matrix_to_quaternionf(float *quaternion, float *matrix); /* Converts a 4x4 32 bit float matrix to a 32 bit float quaternoion.*/
extern void f_matrix_to_quaterniond(double *quaternion, double *matrix); /* Converts a 4x4 64 bit double matrix to a 64 bit double quaternoion.*/
extern void f_matrix_to_pos_quaternion_scalef(float *matrix, float *pos, float *quaternion, float *scale); /* Converts a 4x4 32 bit float matrix to a 32 bit float position, scale and quaternoion.*/
extern void f_matrix_to_pos_quaternion_scaled(double *matrix, double *pos, double *quaternion, double *scale); /* Converts a 4x4 64 bit double matrix to a 64 bit double position, scale and quaternoion.*/
extern void f_pos_quaternion_scale_to_matrixf(float *pos, float *quaternion, float *scale, float *matrix); /* Converts a 32 bit float position, scale and quaternoion to a 4x4 32 bit float matrix.*/
extern void f_pos_quaternion_scale_to_matrixd(double *pos, double *quaternion, double *scale, double *matrix); /* Converts a 64 bit double position, scale and quaternoion to a 4x4 64 bit double matrix.*/

/*------- Matrix Creation ------------------------
These functions lets you create a matrix from two points and an optional origo (The origo can be left as NULL). The first vector dominates and the second will be used to determain rotation arround trhe first vecrtor*/


extern void f_matrixxyf(float *matrix, const float *origo, const float *point_a, const float *point_b); /* Lets you create a 32 bit float 4x4 matrix using the X and Y vector */
extern void f_matrixxzf(float *matrix, const float *origo, const float *point_a, const float *point_b); /* Lets you create a 32 bit float 4x4 matrix using the X and Z vector */
extern void f_matrixyxf(float *matrix, const float *origo, const float *point_a, const float *point_b); /* Lets you create a 32 bit float 4x4 matrix using the Y and X vector */
extern void f_matrixyzf(float *matrix, const float *origo, const float *point_a, const float *point_b); /* Lets you create a 32 bit float 4x4 matrix using the Y and Z vector */
extern void f_matrixzxf(float *matrix, const float *origo, const float *point_a, const float *point_b); /* Lets you create a 32 bit float 4x4 matrix using the Z and X vector */
extern void f_matrixzyf(float *matrix, const float *origo, const float *point_a, const float *point_b); /* Lets you create a 32 bit float 4x4 matrix using the Z and Y vector */
extern void f_matrixxyd(double *matrix, const double *origo, const double *point_a, const double *point_b); /* Lets you create a 64 bit double 4x4 matrix using the X and Y vector */
extern void f_matrixxzd(double *matrix, const double *origo, const double *point_a, const double *point_b); /* Lets you create a 64 bit double 4x4 matrix using the X and Z vector */
extern void f_matrixyxd(double *matrix, const double *origo, const double *point_a, const double *point_b); /* Lets you create a 64 bit double 4x4 matrix using the Y and X vector */
extern void f_matrixyzd(double *matrix, const double *origo, const double *point_a, const double *point_b); /* Lets you create a 64 bit double 4x4 matrix using the Y and Z vector */
extern void f_matrixzxd(double *matrix, const double *origo, const double *point_a, const double *point_b); /* Lets you create a 64 bit double 4x4 matrix using the Z and X vector */
extern void f_matrixzyd(double *matrix, const double *origo, const double *point_a, const double *point_b); /* Lets you create a 64 bit double 4x4 matrix using the Z and Y vector */

/* ---- Splines ---- 
Multi dimentional bicubic spline segments.*/


extern float f_splinef(float f, float v0, float v1, float v2, float v3); /* 1D 32 bit float spline. */
extern void f_spline2df(float *out, float f, float *v0, float *v1, float *v2, float *v3); /* 2D 32 bit float spline. */
extern void f_spline3df(float *out, float f, float *v0, float *v1, float *v2, float *v3); /* 3D 32 bit float spline. */
extern double f_splined(double f, double v0, double v1, double v2, double v3); /* 1D 64 bit double spline. */
extern void f_spline2dd(double *out, double f, double *v0, double *v1, double *v2, double *v3); /* 2D 64 bit double spline. */
extern void f_spline3dd(double *out, double f, double *v0, double *v1, double *v2, double *v3); /* 3D 64 bit double spline. */

/* ---- Wiggle ---- 
Wiggle is a useful animation function that creates a value that over f moves in a psevdo random way. The motion can be compared to a fly cirkling a lightsource */

extern float f_wigglef(float f, float size); /* 1D 32 bit float wiggle*/
extern void f_wiggle2df(float *out, float f, float size); /* 2D 32 bit float wiggle*/
extern void f_wiggle3df(float *out, float f, float size); /* 3D 32 bit float wiggle*/
extern double f_wiggled(double f, double size); /* 1D 64 bit double wiggle*/
extern void f_wiggle2dd(double *out, double f, double size); /* 2D 64 bit double wiggle*/
extern void f_wiggle3dd(double *out, double f, double size); /* 3D 64 bit double wiggle*/

/* ---- Smooth step ---- 
A fast smooth step funtion that eases in as it closes on on zero and one.*/

extern float f_smooth_stepf(float f); /* 32 bit float smoothstep. */
extern double f_smooth_stepd(double f); /* 64 bit double smoothstep. */

/* ------ Perlin noise --------
Single and multi dimentional implementations of Perlin noise. Very useful for procedural data generation */

extern float f_noisef(float f); /* Single octave 1D 32 bit float noise.*/
extern float f_noise2f(float x, float y); /* Single octave 2D 32 bit float noise.*/
extern float f_noise3f(float x, float y, float z); /* Single octave 3D 32 bit float noise.*/
extern float f_noiserf(float f, uint recursions); /* Recursive 1D 32 bit float noise.*/
extern float f_noiser2f(float x, float y, uint recursions); /* Recursive 2D 32 bit float noise.*/
extern float f_noiser3f(float x, float y, float z, uint recursions); /* Recursive 3D 32 bit float noise.*/
extern double f_noised(double f); /* Single octave 1D 64 bit double noise.*/
extern double f_noise2d(double x, double y); /* Single octave 2D 64 bit double noise.*/
extern double f_noise3d(double x, double y, double z); /* Single octave 3D 64 bit double noise.*/
extern double f_noiserd(double f, uint recursions); /* Recursive 1D 64 bit double noise.*/
extern double f_noiser2d(double x, double y, uint recursions); /* Recursive 2D 64 bit double noise.*/
extern double f_noiser3d(double x, double y, double z, uint recursions); /* Recursive 3D 64 bit double noise.*/

/* ------ Snap --------
Snaps a floating point to the nearest step */

extern float f_snapf(float f, float step_size); /* 32 bit float step function. */
extern double f_snapd(double f, double step_size); /* 64 bit double step function. */

/* ---- Color space -------------- 
These functions lets you convert colors between useful color spaces like RGB (For rendering), HSV (For user interfaces), XYZ  (for a luminance corrected RGB) and LAB (For color correction). */

extern void f_rgb_to_hsv(float *hsv, float r, float g, float b); /* Converts from RBG to HSV.*/
extern void f_hsv_to_rgb(float *rgb, float h, float s, float v); /* Converts from HSV to RGB.*/

extern void f_rgb_to_xyz(float *xyz, float r, float g, float b); /* Converts from RBG to XYZ.*/
extern void f_xyz_to_rgb(float *rgb, float x, float y, float z); /* Converts from XYZ to RGB.*/

extern void f_xyz_to_lab(float *lab, float x, float y, float z); /* Converts from XYZ to LAB.*/
extern void f_lab_to_xyz(float *xyz, float l, float a, float b); /* Converts from LAB to XYZ.*/

extern void f_rgb_to_lab(float *lab, float r, float g, float b); /* Converts from RBG to LAB.*/
extern void f_lab_to_rgb(float *rgb, float l, float a, float b); /* Converts from LAB to RGB.*/

/* ---- Triangle intersection -------------- 
This is a very fast implementation of line to triabngle intersection. Useful for any kind of raytraceing. */

extern boolean f_raycast_trif(float orig[3], float dir[3], float vert0[3], float vert1[3], float vert2[3], float *t, float *u, float *v); /* 32 bit float line to triabngle intersection without backface culling. */
extern boolean f_raycast_tri_cullf(float orig[3], float dir[3], float vert0[3], float vert1[3], float vert2[3], float *t, float *u, float *v); /* 32 bit float line to triabngle intersection with backface culling. */

extern boolean f_raycast_trid(double orig[3], double dir[3], double vert0[3], double vert1[3], double vert2[3], double *t, double *u, double *v);/* 64 bit double line to triabngle intersection without backface culling. */
extern boolean f_raycast_tri_culld(double orig[3], double dir[3], double vert0[3], double vert1[3], double vert2[3], double *t, double *u, double *v);/* 64 bit float line to triabngle intersection with backface culling. */

/* Text */

extern uint		f_find_next_word(char *text);
extern boolean	f_find_word_compare(char *text_a, char *text_b);
extern uint		f_text_copy(uint length, char *dest, char *source);
extern char		*f_text_copy_allocate(char *source);
extern uint		f_word_copy(uint length, char *dest, char *source);
extern uint		f_text_copy_until(uint length, char *dest, char *source, char *until);
extern boolean	f_text_filter(char *text, char *filter);

/* ---- Sorting ------ 
A fast Merge sort implementations for sorting integers and pointers */

extern uint		*f_sort_ids(uint length, uint *ids, boolean (*compate_func)(uint big, uint small, void *user), void *user);
extern void		**f_sort_pointers(uint length, void **pointers, boolean (*compate_func)(void *big, void *small, void *user), void *user);

/* ---- Poly mesh neighbour ------ 
A very fast algorithem for finding shared edges in a polygon soup. This is a algorithem i have invented that is close to linear in preformance. */


extern uint		*f_sort_quad_tri_neighbor(uint *ref, uint quad_length, uint tri_length, uint vertex_count);
extern uint		*f_sort_tri_neighbor(uint *ref, uint tri_length, uint vertex_count);

#ifdef F_DOUBLE_PRECISION

#define f_length2 f_length2d
#define f_length3 f_length3d
#define f_distance2 f_distance2d
#define f_distance3 f_distance3d
#define f_dot2 f_dot2d
#define f_dot3 f_dot3d
#define f_cross2 f_cross2d
#define f_cross3 f_cross3d
#define f_normalize2 f_normalize2d
#define f_normalize3 f_normalize3d
#define f_normalize4 f_normalize4d
#define f_reflect2 f_reflect2d
#define f_reflect3 f_reflect3d
#define f_flatten2 f_flatten2d
#define f_flatten3 f_flatten3d
#define f_project2 f_project2d
#define f_project3 f_project3d
#define f_intersect2 f_intersect2d
#define f_matrix_clear f_matrix_cleard
#define f_transform3 f_transform3d
#define f_transform4 f_transform4d
#define f_matrix_multiply f_matrix_multiplyd
#define f_matrixxy f_matrixxyd
#define f_matrixxz f_matrixxzd
#define f_matrixyx f_matrixyxd
#define f_matrixyz f_matrixyzd
#define f_matrixzx f_matrixzxd
#define f_matrixzy f_matrixzyd
#define f_spline f_splined
#define f_spline2d f_spline2dd
#define f_spline3d f_spline3dd
#define f_wiggle f_wiggled
#define f_wiggle2d f_wiggle2dd
#define f_wiggle3d f_wiggle3dd
#define f_smooth_step f_smooth_stepd
#define f_noise f_noised
#define f_noise2 f_noise2d
#define f_noise3 f_noise3d
#define f_noiser f_noiserd
#define f_noiser2 f_noiser2d
#define f_noiser3 f_noiser3d
#define f_step f_stepd

#else

#define f_length2 f_length2f
#define f_length3 f_length3f
#define f_distance2 f_distance2f
#define f_distance3 f_distance3f
#define f_dot2 f_dot2f
#define f_dot3 f_dot3f
#define f_cross2 f_cross2f
#define f_cross3 f_cross3f
#define f_normalize2 f_normalize2f
#define f_normalize3 f_normalize3f
#define f_normalize4 f_normalize4f
#define f_reflect2 f_reflect2f
#define f_reflect3 f_reflect3f
#define f_flatten2 f_flatten2f
#define f_flatten3 f_flatten3f
#define f_project2 f_project2f
#define f_project3 f_project3f
#define f_intersect2 f_intersect2f
#define f_matrix_clear f_matrix_clearf
#define f_transform3 f_transform3f
#define f_transform4 f_transform4f
#define f_matrix_multiply f_matrix_multiplyf
#define f_matrixxy f_matrixxyf
#define f_matrixxz f_matrixxzf
#define f_matrixyx f_matrixyxf
#define f_matrixyz f_matrixyzf
#define f_matrixzx f_matrixzxf
#define f_matrixzy f_matrixzyf
#define f_spline f_splinef
#define f_spline2d f_spline2df
#define f_spline3d f_spline3df
#define f_wiggle f_wigglef
#define f_wiggle2d f_wiggle2df
#define f_wiggle3d f_wiggle3df
#define f_smooth_step f_smooth_stepf
#define f_noise f_noisef
#define f_noise2 f_noise2f
#define f_noise3 f_noise3f
#define f_noiser f_noiserf
#define f_noiser2 f_noiser2f
#define f_noiser3 f_noiser3f
#define f_step f_stepf 

#endif

#endif
